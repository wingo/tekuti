;; Tekuti
;; Copyright (C) 2008, 2010, 2012 Andy Wingo <wingo at pobox dot com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:
;;
;; Comments -- pulling them out of the database, and making new ones.
;;
;;; Code:

(define-module (tekuti classifier)
  #:use-module (tekuti comment)
  #:use-module (tekuti filters)
  #:use-module (tekuti git)
  #:use-module (tekuti util)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-9)
  #:export (reindex-legit-comments
            reindex-bogus-comments
            reindex-classifier
            comment-is-bogus?))

(define (tokenize-comment comment)
  (define (decorate-tokens decorator tokens)
    (map (lambda (token) (string-append decorator token)) tokens))
  (define (tokenize exp)
    (match exp
      ((or () #f) '())
      ((? string?) (string-tokenize exp char-set:letter+digit))
      (((or 'comment 'div 'p) . body)
       (tokenize body))
      (((? symbol? tag) . body)
       (decorate-tokens (string-append (symbol->string tag) ".")
                        (tokenize body)))
      ((head . tail) (append (tokenize head) (tokenize tail)))
      (_ (pk 'what exp comment) '())))
  (tokenize
   `(comment
     (author ,(assq-ref comment 'author))
     (email ,(assq-ref comment 'author_email))
     (url ,(assq-ref comment 'author_url))
     ,(let ((format (or (assq-ref comment 'format) 'wordpress))
            (raw (assq-ref comment 'raw-content)))
        (or (case format
              ((wordpress) (false-if-exception (wordpress->sxml raw)))

              (else `(pre ,raw)))
            `(pre ,raw))))))

(define (fold-features comment f seed)
  (let lp ((tokens (tokenize-comment comment)) (seed seed))
    (define (make-3-gram a b c)
      (string-append a " " (or b "") " " (or c "")))
    (define (make-2-gram a b)
      (string-append a " " (or b "")))
    (define (make-1-gram a)
      a)
    (match tokens
      ((a . tokens)
       (lp tokens
           (or (f (match tokens
                    ((b c . _) (make-3-gram a b c))
                    ((b) (make-3-gram a b #f))
                    (() (make-3-gram a #f #f)))
                  (or (f (match tokens
                           ((b . _) (make-2-gram a b))
                           (() (make-2-gram a #f)))
                         (f (make-1-gram a)
                            seed))
                      seed))
               (f (make-1-gram a)
                  seed))))
      (() seed))))

(define (count-features comments)
  (let ((counts (make-hash-table)))
    (hash-for-each
     (lambda (comment-sha1 comment-name)
       (fold-features (blob->comment comment-name comment-sha1)
                      (lambda (feature counts)
                        (add-feature! counts feature)
                        counts)
                      counts))
     comments)
    counts))

;; A feature's bogosity is the probability that a bogus document
;; contains that feature, divided by the probability that a legit
;; document contains the feature.
(define (compute-log-bogosities legit-features bogus-features)
  (define (feature-count table)
    (hash-fold (lambda (feature count sum) (+ count sum)) 0 table))
  (let ((total-bogus-features (feature-count bogus-features))
        (total-legit-features (feature-count legit-features))
        (log-bogosities (make-hash-table)))
    (hash-for-each
     (lambda (feature bogus-count)
       (let ((legit-count (hash-ref legit-features feature 0)))
         (hash-set! log-bogosities feature
                    (if (and (> total-bogus-features 0)
                             (> total-legit-features 0))
                        (log (/ (/ (+ bogus-count 0.001) total-bogus-features)
                                (/ (+ legit-count 0.001) total-legit-features)))
                        0))))
     bogus-features)
    (hash-for-each
     (lambda (feature legit-count)
       (let ((bogus-count (hash-ref bogus-features feature)))
         (unless bogus-count
           (hash-set! log-bogosities feature
                      (if (and (> total-bogus-features 0)
                               (> total-legit-features 0))
                          (log (/ (/ 0.01 total-bogus-features)
                                  (/ (+ legit-count 0.01) total-legit-features)))
                          0)))))
     legit-features)
    log-bogosities))

(define (update-bogosities! log-bogosities changed-features
                            legit-features bogus-features)
  (define (feature-count table)
    (hash-fold (lambda (feature count sum) (+ count sum)) 0 table))
  (let ((total-bogus-features (feature-count bogus-features))
        (total-legit-features (feature-count legit-features)))
    (hash-for-each
     (lambda (feature _)
       (let ((bogus-count (hash-ref bogus-features feature 0))
             (legit-count (hash-ref legit-features feature 0)))
         (hash-set! log-bogosities feature
                    (if (and  (> total-bogus-features 0)
                              (> total-legit-features 0))
                        (log (/ (/ (+ bogus-count 0.001) total-bogus-features)
                                (/ (+ legit-count 0.001) total-legit-features)))
                        0))))
     changed-features)))

(define (compute-bogus-probability comment log-bogosities bogus-prior
                                   feature-limit)
  (let ((v (make-vector feature-limit 0.0)))
    (define (add-bogosity! log-bogosity)
      (let ((mag (abs log-bogosity)))
        (when (< (abs (vector-ref v 0)) mag)
          (let lp ((idx 0))
            (let ((next (1+ idx)))
              (cond
               ((and (< next (vector-length v))
                     (< (abs (vector-ref v next)) mag))
                (vector-set! v idx (vector-ref v next))
                (lp (1+ idx)))
               (else
                (vector-set! v idx log-bogosity))))))))
    (fold-features comment
                   (lambda (feature _)
                     (add-bogosity! (hash-ref log-bogosities feature 0.0)))
                   #f)
    (let* ((ratio (exp (+ (log (/ bogus-prior (- 1.0 bogus-prior)))
                          (apply + (vector->list v))))))
      (/ ratio (+ ratio 1.0)))))

(define (compute-differing-comments old-master new-master)
  ;; sha1 -> name
  (define (compute-hash-diff old new)
    (let ((removed (make-hash-table))
          (added (make-hash-table)))
      (hash-for-each (lambda (post-sha1 post-name)
                       (unless (hash-ref new post-sha1)
                         (hash-set! removed post-sha1 post-name)))
                     old)
      (hash-for-each (lambda (post-sha1 post-name)
                       (unless (hash-ref old post-sha1)
                         (hash-set! added post-sha1 post-name)))
                     new)
      (values removed added)))
  (define* (git-ls-tree->hash ref kind #:optional (out (make-hash-table)))
    (for-each
     (match-lambda
       ((name sha1 (? (lambda (x) (eq? x kind))))
        (hash-set! out sha1 name)))
     (git-ls-tree ref #f))
    out)
  (let ((old-post-trees (git-ls-tree->hash old-master 'tree))
        (new-post-trees (git-ls-tree->hash new-master 'tree)))
    (let-values (((removed-post-trees added-post-trees)
                  (compute-hash-diff old-post-trees new-post-trees)))
      (let ((old-comments (make-hash-table))
            (new-comments (make-hash-table)))
        (hash-for-each
         (lambda (sha1 name)
           (git-ls-tree->hash (string-append sha1 ":comments") 'blob
                              old-comments))
         removed-post-trees)
        (hash-for-each
         (lambda (sha1 name)
           (git-ls-tree->hash (string-append sha1 ":comments") 'blob
                              new-comments))
         added-post-trees)
        (compute-hash-diff old-comments new-comments)))))

(define (reindex-legit-comments old-index index)
  (let ((old-legit-comments (assq-ref old-index 'legit-comments))
        (old-master (assq-ref old-index 'master))
        (new-master (assq-ref index 'master)))
    (if old-legit-comments
        (let-values (((removed added)
                      (compute-differing-comments old-master new-master)))
          (hash-for-each (lambda (k v)
                           (hash-remove! old-legit-comments k))
                         removed)
          (hash-for-each (lambda (k v)
                           (hash-set! old-legit-comments k v))
                         added)
          old-legit-comments)
        (compute-legit-comments new-master))))

(define (reindex-bogus-comments old-index index)
  (let ((old-bogus-comments (assq-ref old-index 'bogus-comments))
        (old-classifier (assq-ref old-index 'classifier))
        (old-master (assq-ref old-index 'master))
        (new-master (assq-ref index 'master)))
    (if old-bogus-comments
        (let-values (((removed added)
                      (compute-differing-comments old-master new-master)))
          (hash-for-each (lambda (k v)
                           (hash-remove! old-bogus-comments k))
                         added)
          (hash-for-each (lambda (k v)
                           (hash-set! old-bogus-comments k v))
                         removed)
          old-bogus-comments)
        (compute-bogus-comments new-master (assq-ref index 'legit-comments)))))

(define-record-type <classifier>
  (make-classifier legit-prior legit-features bogus-features bogosities)
  classifier?
  (legit-prior classifier-legit-prior)
  (legit-features classifier-legit-features)
  (bogus-features classifier-bogus-features)
  (bogosities classifier-bogosities))

(define (rebuild-classifier legit-comments bogus-comments)
  (format #t "Rebuilding bogus comment classifier...\n")
  (with-time-debugging
   (let* ((legit-count (hash-count (const #t) legit-comments))
          (bogus-count (hash-count (const #t) bogus-comments))
          (legit-prior (if (> legit-count 0)
                           (/ legit-count (+ legit-count bogus-count 0.0))
                           0))
          (legit-features (count-features legit-comments))
          (bogus-features (count-features bogus-comments))
          (bogosities (compute-log-bogosities legit-features bogus-features)))
     (make-classifier legit-prior legit-features bogus-features bogosities))))

(define* (add-feature! features feature #:optional (count 1))
  (let ((h (hash-create-handle! features feature 0)))
    (set-cdr! h (+ (cdr h) count))))

(define* (remove-feature! features feature #:optional (count 1))
  (let ((h (hash-get-handle features feature)))
    (when h
      (set-cdr! h (- (cdr h) count)))))

(define (reindex-classifier old-index index)
  (match (assq-ref old-index 'classifier)
    (#f
     (rebuild-classifier (assq-ref index 'legit-comments)
                         (assq-ref index 'bogus-comments)))
    ((and classifier
          ($ <classifier> legit-prior legit-features bogus-features bogosities))
     (let-values (((removed added)
                   (compute-differing-comments (assq-ref old-index 'master)
                                               (assq-ref index 'master))))
       (let ((removed-features (count-features removed))
             (added-features (count-features added)))
         (hash-for-each (lambda (k v)
                          (remove-feature! legit-features k v)
                          (add-feature! bogus-features k v))
                        removed-features)
         (hash-for-each (lambda (k v)
                          (add-feature! legit-features k v)
                          (remove-feature! bogus-features k v))
                        added-features)
         (update-bogosities! bogosities removed-features
                             legit-features bogus-features)
         (update-bogosities! bogosities added-features
                             legit-features bogus-features)))
     classifier)))

(define* (comment-bogus-probability classifier comment #:key (feature-limit 20))
  (let* ((log-bogosities (classifier-bogosities classifier))
         (bogus-prior (- 1.0 (classifier-legit-prior classifier))))
    (compute-bogus-probability comment log-bogosities
                               bogus-prior feature-limit)))

(define* (comment-is-bogus? index comment #:key (threshold 0.5))
  (let ((classifier (assq-ref index 'classifier)))
    (> (comment-bogus-probability classifier comment) threshold)))
