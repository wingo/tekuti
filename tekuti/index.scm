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
;; Indexing of the persistent data store.
;;
;;; Code:

(define-module (tekuti index)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (system repl error-handling)
  #:use-module (tekuti util)
  #:use-module (tekuti git)
  #:use-module (tekuti post)
  #:use-module (tekuti tags)
  #:use-module (tekuti cache)
  #:use-module (tekuti classifier)
  #:export (maybe-reindex read-index update-index))

;; Additionally an index has an "index" field, indicating the commit
;; that it was saved in, and a "master" field, indicating the commit
;; that it indexes.
(define index-specs
  `((master #f ,write ,read)
    (posts ,reindex-posts ,write-hash ,read-hash)
    (posts-by-date ,reindex-posts-by-date ,write ,read)
    (tags ,reindex-tags ,write-hash ,read-hash)
    (legit-comments ,reindex-legit-comments ,write-hash ,read-hash)
    (bogus-comments ,reindex-bogus-comments ,write-hash ,read-hash)
    (classifier ,reindex-classifier #f #f)
    (cache ,(lambda _ (make-empty-cache)) #f #f)))

(define (reindex oldindex master)
  ;; Leave off "index" field.
  (with-time-debugging
   (fold (lambda (spec index)
           (match spec
             ((key reindex write read)
              (acons key (with-time-debugging (begin (pk key) (reindex oldindex index))) index))))
         (acons 'master master '())
         ;; Skip past "master" as we handle that one specially.
         (match index-specs
           ((('master . _) . specs) specs)))))

(define (write-index index oldref)
  (let ((new (git-commit-tree
              (git-mktree
               (let lp ((index index))
                 (match index
                   (() '())
                   (((k . v) . index)
                    (match (assq k index-specs)
                      ((_ reindex write read)
                       (if write
                           (cons (list k (with-output-to-blob (write v)) 'blob)
                                 (lp index))
                           (lp index)))
                      (_ (lp index)))))))
              oldref "reindex\n"
              (commit-utc-timestamp (assq-ref index 'master)))))
    (or (false-if-git-error
         (git "update-ref" "refs/heads/index" new (or oldref "")))
        (warn "could not update indexes ref"))
    new))

(define (read-index)
  (pk 'reading-index)
  (match (false-if-git-error (git-rev-parse "refs/heads/index"))
    (#f (maybe-reindex '()))
    (ref
     (let ((dents (git-ls-tree ref #f)))
       (fold (lambda (spec index)
               (match spec
                 ((key reindex write read)
                  (pk 'read-index-key key)
                  (acons key
                         (cond
                          ((and read (assoc (symbol->string key) dents))
                           => (match-lambda
                                ((_ sha1 'blob)
                                 (with-input-from-blob sha1 (read)))))
                          (else
                           (reindex '() index)))
                         index))))
             `((index . ,ref))
             index-specs)))))

(define (maybe-reindex old-index)
  (let ((master (git-rev-parse "refs/heads/master")))
    (if (equal? (assq-ref old-index 'master) master)
        old-index
        (call-with-error-handling
         (lambda ()
           (let ((new-index (reindex old-index master)))
             (acons
              'index (write-index new-index (assq-ref old-index 'index))
              new-index)))
         #:on-error 'backtrace
         #:post-error (lambda _ old-index)))))

(define (update-index index key update)
  (cond
   ((null? index) (acons key (update '()) '()))
   ((eq? (caar index) key) (acons key (update index) (cdr index)))
   (else (cons (car index) (update-index (cdr index) key update)))))
