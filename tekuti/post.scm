;; Tekuti
;; Copyright (C) 2008, 2010, 2011, 2012, 2014, 2021 Andy Wingo <wingo at pobox dot com>

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
;; Posts -- pulling them out of git, and, later, putting them in.
;;
;;; Code:

(define-module (tekuti post)
  #:use-module (srfi srfi-1)
  #:use-module (web uri)
  #:use-module (tekuti match-bind)
  #:use-module (tekuti util)
  #:use-module (tekuti comment)
  #:use-module (tekuti config)
  #:use-module (tekuti git)
  #:use-module (tekuti filters)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:export (post-from-key

            post-tags post-timestamp post-key
            post-public? post-draft? post-private?
            post-comments-open? post-comments-closed-timestamp
            post-comments
            post-sxml-content post-readable-date post-n-comments
            post-raw-content
            post-title

            make-new-post modify-post delete-post

            latest-posts

            reindex-posts reindex-posts-by-date))

;;;
;;; pulling posts out of git
;;;

(define *post-spec*
  `((timestamp . ,string->number)
    (tags . ,(lambda (v) (string-split/trimming v #\,)))
    (title . ,identity)
    (comments-closed-timestamp . ,(lambda (str)
                                    (if (string-null? str)
                                        #f
                                        (string->number str))))))

(define (post-from-tree encoded-name sha1)
  (append `((key . ,encoded-name)
            (sha1 . ,sha1))
          (match-lines
           (git "show" (string-append sha1 ":metadata"))
           "^([^: ]+): +(.*)$" (_ k v)
           (let* ((k (string->symbol k))
                  (parse (or (assq-ref *post-spec* k)
                             identity)))
             (cons k (parse v))))))

(define (post-from-git master key)
  (false-if-git-error
   (let ((pairs (git-ls-subdirs master key)))
     (and (= (length pairs) 1)
          (post-from-tree key (cdar pairs))))))

;;;
;;; pulling posts out of the index
;;;

(define* (post-from-key index key #:key allow-unpublished? allow-draft?)
  (let ((post (hash-ref (assq-ref index 'posts) key)))
    (if (and post (or (post-public? post)
                      (and (post-draft? post) allow-draft?)
                      allow-unpublished?))
        post
        #f)))

;;;
;;; accessors
;;;

(define (post-public? post-alist)
  (equal? (assq-ref post-alist 'status) "publish"))

(define (post-draft? post-alist)
  (equal? (assq-ref post-alist 'status) "draft"))

(define (post-private? post-alist)
  (equal? (assq-ref post-alist 'status) "private"))

(define (post-timestamp post-alist)
  (assq-ref post-alist 'timestamp))

(define (post-tags post-alist)
  (or (assq-ref post-alist 'tags) '()))

(define (post-key post)
  (assq-ref post 'key))

(define (post-title post)
  (assq-ref post 'title))

(define (post-comments-open? post)
  (and (equal? (assq-ref post 'comment_status) "open")
       (cond
        ((post-comments-closed-timestamp post)
         => (lambda (at-timestamp)
              (< (time-second (current-time)) at-timestamp)))
        (else #t))))

(define (post-comments-closed-timestamp post)
  (assq-ref post 'comments-closed-timestamp))

(define (post-raw-content post)
  (git "show" (string-append (assq-ref post 'sha1) ":content")))

(define (post-sxml-content post)
  (let ((format (or (assq-ref post 'format) 'wordpress))
        (raw (post-raw-content post)))
    (catch #t
           (lambda ()
             (case format
               ((wordpress) (wordpress->sxml raw))
               (else `(pre ,raw))))
           (lambda args
             `(pre ,(bad-user-submitted-xhtml? raw))))))

(define (post-readable-date post)
  (let ((date (time-utc->date
               (make-time time-utc 0 (post-timestamp post)))))
    (date->string date "~e ~B ~Y ~l:~M ~p")))

(define (post-comments post)
  (dsu-sort
   (map (lambda (pair)
          (blob->comment (car pair) (cadr pair)))
        (git-ls-tree (string-append (assq-ref post 'sha1) ":comments") #f))
   comment-timestamp
   <))

(define (post-n-comments post)
  (length (git-ls-tree (string-append (assq-ref post 'sha1) ":comments") #f)))

(define (munge-post old-key parsed)
  (let ((metadata (with-output-to-blob
                   (for-each
                    (lambda (k)
                      (cond
                       ((assq-ref parsed k)
                        => (lambda (v) (format #t "~a: ~a\n" k v)))))
                    '(timestamp tags status title name comment_status
                                comments-closed-timestamp))))
        (content (with-output-to-blob (display (assq-ref parsed 'body))))
        (key (assq-ref parsed 'key))
        (message (format #f "~a: \"~a\""
                         (if old-key "post modified" "new post")
                         (assq-ref parsed 'title))))
    (define (maybe-rename ops)
      (if (and old-key (not (equal? old-key key)))
          (cons `(rename () (,old-key ,key)) ops)
          ops))
    (define (maybe-clear ops)
      (if old-key
          (append `((delete (,key) ("content"))
                    (delete (,key) ("metadata")))
                  ops)
          ops))
    (let ((ops (maybe-rename
                (maybe-clear
                 `((create (,key) ("metadata" ,metadata blob))
                   (create (,key) ("content" ,content blob)))))))
      (post-from-git
       (git-update-ref "refs/heads/master"
                       (lambda (master)
                         (git-commit-tree (munge-tree master ops)
                                          master message #f))
                       5)
       key))))

(define space-to-dash (s///g "[ .]" "-"))
(define remove-extraneous (s///g "[^a-z0-9-]+" ""))
(define collapse (s///g "-+" "-"))

(define (title->name title)
  (collapse (remove-extraneous (space-to-dash (string-downcase title)))))

;; some verification necessary...
(define (parse-post-data post-data)
  (let ((title (assoc-ref post-data "title"))
        (body (assoc-ref post-data "body"))
        (tags (assoc-ref post-data "tags"))
        (status (assoc-ref post-data "status"))
        (comments-open? (assoc-ref post-data "comments"))
        (date-str (assoc-ref post-data "date"))
        (comments-closed-date-str (assoc-ref post-data "comments-closed-date")))
    (let* ((timestamp (if (string-null? date-str)
                          (time-second (current-time))
                          (rfc822-date->timestamp date-str)))
           (comments-closed-timestamp
            (if (string-null? comments-closed-date-str)
                (if (post-public? (acons 'status status '()))
                    (+ *comments-open-window* timestamp)
                    #f)
                (rfc822-date->timestamp comments-closed-date-str)))
           (name (title->name title)))
      `((title . ,title)
        (body . ,body)
        (tags . ,tags)
        (status . ,status)
        (comment_status . ,(if comments-open? "open" "closed"))
        (comments-closed-timestamp . ,comments-closed-timestamp)
        (timestamp . ,timestamp)
        (name . ,name)
        (key . ,(string-downcase
                 (uri-encode
                  (string-append (date->string (timestamp->date timestamp)
                                               "~Y/~m/~d/")
                                 (uri-encode name)))))))))

(define (make-new-post post-data)
  (munge-post #f (parse-post-data post-data)))

(define (modify-post old-key post-data)
  (munge-post old-key (parse-post-data post-data)))

(define (delete-post post)
  (let ((message (format #f "~a: \"~a\"" "post deleted" (post-title post))))
    (git-update-ref "refs/heads/master"
                  (lambda (master)
                    (git-commit-tree
                     (munge-tree1 master 'delete '() `(,(post-key post)))
                     master message #f))
                  5)))

(define* (latest-posts index #:key allow-unpublished? (filter identity)
                       (limit 10))
  (filter-mapn
   (lambda (key)
     (and=> (post-from-key index key #:allow-unpublished? allow-unpublished?)
            (lambda (post) (and post (filter post) post))))
   (assq-ref index 'posts-by-date)
   limit))

(define (reindex-posts old-index index)
  (let ((old (assq-ref old-index 'posts))
        (new (make-hash-table)))
    (for-each
     (lambda (dent)
       (let* ((key (car dent))
              (sha1 (cadr dent))
              (prev (and (hash-table? old) (hash-ref old key))))
         (hash-set! new key
                    (if (and prev (equal? (assq-ref prev 'sha1) sha1))
                        prev
                        (begin
                          (pk 'updated dent)
                          (post-from-tree key sha1))))))
     (git-ls-tree (assq-ref index 'master) #f))
    new))

(define (reindex-posts-by-date old-index index)
  (map cdr
       (sort (hash-map->list (lambda (key post)
                               (cons (post-timestamp post) key))
                             (assq-ref index 'posts))
             (lambda (x y)
               (> (car x) (car y))))))
