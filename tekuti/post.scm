;; Tekuti
;; Copyright (C) 2008 Andy Wingo <wingo at pobox dot com>

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
  #:use-module (match-bind)
  #:use-module (tekuti util)
  #:use-module (tekuti url)
  #:use-module (tekuti comment)
  #:use-module (tekuti config)
  #:use-module (tekuti git)
  #:use-module (tekuti filters)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:export (post-from-tree post-from-key

            post-tags post-timestamp post-key post-published?
            post-comments-open? post-comments
            post-sxml-content post-readable-date post-n-comments
            post-raw-content
            post-title

            make-new-post modify-post

            all-published-posts

            reindex-posts))

;;; 
;;; pulling posts out of git
;;; 

(define *post-spec*
  `((timestamp . ,string->number)
    (tags . ,(lambda (v) (string-split/trimming v #\,)))
    (title . ,identity)))

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

(define (post-from-key master key . allow-unpublished)
  (false-if-git-error
   (let ((pairs (git-ls-subdirs master key)))
     (and (= (length pairs) 1)
          (let ((post (post-from-tree key (cdar pairs))))
            (if (or (post-published? post)
                    (and (pair? allow-unpublished) (car allow-unpublished)))
                post
                #f))))))

;;; 
;;; accessors
;;; 

(define (post-published? post-alist)
  (equal? (assq-ref post-alist 'status) "publish"))

(define (post-timestamp post-alist)
  (assq-ref post-alist 'timestamp))

(define (post-tags post-alist)
  (or (assq-ref post-alist 'tags) '()))

(define (post-key post)
  (assq-ref post 'key))

(define (post-title post)
  (assq-ref post 'title))

(define (post-comments-open? post)
  (equal? (assq-ref post 'comment_status) "open"))

(define (post-raw-content post)
  (git "show" (string-append (assq-ref post 'sha1) ":content")))

(define (post-sxml-content post)
  (let ((format (or (assq-ref post 'format) 'wordpress)))
    ((case format
       ((wordpress) wordpress->sxml)
       (else (lambda (text) `(pre ,text))))
     (post-raw-content post))))

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
                      (format #t "~a: ~a\n" k (assq-ref parsed k)))
                    '(timestamp tags status title name comment_status))))
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
      (post-from-key
       (git-update-ref "refs/heads/master"
                       (lambda (master)
                         (git-commit-tree (munge-tree master ops)
                                          master message #f))
                       5)
       key #t))))

(define space-to-dash (s///g " " "-"))
(define remove-extraneous (s///g "[^a-z-]+" ""))
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
        (date-str (assoc-ref post-data "date")))
    (let ((timestamp (if (string-null? date-str)
                         (time-second (current-time))
                         (rfc822-date->timestamp date-str)))
          (name (title->name title)))
      `((title . ,title)
        (body . ,body)
        (tags . ,tags)
        (status . ,status)
        (comment_status . ,(if comments-open? "open" "closed"))
        (timestamp . ,timestamp)
        (name . ,name)
        (key . ,(url:encode
                 (string-append (date->string (timestamp->date timestamp)
                                              "~Y/~m/~d/")
                                (url:encode name))))))))

(define (make-new-post post-data)
  (munge-post #f (parse-post-data post-data)))

(define (modify-post old-key post-data)
  (munge-post old-key (parse-post-data post-data)))

(define (all-posts master)
  (map (lambda (pair)
         (post-from-tree (car pair) (cdr pair)))
       (git-ls-subdirs master #f)))

(define (all-published-posts master)
  (dsu-sort
   (filter post-published? (all-posts master))
   post-timestamp
   >))

(define (hash-fill proc list)
  (let ((table (make-hash-table)))
    (for-each (lambda (x) (proc x table))
              list)
    table))

(define (reindex-posts oldindex newindex)
  (let ((old (hash-fill (lambda (post h)
                          (hash-set! h (assq-ref post 'sha1) post))
                        (or (assq-ref oldindex 'posts) '()))))
    (dsu-sort (map (lambda (dent)
                     (or (hash-ref old (cadr dent))
                         (begin (pk 'updated dent)
                                (post-from-tree (car dent) (cadr dent)))))
                   (git-ls-tree (assq-ref newindex 'master) #f))
              post-timestamp
              >)))

