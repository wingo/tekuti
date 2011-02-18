;; Tekuti
;; Copyright (C) 2008, 2010 Andy Wingo <wingo at pobox dot com>

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

;;hack!
(define-module (tekuti comment)
  #:use-module (tekuti git)
  #:use-module (tekuti util)
  #:use-module (tekuti filters)
  #:use-module (tekuti post)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (sxml transform)
  #:use-module (tekuti match-bind)
  #:export (blob->comment comment-sxml-content comment-timestamp
            comment-readable-date bad-new-comment-post?
            make-new-comment delete-comment))

(define *comment-spec*
  `((timestamp . ,string->number)))

(define (blob->comment encoded-name sha1)
  (let ((blob (git "show" sha1)))
    (match-bind
     "\n\n(.*)$" blob (_ content)
     (append
      `((raw-content . ,content)
        (sha1 . ,sha1)
        (key . ,encoded-name))
      (match-lines (substring blob 0 (- (string-length blob)
                                        (string-length _)))
                   "^([^: ]+): +(.*)$" (_ k v)
                   (let* ((k (string->symbol k))
                          (parse (or (assq-ref *comment-spec* k) identity)))
                     (cons k (parse v))))))))

(define (comment-readable-date comment)
  (let ((date (time-utc->date
               (make-time time-utc 0 (assq-ref comment 'timestamp)))))
    (date->string date "~e ~B ~Y ~l:~M ~p")))

(define (comment-raw-content comment)
  (assq-ref comment 'raw-content))

(define (comment-sxml-content comment)
  `(li (@ (class "alt") (id ,(assq-ref comment 'key)))
       (cite ,(let ((url (assq-ref comment 'author_url))
                    (name (assq-ref comment 'author)))
                (if (and url (not (string-null? url)))
                    `(a (@ (href ,url) (rel "external nofollow")) ,name)
                    name)))
       " says:" (br)
       (small (@ (class "commentmetadata"))
              (a (@ (href ,(string-append "#" (assq-ref comment 'key))))
                 ,(comment-readable-date comment)))
       ,(let ((format (or (assq-ref comment 'format) 'wordpress)))
          ((case format
             ((wordpress) wordpress->sxml)
             (else (lambda (text) `(pre ,text))))
           (comment-raw-content comment)))))

(define (comment-timestamp comment-alist)
  (or (assq-ref comment-alist 'timestamp) #f))

(define (bad-email? x)
  (if (emailish? x)
      #f
      `(p "Please pretend to specify a valid email address.")))

(define (bad-url? x)
  (if (or (string-null? x) (urlish? x))
      #f
      `(p "Bad URL. (Only http and https are allowed.)")))

(define (bad-number? x)
  (if (string->number x)
      #f
      '(p "Bad number. Give me something that Scheme's "
          (tt "string->number") " will like.")))

(define *new-comment-spec*
  `(("author" ,(lambda (x) #f))
    ("email" ,bad-email?)
    ("url" ,bad-url?)
    ("comment" ,bad-user-submitted-xhtml?)
    ("x" ,bad-number?)
    ("submit" ,(lambda (x) #f))))

(define (bad-new-comment-post? post-data)
  (or (or-map (lambda (pair)
                (and (not (assoc (car pair) *new-comment-spec*))
                     `(p "Bad post data: " ,(car pair))))
              post-data)
      (or-map (lambda (pair)
                (and (not (assoc (car pair) post-data))
                     `(p "Incomplete post data:" ,(car pair))))
              *new-comment-spec*)
      (or-map (lambda (pair)
                ((cadr pair) (assoc-ref post-data (car pair))))
              *new-comment-spec*)))

(define de-newline (s///g "[\n\r]" " "))

(define (make-new-comment key title post-data)
  (let ((content (assoc-ref post-data "comment"))
        (author (assoc-ref post-data "author"))
        (email (assoc-ref post-data "email"))
        (url (assoc-ref post-data "url"))) 
    (let ((sha1 (with-output-to-blob
                 (for-each
                  (lambda (pair)
                    (format #t "~a: ~a\n" (car pair) (cdr pair)))
                  `((timestamp . ,(time-second (current-time)))
                    (author . ,(de-newline author))
                    (author_email . ,email)
                    (author_url . ,url)))
                 (display "\n")
                 (display content)))
          (message (format #f "comment on \"~a\" by ~a" title author)))
      (git-update-ref
       "refs/heads/master"
       (lambda (master)
         (git-commit-tree (munge-tree1 master
                                       'create
                                       (list key "comments")
                                       (list sha1 sha1 'blob))
                          master message #f))
       5))))

(define (delete-comment post id)
  (let ((key (post-key post))
        (message (format #f "~a on \"~a\"" "comment deleted" (post-title post))))
    (git-update-ref "refs/heads/master"
                  (lambda (master)
                    (git-commit-tree (munge-tree1 master
                                                  'delete
                                                  `(,key "comments")
                                                  `(,id))
                                     master message #f))
                  5)))
