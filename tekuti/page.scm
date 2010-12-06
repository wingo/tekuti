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
;; This is the main script that will launch tekuti.
;;
;;; Code:

(define-module (tekuti page)
  #:use-module (tekuti config)
  #:use-module (tekuti util)
  #:use-module (tekuti git)
  #:use-module (tekuti post)
  #:use-module (tekuti comment)
  #:use-module (web uri)
  #:use-module (web request)
  #:use-module (tekuti request)
  #:use-module (tekuti page-helpers)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-19)
  #:export (page-admin
            page-admin-posts
            page-admin-post
            page-admin-new-post
            page-admin-modify-post 
            page-admin-changes 
            page-admin-change
            page-admin-revert-change
            page-index 
            page-show-post 
            page-new-comment
            page-archives 
            page-show-tags
            page-show-tag
            page-debug 
            page-search 
            page-feed-atom
            page-debug
            page-not-found))

;; Encoded twice, so as to form a single path-component.
(define (make-post-key . parts)
  (uri-encode (encode-and-join-uri-path parts)))

(define (not-implemented request . args)
  (respond `((h1 "Not yet implemented")
                  (p "Path handler not yet implemented: "
                     ,(request-relative-path-str request)))
           #:status 500))

(define (page-admin request body index)
  (with-authentication
   request
   (lambda ()
     ;; here we need to be giving a dashboard view instead of this
     (define (post-links n)
       (mapn (lambda (post)
               `(li ,(admin-post-link post)))
             (assq-ref index 'posts)
             n))
     (define (recent-changes n)
       (map (lambda (rev)
              `(li ,(rellink `("admin" "changes" ,(car rev))
                             (caddr rev))))
            (git-rev-list "refs/heads/master" n)))
     (respond `(,(sidebar-ul `((li (h2 "posts " ,(rellink '("admin" "posts")
                                                          ">>"))
                                   (ul ,@(post-links 5)))
                               (li (h2 "changes" ,(rellink '("admin" "changes")
                                                           ">>"))
                                   (ul ,(recent-changes 5)))))
                (h2 "new post")
                ,(post-editing-form #f))))))

(define (page-admin-posts request body index)
  (with-authentication
   request
   (lambda ()
     (define (post-headers)
       (map (lambda (post)
              `(h3 ,(admin-post-link post)))
            (assq-ref index 'posts)))
     (respond `((h1 "all your posts are belong to tekuti")
                ,@(post-headers))))))

(define (page-admin-post request body index key)
  (with-authentication
   request
   (lambda ()
     (let ((post (post-from-key (assq-ref index 'master) key #t)))
       (respond `((h1 ,(post-title post))
                  ,(post-editing-form post)))))))

(define (page-admin-new-post request body index)
  (with-authentication
   request
   (lambda ()
     (let ((post (make-new-post (request-form-data request body))))
       (respond `((p "redirecting..."))
                #:redirect (admin-post-url post))))))

(define (page-admin-modify-post request body index key)
  (with-authentication
   request
   (lambda ()
     (let ((post (modify-post key (request-form-data request body))))
       (respond `((p "redirecting..."))
                #:redirect (admin-post-url post))))))
     
(define (page-admin-changes request body index) 
  (with-authentication
   request
   (lambda ()
     (let ((revs (git-rev-list (request-query-ref request "start"
                                                  "refs/heads/master")
                               10)))
       (respond `((h2 "recent changes")
                  ,@(map (lambda (rev)
                           `(div (h3 ,(rellink `("admin" "changes" ,(car rev))
                                               (caddr rev)))
                                 ,(timestamp->rfc822-date (cadr rev))))
                         revs)
                  (h3 ,(rellink '("admin" "changes")
                                "more"
                                #:query
                                `(("start" . ,(caar (last-pair revs))))))))))))

(define (page-admin-change request body index sha1) 
  (with-authentication
   request
   (lambda ()
     (let ((commit (parse-commit sha1)))
       (respond `((h2 ,(assq-ref commit 'message))
                  (p "Committed on "
                     ,(timestamp->rfc822-date
                       ;; needlessly goes to git again...
                       (commit-utc-timestamp sha1)))
                  (pre ,(git "diff-tree" "-M" "-p" sha1))
                  (form (@ (action ,(relurl `("admin" "revert-change" ,sha1)))
                           (method "POST"))
                        (input (@ (type "submit") (value "Undo this change"))))))))))


(define (page-admin-revert-change request body index sha1)
  (with-authentication
   request
   (lambda ()
     (let ((new-master (git-revert "refs/heads/master" sha1)))
       (respond `((h3 "Change reverted"))
                #:redirect (relurl '("admin")))))))

(define (page-index request body index)
  (respond `(,(main-sidebar request index)
             ,@(map (lambda (post)
                      (show-post post #f))
                    (published-posts index 10)))
           #:etag (assq-ref index 'master)))

(define (page-show-post request body index year month day post)
  (cond
   ((post-from-key (assq-ref index 'master)
                   (make-post-key year month day post))
    => (lambda (post)
         (respond `(,(post-sidebar post index)
                    ,(show-post post #t))
                  #:title (string-append (post-title post) " -- " *title*)
                  #:etag (assq-ref index 'master))))
   (else
    (page-not-found request body index))))

(define (page-new-comment request body index year month day name)
  (let ((data (request-form-data request body)))
    (cond
     ((post-from-key (assq-ref index 'master)
                     (make-post-key year month day name))
      => (lambda (post)
           (cond
            ((bad-new-comment-post? data)
             => (lambda (reason)
                  (respond `((p "Bad post data: " ,(pk reason))))))
            (else
             (let ((comment (make-new-comment (post-key post) (post-title post)
                                              data)))
               ;; nb: at this point, `post' is out-of-date
               (respond `((p "Comment posted, thanks."))
                        #:redirect (post-url post #:fragment "comments")
                        #:title "comment posted"))))))
     (else
      (page-not-found request body index)))))

;; fixme exception handling for input
(define (page-archives request body index year month day)
  (let ((year (and=> year string->number))
        (month (and=> month string->number))
        (day (and=> day string->number)))
    (let ((start (make-date 0 0 0 0 (or day 1) (or month 1) (or year 1980) 0)))
      (define too-early?
        (compose1 (date-before? start) post-timestamp))
      (define early-enough?
        (if year
            (compose1 (date-before?
                       (cond (day (date-increment start #:day 1))
                             (month (date-increment start #:month 1))
                             (else (date-increment start #:year 1))))
                      post-timestamp)
            (lambda (post) #t)))
      (define (make-date-header post)
        (lambda (x) #f))
    
      (let lp ((posts (published-posts index -1)))
        (cond ((or (null? posts) (too-early? (car posts)))
               (respond `((h1 "No posts found")
                          (p "No posts were found in the specified period."))
                        #:title *title*))
              ((early-enough? (car posts))
               (let lp ((posts posts) (new-header (make-date-header #t)) (out '()))
                 (cond
                  ((or (null? posts) (too-early? (car posts)))
                   (respond (reverse out)
                            #:title (string-append "archives -- " *title*)
                            #:etag (assq-ref index 'master)))
                  ((new-header (car posts))
                   => (lambda (sxml)
                        (lp (cdr posts) (make-date-header (car posts))
                            (cons (post-link (car posts)) (append sxml out)))))
                  (else
                   (lp (cdr posts) new-header (cons `(p ,(post-link (car posts))) out))))))
              (else (lp (cdr posts))))))))

(define (page-search request body index)
  (let* ((string (or (assoc-ref (request-form-data request body) "string") ""))
         (posts (find-posts-matching string index)))
    (respond `((h2 "search results: \"" ,string "\"")
               ,@(if (null? posts)
                     `((p "No posts matched your search string."))
                     (map (lambda (post)
                            `(p ,(post-link post)))
                          posts))))))

(define (page-show-tags request body index)
  (respond `((div (@ (id "tag-cloud"))
                  (h2 "all tags")
                  ,@(tag-cloud (top-tags index 200))))
           #:etag (assq-ref index 'master)
           #:title (string-append "all tags -- " *title*)))

(define (page-show-tag request body index tag)
  (let* ((tags (assq-ref index 'tags))
         (posts (map (lambda (key)
                       (post-from-key (assq-ref index 'master) key))
                     (hash-ref tags tag '()))))
    (if (pair? posts)
        (respond `((h2 "posts tagged \"" ,tag "\"")
                   ,@(map (lambda (post) `(p ,(post-link post)))
                          posts)
                   ,(related-tag-cloud tag index)) 
                 #:etag (assq-ref index 'master)
                 #:title (string-append "posts tagged \"" tag "\""))
        (respond `((h2 "Unknown tag " ,tag)
                   (p "No posts were found tagged as \"" ,tag "\"."))
                 #:status 404))))

(define (page-debug request body index)
  (respond `((p "hello world!")
             (table
              (tr (th "header") (th "value"))
              ,@(map (lambda (pair)
                       `(tr (td (tt ,(with-output-to-string
                                       (lambda () (display (car pair))))))
                            (td (tt ,(with-output-to-string
                                       (lambda ()
                                         (write (cdr pair))))))))
                     (request-headers request))))
           #:title "debug"))

(define (page-not-found request body index)
  (respond `((h1 "Page not found")
             (p "Unknown path: "
                ,(request-relative-path-str (pk 'not-found request))))
           #:status 404))

(define (page-feed-atom request body index)
  (let ((last-modified (let ((posts (published-posts index 1)))
                         (and (pair? posts)
                              (post-timestamp (car posts))))))
    (cond
     ((let ((since (request-if-modified-since request)))
        (and since (>= (date->timestamp since) last-modified)))
      (respond #f #:status 304
               #:last-modified (timestamp->date last-modified)
               #:etag (assq-ref index 'master)))
     (else
      (respond #f
               #:last-modified (and=> last-modified timestamp->date)
               #:doctype #f
               #:content-type "application/atom+xml"
               #:etag (assq-ref index 'master)
               #:sxml (append (atom-header last-modified)
                              (map
                               (lambda (post)
                                 (atom-entry post))
                               (published-posts index 10))))))))
