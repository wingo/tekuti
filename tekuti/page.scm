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
;; This is the main script that will launch tekuti.
;;
;;; Code:

(define-module (tekuti page)
  #:use-module (tekuti config)
  #:use-module (tekuti util)
  #:use-module (tekuti git)
  #:use-module (tekuti post)
  #:use-module (tekuti comment)
  #:use-module (tekuti url)
  #:use-module (tekuti request)
  #:use-module (tekuti page-helpers)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-19)
  #:use-module (scheme kwargs)
  #:export (page-admin
            page-admin-posts
            page-admin-post
            page-admin-new-post
            page-admin-modify-post 
            page-admin-delete-comment 
            page-admin-delete-post 
            page-index 
            page-show-post 
            page-new-comment
            page-archives 
            page-show-tags
            page-show-tag
            page-debug 
            page-search 
            page-show-post 
            page-feed-atom
            page-debug
            page-not-found))

(define (make-post-key . parts)
  (url:encode (format #f "~{~a~^/~}" (map url:encode parts))))

(define (not-implemented request . args)
  (rcons* request
          'status 500
          'body `((h1 "Not yet implemented")
                  (p "Path handler not yet implemented: "
                     ,(rref request 'path-str)))))

(define (page-admin request index)
  (with-authentication
   request
   (lambda ()
     ;; here we need to be giving a dashboard view instead of this
     (define (post-links n)
       (mapn (lambda (post)
               `(li ,(admin-post-link post)))
             (assq-ref index 'posts)
             n))
     (rcons* request
             'body `(,(sidebar-ul `((li (h2 ,(rellink "admin/posts" "posts"))
                                        (ul ,@(post-links 10)))
                                    (li (h2 "recent comments")
                                        (p "ain't got none"))))
                     (h2 "new post")
                     ,(post-editing-form #f))))))

(define (page-admin-posts request index)
  (with-authentication
   request
   (lambda ()
     (define (post-headers)
       (map (lambda (post)
              `(h3 ,(admin-post-link post)))
            (assq-ref index 'posts)))
     (rcons* request
             'body `((h1 "all your posts are belong to tekuti")
                     ,@(post-headers))))))

(define (page-admin-post request index key)
  (with-authentication
   request
   (lambda ()
     (let ((post (post-from-key (assq-ref index 'master) key)))
       (rcons* request
               'body `((h1 ,(post-title post))
                       ,(post-editing-form post)))))))

(define (page-admin-new-post request index)
  (with-authentication
   request
   (lambda ()
     (let ((form-data (request-form-data request)))
       (rcons* request
               'status 201              ; created
               'output-headers (acons "Location" *public-url-base*
                                      (rref request 'output-headers '()))
               'body `((h1 "Created")
                       (p "Created new post: " ,(assoc-ref form-data "title"))
                       (pre ,(assoc-ref form-data "body"))))))))

(define (page-admin-modify-post request index key)
  (with-authentication
   request
   (lambda ()
     (not-implemented request index))))
(define page-delete-comment not-implemented)
(define page-delete-post not-implemented)

(define (page-index request index)
  (rcons* request
          'body `(,(main-sidebar request index)
                  ,@(map (lambda (post)
                           (show-post post #f))
                         (published-posts index 10)))))

(define (page-show-post request index year month day post)
  (cond
   ((post-from-key (assq-ref index 'master)
                   (make-post-key year month day post))
    => (lambda (post)
         (rcons* request
                 'title (string-append (post-title post) " -- " *title*)
                 'body (show-post post #t))))
   (else
    (page-not-found request index))))

(define (page-new-comment request index year month day name)
  (let ((data (request-form-data request)))
    (cond
     ((post-from-key (assq-ref index 'master)
                     (make-post-key year month day name))
      => (lambda (post)
           (cond
            ((bad-new-comment-post? data)
             => (lambda (reason)
                  (rcons* request
                          'body `((p "Bad post data: " ,(pk reason))))))
            (else
             (let ((comment (make-new-comment post data)))
               ;; nb: at this point, `post' is out-of-date
               (rcons* request
                       'title "comment posted"
                       'body `((p "Comment posted, thanks.")
                               ;; fixme: show the post directly; or a redirect?
                               (p "Back to the post: " ,(post-link post)))))))))
     (else
      (page-not-found request index)))))

;; fixme exception handling for input
(define (page-archives request index year month day)
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
               (rcons* request
                       'title *title*
                       'body `((h1 "No posts found")
                               (p "No posts were found in the specified period."))))
              ((early-enough? (car posts))
               (let lp ((posts posts) (new-header (make-date-header #t)) (out '()))
                 (cond
                  ((or (null? posts) (too-early? (car posts)))
                   (rcons* request
                           'title (string-append "archives -- " *title*)
                           'body (reverse out)))
                  ((new-header (car posts))
                   => (lambda (sxml)
                        (lp (cdr posts) (make-date-header (car posts))
                            (cons (post-link (car posts)) (append sxml out)))))
                  (else
                   (lp (cdr posts) new-header (cons `(p ,(post-link (car posts))) out))))))
              (else (lp (cdr posts))))))))

(define (page-show-tags request index)
  (not-implemented request index))

(define (page-show-tag request index tag)
  (not-implemented request index))

(define (page-debug request index)
  (rcons* request
          'title "debug"
          'body `((p "hello world!")
                  (table
                   (tr (th "header") (th "value"))
                   ,@(map (lambda (pair)
                            `(tr (td ,(car pair)) (td ,(cdr pair))))
                          (rref request 'headers))))))

(define page-search not-implemented)

(define (page-not-found request index)
  (rcons* (pk 'not-found request)
          'status 404
          'body `((h1 "Page not found")
                  (p "Unknown path: " ,(rref request 'path-str)))))

(define (page-feed-atom request index)
  (let ((last-modified (let ((posts (published-posts index 1)))
                         (and (pair? posts)
                              (post-timestamp (car posts)))))
        (server-name (request-server-name request)))
    (cond
     ((let ((since (assoc-ref (rref request 'headers '())
                              "If-Modified-Since")))
        (and since (>= (rfc822-date->timestamp since) last-modified)))
      (rcons* request
              'status 304
              'doctype #f))
     (else
      (rcons* (rpush 'output-headers (cons "Last-Modified"
                                           (timestamp->rfc822-date
                                            last-modified))
                     request)
              'doctype ""
              'content-type "application/atom+xml"
              'sxml (append (atom-header server-name last-modified)
                            (map
                             (lambda (post)
                               (atom-entry server-name post))
                             (published-posts index 10))))))))
