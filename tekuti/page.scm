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
            page-debug
            page-not-found))

(define (relurl path . body)
  `(a (@ (href ,(string-append *public-url-base* path)))
      ,@body))

(define (make-post-key . parts)
  (url:encode (format #f "~{~a~^/~}" (map url:encode parts))))

(define (not-implemented request . args)
  (rcons* request
          'status 500
          'body `((h1 "Not yet implemented")
                  (p "Path handler not yet implemented: "
                     ,(rref request 'path-str)))))

(define (post-editing-form post)
  `(form (@ (method "POST")
            (action ,(string-append *public-url-base* 
                                    (if post
                                        (string-append "admin/modify-post/"
                                                       (url:encode (assq-ref post 'key)))
                                        "admin/new-post"))))
         (p "title: "
            (input (@ (name "title") (type "text")
                      (value ,(if post (assq-ref post 'title) "")))))
         (div (textarea (@ (name "body") (rows "20") (cols "80"))
                        ,(if post (post-raw-content post) "")))
         (input (@ (type "submit")
                   (value ,(if post "edit post" "new post"))))))

(define (sidebar-ul body)
  `(div (@ (id "menu"))
        (ul ,@body)))

(define (with-authentication request thunk)
  (if (request-authenticated? request)
      (thunk)
      (rcons* (rpush 'output-headers
                     '("WWW-Authenticate" . "Basic realm=\"Tekuti\"")
                     request)
              'status 401
              'body `((p "Authentication required, yo")))))

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
             'body `(,(sidebar-ul `((li (h2 ,(relurl "admin/posts" "posts"))
                                        (ul ,@(post-links 10)))
                                    (li (h2 "recent comments")
                                        (p "ain't got none"))))
                     (h2 "new post")
                     ,(post-editing-form #f))))))

(define (admin-post-link post)
  (relurl (string-append "admin/posts/"
                         (url:encode (assq-ref post 'key)))
          (assq-ref post 'title)))

(define (post-link post)
  (relurl (string-append "archives/" (url:decode (assq-ref post 'key)))
          (assq-ref post 'title)))

(define (page-admin-posts request index)
  (with-authentication
   request
   (lambda ()
     (define (post-headers)
       (map (lambda (post)
              ;; double-encoding is a hack to trick apache
              `(h3 ,(relurl (string-append "admin/posts/" (url:encode (assq-ref post 'key)))
                            (assq-ref post 'title))))
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
               'body `((h1 ,(assq-ref post 'title))
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

(define (show-post post comments?)
  `((h2 (@ (class "storytitle"))
        ,(post-link post))
    (div (@ (class "post"))
         (h3 (@ (class "meta"))
             ,(post-readable-date post)
             " (" ,@(list-intersperse (post-category-links post)
                                      " | ")
             ")")
         (div (@ (class "storycontent"))
              ,(post-sxml-content post))
         ,@(if comments? '()
               (list (post-sxml-n-comments post))))
    ,@(if comments?
          (list (post-sxml-comments post))
          '())))

;;                     (a (@ (href ,new-url)) ,new-url)

(define (page-admin-modify-post request index key)
  (with-authentication
   request
   (lambda ()
     (not-implemented request index))))
(define page-delete-comment not-implemented)
(define page-delete-post not-implemented)

(define (tag-cloud index)
  (define (determine-sizes counts)
    (let ((maxcount (apply max counts)))
      (map (lambda (x)
             (floor (+ 80 (* 120 (/ x maxcount)))))
           counts)))
  (let* ((cats (hash-fold (lambda (k v seed) (acons k (length v) seed))
                          '() (assq-ref index 'categories)))
         (top-20 (dsu-sort (take-max (dsu-sort cats cdr >) 20)
                           car string<?)))
    `(ul (li (@ (style "line-height: 150%"))
             ,@(list-intersperse
                (map (lambda (name size)
                       `(a (@ (href ,(string-append
                                      *public-url-base* "tags/"
                                      (url:encode name)))
                              (rel "tag")
                              (style ,(format #f "font-size: ~d%" size)))
                           ,name))
                     (map car top-20)
                     (determine-sizes (map cdr top-20)))
                " "))
         )))

(define (main-sidebar request index)
  (sidebar-ul
   `((li (h2 "orbit")
         (ul
          ,@(map (lambda (pair)
                   `(li (a (@ (href ,(car pair))) ,(cdr pair))))
                 '(("http://advogato.org/recentlog.html?thresh=3" . "advogato")
                   ("http://ambient.2y.net/" . "ambient")
                   ("http://planet.gnome.org/" . "gnome")
                   ("http://gstreamer.freedesktop.org/planet/" . "gstreamer")
                   ("http://planet.lisp.org/" . "lisp")))))
     (li (h2 "tags "
             (a (@ (href ,(string-append *public-url-base* "tags/")))
                ">>"))
         ,(tag-cloud index)))))


(define (page-index request index)
  (rcons* request
          'title "my bloggidy blog"
          'body `(,(main-sidebar request index)
                  ,@(map (lambda (post)
                           (show-post post #f))
                         (take-max (assq-ref index 'posts) 10)))))

(define (page-show-post request index year month day post)
  (let ((slug (make-post-key year month day post)))
    (cond
     ((false-if-git-error
       (git-rev-parse (string-append (assq-ref index 'master) ":" slug)))
      => (lambda (tree)
           (let ((post (post-from-tree slug tree)))
             (rcons* request
                     'title (assq-ref post 'title)
                     'body (show-post post #t)))))
     (else
      (page-not-found request index)))))

(define (page-new-comment request index year month day post)
  (let ((slug (make-post-key year month day post))
        (data (request-form-data request)))
    (cond
     ((false-if-git-error
       (git-rev-parse (string-append (assq-ref index 'master) ":" slug)))
      => (lambda (tree)
           (cond
            ((bad-new-comment-post? data)
             => (lambda (reason)
                  (pk reason)
                  (rcons* request
                          'body `((p "Bad post data: " ,reason)))))
            (else
             (rcons* request
                     'body `((p "hey hey hey like fat albert")))))))
     (else
      (page-not-found request index)))))

(define/kwargs (date-increment date (day 0) (month 0) (year 0))
  (make-date (date-nanosecond date) (date-second date)
             (date-minute date) (date-minute date)
             (+ (date-day date) day) (+ (date-month date) month)
             (+ (date-year date) year) (date-zone-offset date)))

(define (date-comparator date comp)
  (let ((this (time-second (date->time-utc date))))
    (lambda (that)
      (comp that this))))

(define (date-before? date)
  (date-comparator date <))

(define (date-after? date)
  (date-comparator date >))

(define (compose1 proc . procs)
  (if (null? procs)
      proc
      (let ((other (apply compose1 procs)))
        (lambda (x)
          (proc (other x))))))

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
    
      (let lp ((posts (assq-ref index 'posts)))
        (pk 'foo (or (null? posts) (car posts)))
        (cond ((or (null? posts) (too-early? (car posts)))
               (rcons* request
                       'title "no posts found"
                       'body `((h1 "No posts found")
                               (p "No posts were found in the specified period."))))
              ((early-enough? (car posts))
               (let lp ((posts posts) (new-header (make-date-header #t)) (out '()))
                 (cond
                  ((or (null? posts) (too-early? (car posts)))
                   (rcons* request
                           'title "archives"
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
          'title "hello"
          'body `((p "hello world!")
                  (table
                   (tr (th "header") (th "value"))
                   ,@(map (lambda (pair)
                            `(tr (td ,(car pair)) (td ,(cdr pair))))
                          (rref request 'headers))))))

(define page-search not-implemented)

(define (page-not-found request index)
  (pk request)
  (rcons* request
          'status 404
          'body `((h1 "Page not found")
                  (p "Unknown path: " ,(rref request 'path-str)))))
