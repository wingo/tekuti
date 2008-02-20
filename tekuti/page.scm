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
  #:use-module (tekuti url)
  #:use-module (tekuti request)
  #:use-module (srfi srfi-34)
  #:export (page-admin
            page-admin-posts
            page-admin-post
            page-admin-new-post
            page-admin-new-comment
            page-admin-modify-post 
            page-admin-delete-comment 
            page-admin-delete-post 
            page-index 
            page-show-post 
            page-archives 
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

(define (show-post slug index)
  `(sxml . (p "hello" ,slug)))

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

(define (page-admin request index)
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
                  ,(post-editing-form #f))))

(define (admin-post-link post)
  (relurl (string-append "admin/posts/"
                         (url:encode (assq-ref post 'key)))
          (assq-ref post 'title)))

(define (post-link post)
  (relurl (string-append "archives/" (url:decode (assq-ref post 'key)))
          (assq-ref post 'title)))

(define (page-admin-posts request index)
  (define (post-headers)
    (map (lambda (post)
           ;; double-encoding is a hack to trick apache
           `(h3 ,(relurl (string-append "admin/posts/" (url:encode (assq-ref post 'key)))
                         (assq-ref post 'title))))
         (assq-ref index 'posts)))
  (rcons* request
          'body `((h1 "all your posts are belong to tekuti")
                  ,@(post-headers))))

(define (page-admin-post request index key)
  (let ((post (post-from-key (assq-ref index 'master) key)))
    (pk 'foo post)
    (rcons* request
          'body `((h1 ,(assq-ref post 'title))
                  ,(post-editing-form post)))))

(define (decode-form-data request)
  (let-request request (headers post-data)
    (if (string-null? post-data)
        '()
        (let ((content-type (assoc-ref headers "content-type")))
          (cond
           ((equal? content-type "application/x-www-form-urlencoded")
            (map
             (lambda (piece)
               (let ((equals (string-index piece #\=)))
                 (if equals
                     (cons (url:decode (substring piece 0 equals))
                           (url:decode (substring piece (1+ equals))))
                     (cons (url:decode piece) ""))))
             (string-split post-data #\&)))
           (else
            (error "bad content-type" content-type)))))))

(define (page-admin-new-post request index)
  (let ((form-data (decode-form-data request)))
    (rcons* request
            'status 201         ; created
            'output-headers (acons "Location" *public-url-base*
                                   (rref request 'output-headers '()))
            'body `((h1 "Created")
                    (p "Created new post: " ,(assoc-ref form-data "title"))
                    (pre ,(assoc-ref form-data "body"))))))

(define (show-post post)
  `((h2 (@ (class "storytitle"))
        ,(post-link post))
    (div (@ (class "post"))
         (h3 (@ (class "meta"))
             ,(post-readable-date post)
             " (" ,@(list-intersperse (post-category-links post)
                                      " | ")
             ")")
         (div (@ (class "storycontent"))
              ,(post-sxml-content post)))))

;;                     (a (@ (href ,new-url)) ,new-url)

(define (page-admin-modify-post request index key)
  (not-implemented request index))
(define page-new-comment not-implemented)
(define page-delete-comment not-implemented)
(define page-delete-post not-implemented)
(define page-index not-implemented)

(define (page-show-post request index year month day post)
  (let ((slug (make-post-key year month day post)))
    (cond
     ((false-if-git-error
       (git-rev-parse (string-append (assq-ref index 'master) ":" slug)))
      => (lambda (tree)
           (let ((post (post-from-tree slug tree)))
             (pk post)
             (rcons* request
                     'title (assq-ref post 'title)
                     'body (show-post post)))))
     (else
      (page-not-found request index)))))

(define page-archives not-implemented)

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
