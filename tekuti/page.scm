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
  #:use-module (tekuti git)
  #:use-module (tekuti post)
  #:use-module (tekuti request)
  #:export (page-new-post 
            page-modify-post 
            page-new-comment 
            page-delete-comment 
            page-delete-post 
            page-index 
            page-show-post 
            page-archives 
            page-debug 
            page-search 
            page-show-post 
            page-debug
            page-not-found))

(define (make-post-slug y m day post)
  (url:encode (format #f "~a/~a/~a" y m (url:encode post))))

(define (show-post slug index)
  `(sxml . (p "hello" ,slug)))

(define (not-implemented request . args)
  (rcons* request
          'status 404
          'body `(p "Not implemented:" ,(rref request 'url))))

(define page-new-post not-implemented)
(define page-modify-post not-implemented)
(define page-new-comment not-implemented)
(define page-delete-comment not-implemented)
(define page-delete-post not-implemented)
(define page-index not-implemented)

(define (page-show-post request index year month day post)
  (let ((slug (make-post-slug year month day post)))
    (let ((tree (git-rev-parse (string-append (assq-ref index 'master) ":" slug))))
      (let ((post (post-from-tree slug tree)))
        `((title . "post")
          (sxml . (pre ,(with-output-to-string
                          (lambda ()
                            (write post))))))))))

(define page-archives not-implemented)

(define (page-debug request index)
  (rcons* request
          'title "hello"
          'body `(div
                  (p "hello world!")
                  (table
                   (tr (th "header") (th "value"))
                   ,@(map (lambda (pair)
                            `(tr (td ,(car pair)) (td ,(cdr pair))))
                          (rref request 'headers))))))

(define page-search not-implemented)

(define (page-not-found request index)
  (rcons* request
          'status 404
          'body `(p "Not found:" ,(rref request 'url))))

