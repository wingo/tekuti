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
;; Tags, tags, tags
;;
;;; Code:

(define-module (tekuti tags)
  #:use-module (tekuti util)
  #:use-module (tekuti url)
  #:use-module (tekuti config)
  #:use-module (tekuti post)
  #:use-module (tekuti git)
  #:use-module ((srfi srfi-1) #:select (filter))
  #:export (tag-link reindex-tags))

(define (tag-link tagname)
  `(a (@ (href ,(string-append *public-url-base* "tags/"
                               (url:encode tagname))))
      ,tagname))

(define (compute-tags posts)
  (let ((hash (make-hash-table)))
    (for-each
     (lambda (post)
       (for-each
        (lambda (cat)
          (hash-push! hash cat (post-key post)))
        (post-tags post)))
     posts)
    hash))

(define (reindex-tags old-index index)
  (compute-tags (filter post-published? (assq-ref index 'posts))))
