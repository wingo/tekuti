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

(define-module (tekuti web)
  #:use-module (web server)
  #:use-module (tekuti cache)
  #:use-module (tekuti request)
  #:use-module (tekuti index)
  #:use-module (tekuti page)
  #:use-module (tekuti config)
  #:export (main-loop))
            
(define (choose-handler request)
  (request-path-case
   request
   ((GET admin) page-admin)
   ((GET admin posts) page-admin-posts)
   ((GET admin posts post-key!) page-admin-post)
   ((POST admin new-post) page-admin-new-post)
   ;; would be fine to have e.g. (DELETE admin posts posts-key!), but
   ;; web browsers don't handle that
   ((POST admin modify-post post-key!) page-admin-modify-post)
   ((GET admin changes) page-admin-changes)
   ((GET admin changes sha1!) page-admin-change)
   ((POST admin revert-change sha1!) page-admin-revert-change)
   ((GET) page-index)
   ((GET archives year? month? day?) page-archives)
   ((GET archives year! month! day! post!) page-show-post)
   ((POST archives year! month! day! post!) page-new-comment)
   ((GET feed) page-feed-atom)
   ((GET feed atom) page-feed-atom)
   ((POST search) page-search)
   ((GET tags) page-show-tags)
   ((GET tags tag!) page-show-tag)
   ((GET debug) page-debug)
   (else page-not-found)))

(define handler
  (lambda (request body index cache)
    (let ((index (maybe-reindex index)))
      (call-with-values
          (lambda ()
            (if (cache-hit? cache (car index) request)
                (cache-ref cache (car index) request)
                (call-with-values
                    (lambda ()
                      ((choose-handler request) request body (cdr index)))
                  (lambda (response body)
                    (sanitize-response request response body)))))
        (lambda (response body)
          (values response body index
                  (cache-set cache (car index) request response body)))))))

;; The seemingly useless lambda is to allow for `handler' to be
;; redefined at runtime.
(define (main-loop)
  (run-server (lambda (r b i c) (handler r b i c))
              *server-impl*
              (if (list? *server-impl-args*)
                  *server-impl-args*
                  (*server-impl-args*))
              (read-index)
              #f))
