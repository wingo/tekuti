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

(define-module (tekuti web)
  #:use-module (sxml simple)
  #:use-module (sxml transform)
  #:use-module (tekuti url)
  #:use-module (tekuti request)
  #:use-module (tekuti template)
  #:use-module (tekuti page)
  #:use-module (srfi srfi-1)
  #:export (let-headers header-ref
            handle-request))
            
(define *status-names*
  '((200 . "OK")
    (201 . "Created")
    (303 . "See Other")
    (304 . "Not Modified")
    (401 . "Unauthorized")
    (404 . "Not Found")
    (500 . "Internal Server Error")))

(define (status->string status)
  (format #f "~a ~a" status (or (assv-ref *status-names* status)
                                "Unknown Error")))

(define xhtml-doctype
  (string-append
   "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" "
   "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"))

;;; useless macro
(define-macro (let-headers headers bindings . body)
  (let ((headers-var (gensym)))
    `(let ((,headers-var ,headers))
       (let (,@(map (lambda (binding)
                      `(,(car binding)
                        (or (assoc-ref ,headers-var ,(cadr binding))
                            (error "Missing header:" ,(cadr binding)))))
                    bindings))
         ,@body))))

(define (make-output request)
  (lambda (port)
    (let ((sxml (or (rref request 'sxml #f)
                    (templatize request))))
      (if sxml
          (begin (display (rref request 'doctype "") port)
                 (sxml->xml sxml port))
          (display "" port)))))

(define (finalize request)
  ;; update output headers
  ;; templatize body
  (rpush* (rcons 'output (make-output request) request)
          'output-headers
          (cons "Status" (status->string (rref request 'status 200)))
          'output-headers
          (cons "Content-Type"
                (string-append (rref request 'content-type "text/html")
                               "; charset=utf-8"))))

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
   ((POST search) page-search)
   (else page-not-found)))

(define (handle-request request index)
  (let ((handler (choose-handler request)))
    (finalize (handler (rcons 'doctype xhtml-doctype request) index))))
