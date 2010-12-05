;; Tekuti
;; Copyright (C) 2010 Andy Wingo <wingo at pobox dot com>

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
;; A simple response cache.  The model is that all request-response
;; pairs that the cache sees are fresh and valid.  The application can
;; invalidate the cache simply by creating a new empty cache.
;;
;;; Code:

(define-module (tekuti cache)
  #:use-module (tekuti util)
  #:use-module (web request)
  #:use-module (web response)
  #:export (make-empty-cache
            cached-response-and-body
            update-cache))
            
(define (cacheable-request? request)
  (and (memq (request-method request) '(GET HEAD))
       (not (request-authorization request))))

(define (cacheable-response? response)
  (and (not (memq 'no-cache (response-pragma response)))
       (not (member '(no-cache . #t) (response-cache-control response)))
       (memq (response-code response) '(200 301 304 404 410))
       (null? (response-vary response))))

(define (make-empty-cache)
  '())

(define (make-entry matcher response body)
  (cons matcher (cons response body)))
(define (entry-matcher entry)
  (car entry))
(define (entry-response-body-pair entry)
  (cdr entry))

(define (cached-response-and-body cache request)
  (and cache
       (cacheable-request? request)
       (or-map (lambda (entry)
                 (and ((entry-matcher entry) request)
                      (entry-response-body-pair entry)))
               cache)))

(define (make-matcher request response)
  (let ((uri (request-uri request))
        (method (request-method request)))
    (lambda (request)
      (and (equal? (request-uri request) uri)
           (eq? (request-method request) method)))))

(define (update-cache cache request response body)
  (if (and (cacheable-request? request)
           (cacheable-response? response))
      (cons (make-entry (make-matcher request response) response body)
            (take-max (or cache '()) 9))
      (or cache '())))
