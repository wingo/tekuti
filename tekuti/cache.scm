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
  #:use-module (srfi srfi-19)
  #:export (make-empty-cache
            cached-response-and-body
            update-cache))
            
(define (cacheable-request? request)
  (and (memq (request-method request) '(GET HEAD))
       (not (request-authorization request))
       ;; We don't cache these conditional requests; just
       ;; if-modified-since and if-none-match.
       (not (request-if-match request))
       (not (request-if-range request))
       (not (request-if-unmodified-since request))))

(define (cacheable-response? response)
  (and (not (memq 'no-cache (response-pragma response)))
       (not (member '(no-cache . #t) (response-cache-control response)))
       (memq (response-code response) '(200 301 304 404 410))
       (null? (response-vary response))))

(define (make-empty-cache)
  '())

(define-syntax build-headers
  (syntax-rules ()
    ((_ k v-exp rest ...)
     (let ((v v-exp))
       (let ((tail (build-headers rest ...)))
         (if v
             (acons 'k v tail)
             tail))))
    ((_ tail)
     tail)))

(define (make-entry request response body)
  (let ((uri (request-uri request))
        (method (request-method request)))
    (case (response-code response)
      ((304)
       (lambda (request)
         (and (equal? (request-uri request) uri)
              (eq? (request-method request) method)
              (let ((last-modified (response-last-modified response))
                    (since (request-if-modified-since request)))
                (if (and last-modified since)
                    (<= (date->time-utc last-modified) (date->time-utc since))
                    #t))
              (let ((etag (response-etag response))
                    (match (request-if-none-match request)))
                (if (and etag match)
                    (and (list? match) (member etag match))
                    #t))
              (cons response body))))
      ((200)
       (lambda (request)
         (and (equal? (request-uri request) uri)
              (eq? (request-method request) method)
              (or (let ((last-modified (response-last-modified response))
                        (since (request-if-modified-since request))
                        (etag (response-etag response))
                        (match (request-if-none-match request)))
                    (and (or since match)
                         (or (not since)
                             (and last-modified
                                  (<= (date->time-utc last-modified)
                                      (date->time-utc since))))
                         (or (not match)
                             (and etag (list? match) (member etag match)))
                         (cons (build-response
                                #:code 304
                                #:headers (build-headers
                                           etag etag
                                           last-modified last-modified
                                           date (current-date)
                                           '()))
                               #f)))
                  (cons response body)))))
      (else
       (lambda (request)
         (and (equal? (request-uri request) uri)
              (eq? (request-method request) method)
              (cons response body)))))))

(define (cached-response-and-body cache request)
  (and cache
       (cacheable-request? request)
       (or-map (lambda (entry) (entry request))
               cache)))

(define (update-cache cache request response body)
  (if (and (cacheable-request? request)
           (cacheable-response? response))
      (cons (make-entry request response body)
            (take-max (or cache '()) 19))
      (or cache '())))
