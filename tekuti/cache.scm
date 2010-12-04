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
;; A cache for responses.
;;
;;; Code:

(define-module (tekuti cache)
  #:use-module (web request)
  #:export (cache-hit?
            cache-ref
            cache-set))
            
(define (cache-hit? cache master request)
  (and cache
       (equal? (car cache) master)
       (eq? (request-method request) 'GET)
       (assoc (request-uri request) (cdr cache))
       #t))

(define (cache-ref cache master request)
  (apply values (assoc-ref (cdr cache) (request-uri request))))

(define (cache-set cache master request . args)
  (cons* master
         (cons (request-uri request) args)
         (if (and cache (equal? (car cache) master))
             (list-head (cdr cache) 9)
             '())))
