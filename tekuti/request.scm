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

(define-module (tekuti request)
  #:use-module ((srfi srfi-1) #:select (find-tail))
  #:use-module (scheme kwargs)
  #:use-module (tekuti util)
  #:use-module (tekuti config)
  #:use-module (tekuti web)
  #:export (make-request rcons rcons* rref let-request
            request-path-case))

(define (make-request . keys-and-values)
  (apply rcons* '() keys-and-values))

(define (rcons k v request)
  (or (symbol? k) (error "request keys should be symbols"))
  (acons k v request))

(define (rcons* request . keys-and-values)
  (let lp ((request '()) (kv keys-and-values))
    (if (null? kv)
        request
        (lp (rcons (car kv) (cadr kv) request) (cddr kv)))))

(define/kwargs (rref request k (default #f) (default-proc #f))
  (let ((pair (assq k request)))
    (cond
     (pair (cdr pair))
     (default-proc (default-proc request k))
     (else default))))

(define-macro (let-request request bindings . body)
  (let ((request-var (gensym)))
    (define (make-binding b)
      (cond
       ((symbol? b) `(,b (,rref ,request-var ',b)))
       ((list? b) `(,(car b) (,rref ,request-var ',(car b) ,@(cdr b))))
       (else (error "what" b))))
    `(let ((,request-var ,request))
       (let (,@(map make-binding bindings))
         ,@body))))

(define-macro (path-proc-case path . clauses)
  (let ((path-var (gensym)))
    (define (optional-argument? arg)
      (eqv? (string-ref arg (1- (string-length arg))) #\?))
    (define (process-clause clause)
      (or (list-has-length? clause 2) (error "foo"))
      (if (eq? (car clause) 'else)
          clause
          (let ((pat (map symbol->string (car clause)))
                (proc (cadr clause)))
            (cond
             ((find-tail optional-argument? pat)
              => (lambda (tail)
                   (define test
                     (let* ((len (length pat))
                            (nopt (length tail))
                            (nreq (- len nopt)))
                       (lambda (path)
                         (let ((pathtail (list-head-match pat path nreq)))
                           (if (and pathtail (<= (length pathtail) nopt))
                               pathtail
                               #f)))))
                   `((,test ,path-var)
                     => (lambda (optargs)
                          (lambda args
                            (apply ,proc (append args optargs)))))))
             (else
              `((equal? ,path-var ',pat) ,proc))))))
    `(let ((,path-var ,path))
       (cond ,@(map process-clause clauses)))))

;; hmm, style mismatch between these let macros
(define-macro (request-path-case request . clauses)
  (define (make-path request)
    (let ((private-url-path (url-path-split *private-url-base*)))
      (let-request request (headers)
        (let-headers headers ((method "method") (path "url"))
          (let* ((tail (list-head-match private-url-path
                                        (url-path-split path)
                                        (length private-url-path))))
            (if (not tail)
                (error "unexpected path" path *private-url-base*)
                (cons method tail)))))))
  (let ((req-sym (gensym)))
    `(let* ((,req-sym ,request))
       (,path-proc-case
        (,make-path ,req-sym)
        ,@clauses))))
