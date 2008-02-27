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
  #:use-module ((srfi srfi-1) #:select (find-tail fold))
  #:use-module (scheme kwargs)
  #:use-module (match-bind)
  #:use-module (tekuti util)
  #:use-module (tekuti url)
  #:use-module (tekuti config)
  #:use-module (tekuti base64)
  #:export (make-request rcons rcons* rpush rpush* rref let-request
            request-path-case request-authenticated?
            request-form-data))

(define (header-ref headers key default)
  (let ((pair (assoc key headers)))
    (if pair
        (cdr pair)
        default)))

(define *request-initializers*
  `((path . ,(lambda (r)
               (let ((private-url-path (url:path-split *private-url-base*))
                     (path (header-ref (rref r 'headers '())
                                       "url" *private-url-base*)))
                 (let* ((tail (list-head-match private-url-path
                                               (url:path-split path)
                                               (length private-url-path))))
                   (or tail (error "unexpected path" path *private-url-base*))
                   tail))))
    (path-str . ,(lambda (r)
                   (url:path-join (rref r 'path '()))))
    (method . ,(lambda (r)
                 (header-ref (rref r 'headers '()) "method" "GET")))))

(define (request-form-data request)
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

(define (make-request . keys-and-values)
  (fold (lambda (pair r)
          (rcons (car pair) ((cdr pair) r) r))
        (apply rcons* '() keys-and-values)
        *request-initializers*))

(define (rcons k v request)
  (or (symbol? k) (error "request keys should be symbols"))
  (acons k v request))

(define (rcons* request . keys-and-values)
  (let lp ((request request) (kv keys-and-values))
    (if (null? kv)
        request
        (lp (rcons (car kv) (cadr kv) request) (cddr kv)))))

(define (rpush k v request)
  (rcons k (cons v (rref request k '())) request))

(define (rpush* request . keys-and-values)
  (let lp ((request request) (kv keys-and-values))
    (if (null? kv)
        request
        (lp (rpush (car kv) (cadr kv) request) (cddr kv)))))

(define/kwargs (rref request k (default #f) (default-proc #f))
  (let ((pair (assq k request)))
    (cond
     (pair (cdr pair))
     (default-proc (default-proc request k))
     (else default))))

;; danger here, regarding the optional alternate clauses...
(define (request-authenticated? request)
  (let ((headers (rref request 'headers '())))
    (let ((auth (assoc-ref headers "Authorization")))
      (and auth
           (match-bind "^Basic ([A-Za-z0-9+/=]*)$" auth (_ b64)
                       (match-bind "^([^:]*):(.*)$"
                                   (base64-decode b64) (_ user pass)
                                   (and (equal? user *admin-user*)
                                        (equal? pass *admin-pass*))
                                   #f)
                       #f)))))

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
    (define (required-argument? arg)
      (eqv? (string-ref arg (1- (string-length arg))) #\!))
    (define (output-argument? arg)
      (or (optional-argument? arg) (required-argument? arg)))
    (define (process-clause clause)
      (or (list-has-length? clause 2) (error "foo"))
      (if (eq? (car clause) 'else)
          clause
          (let ((pat (map symbol->string (car clause)))
                (proc (cadr clause)))
            (cond
             ((find-tail output-argument? pat)
              => (lambda (tail)
                   (define test
                     (let* ((npat (length pat))
                            (ntail (length tail))
                            (req (find-tail required-argument? tail))
                            (opt (find-tail optional-argument? tail))
                            (nopt (if opt (length opt) 0))
                            (nreq (if req (- (length req) nopt) 0)))
                       (lambda (path)
                         (let ((pathtail (list-head-match pat path (- npat ntail))))
                           ;(pk pat npat ntail req opt nopt nreq path pathtail)
                           (if (and pathtail (>= (length pathtail) nreq)
                                    (<= (length pathtail) (+ nreq nopt)))
                               (append pathtail
                                       (make-list (- (+ nreq nopt) (length pathtail)) #f))
                               #f)))))
                   `((,test ,path-var)
                     => (lambda (outargs)
                          (lambda args
                            (apply ,proc (append args outargs)))))))
             (else
              `((equal? ,path-var ',pat) ,proc))))))
    `(let ((,path-var ,path))
       (cond ,@(map process-clause clauses)))))

(define (foldn kons n knil values)
  (if (null? values)
      knil
      (foldn kons n
             (apply kons knil (list-head values n))
             (list-tail values n))))

(define (rcons*-fold request . keys-and-procs)
  (foldn (lambda (request k proc)
           (rcons k (proc request) request))
         2 request keys-and-procs))

(define-macro (request-path-case request . clauses)
  `(,path-proc-case
    (,let-request ,request (method path)
                  (cons method path))
    ,@clauses))
