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
  #:use-module (tekuti config)
  #:use-module (srfi srfi-1)
  #:export (read-headers write-headers let-headers
            visible-error page-not-found unimplemented
            url-path-split url-path-case url-relative-path-case))
            
(define (read-headers socket)
  (define (read-line*)
    (let ((line (read-line socket)))
      (if (eof-object? line)
          (error "unexpected eof")
          line)))
  (let lp ((keys '()) (values '()))
    (let ((k (read-line*)))
      (if (string=? k "end")
          (reverse (map cons keys values))
          (lp (cons k keys) (cons (read-line*) values))))))

(define (write-headers-headers headers port)
  (for-each
   (lambda (k v)
     (format port "~a\n~a\n" k v))
   (map car headers) (map cdr headers))
  (display "end\n" port))

(define-macro (let-headers headers bindings . body)
  (let ((headers-var (gensym)))
    `(let ((,headers-var ,headers))
       (let (,@(map (lambda (binding)
                      `(,(car binding)
                        (or (assoc-ref ,headers-var ,(cadr binding))
                            (error "Missing header:" ,(cadr binding)))))
                    bindings))
         ,@body))))

(define (visible-error . html-body)
  (throw 'visible-error 404 html-body))

(define (page-not-found path)
  (throw 'html-error 404 path))

(define (url-path-split path)
  (filter (lambda (x) (not (string-null? x)))
          (map url:decode (string-split path #\/))))

(define-macro (url-path-case method path . clauses)
  (define (optional-argument arg)
    (let ((len (string-length arg)))
      (and (eqv? (string-ref arg (1- len)) #\?)
           (substring arg 0 (1- len)))))
  (let ((method-sym (gensym)) (path-parts (gensym)))
    (define (process-clauses)
      (map (lambda (clause)
             (let ((pattern (car clause)) (body (cdr clause)))
               (cond
                ((eq? pattern 'else)
                 clause)
                (else
                 (let* ((method-match (car pattern))
                        (parts-match (map symbol->string (cdr pattern)))
                        (nargs (length parts-match))
                        (opt (or (find-tail optional-argument parts-match) '()))
                        (nopt (length opt))
                        (nreq (- nargs nopt)))
                   (cond
                    ((null? opt)
                     `((and (eq? ,method-sym ',method-match)
                            (equal? ,path-parts ',parts-match))
                       ,@body))
                    (else
                     `((and (eq? ,method-sym ',method-match)
                            (equal? (list-head ,path-parts ,nreq)
                                    ',(list-head parts-match nreq))
                            (< (length ,path-parts) ,nargs))
                       (apply
                        (lambda ,(map string->symbol (map optional-argument opt))
                          ,@body)
                        (let ((tail (list-tail ,path-parts ,nreq)))
                          (append tail (make-list (- ,nopt (length tail)) #f))))))))))))
           clauses))
    `(let ((,method-sym (string->symbol ,method))
           (,path-parts (url-path-split ,path)))
       (cond ,@(process-clauses)))))

(define-macro (url-relative-path-case method path . clauses)
  (let ((infix (map string->symbol (url-path-split *private-url-base*))))
    (define (munge-clause clause)
      (cond
       ((eq? (car clause) 'else) clause)
       (else
        (let ((method (caar clause))
              (parts (cdar clause))
              (body (cdr clause)))
          `((,method ,@infix ,@parts) ,@body)))))
    `(url-path-case ,method ,path
                    ,@(map munge-clause clauses))))

(define (unimplemented . args)
  (apply throw 'unimplemented args))
