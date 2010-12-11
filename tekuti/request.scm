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

(define-module (tekuti request)
  #:use-module ((srfi srfi-1) #:select (find-tail))
  #:use-module (tekuti match-bind)
  #:use-module (tekuti util)
  #:use-module (web uri)
  #:use-module (web request)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 rdelim)
  #:use-module (tekuti config)
  #:use-module (tekuti base64)
  #:export (request-relative-path
            request-relative-path-str
            request-query-ref
            request-path-case
            request-authenticated?
            request-form-data))

(define* (parse-www-form-urlencoded str #:optional (charset "utf-8"))
  (map
   (lambda (piece)
     (let ((equals (string-index piece #\=)))
       (if equals
           (cons (uri-decode (substring piece 0 equals) #:charset charset)
                 (uri-decode (substring piece (1+ equals)) #:charset charset))
           (cons (uri-decode piece #:charset charset) ""))))
   (string-split str #\&)))

(define (request-relative-path r)
  (let ((base *private-path-base*)
        (path (split-and-decode-uri-path (uri-path (request-uri r)))))
    (let ((tail (list-head-match base path (length base))))
      (or tail
          (error "unexpected path" path base)))))

(define (request-relative-path-str r)
  (encode-and-join-uri-path (request-relative-path r)))

(define (request-query-ref r param default)
  (let ((q (uri-query (request-uri r))))
    (cond
     ((and q (assoc param (parse-www-form-urlencoded q))) => cdr)
     (else default))))

(define (decode-string bv charset)
  (if (string-ci=? charset "utf-8")
      (utf8->string bv)
      (let ((p (open-bytevector-input-port bv)))
        (set-port-encoding! p charset)
        (read-delimited "" p))))

(define (request-form-data request body)
  (if (bytevector? body)
      ;; Since valid application/x-www-form-urlencoded content only has
      ;; ascii characters, treat the incoming data as ascii (well,
      ;; latin-1), then use the charset when percent-decoding the
      ;; content.
      (request-form-data request (decode-string body "iso-8859-1"))
      (if (or (not body) (string-null? body))
          '()
          (let* ((content-type (request-content-type request))
                 (charset (or (assoc-ref (cdr content-type) "charset")
                              "utf-8")))
            (cond
             ((equal? (car content-type) "application/x-www-form-urlencoded")
              (parse-www-form-urlencoded body charset))
             (else
              (error "bad content-type" content-type)))))))

;; danger here, regarding the optional alternate clauses...
(define (request-authenticated? request)
  (let ((auth (request-authorization request)))
    (and auth
         (match-bind "^Basic ([A-Za-z0-9+/=]*)$" auth (_ b64)
                     (match-bind "^([^:]*):(.*)$"
                                 (utf8->string (base64-decode b64))
                                 (_ user pass)
                                 (and (equal? user *admin-user*)
                                      (equal? pass *admin-pass*))
                                 #f)
                     #f))))

(define-syntax path-proc-case
  (lambda (stx)
    (define (optional-argument? arg)
      (eqv? (string-ref arg (- (string-length arg) 1)) #\?))
    (define (required-argument? arg)
      (eqv? (string-ref arg (- (string-length arg) 1)) #\!))
    (define (output-argument? arg)
      (or (optional-argument? arg) (required-argument? arg)))
    (define (process-clause clause)
      (syntax-case clause (else)
        ((else expr ...) clause)
        (((p ...) proc)
         (let ((pat (map (lambda (p)
                           (symbol->string (syntax->datum p)))
                         #'(p ...))))
           (cond
            ((find-tail output-argument? pat)
             => (lambda (tail)
                  (let* ((req (find-tail required-argument? tail))
                         (opt (find-tail optional-argument? tail))
                         (npat (length pat))
                         (ntail (length tail))
                         (nopt (if opt (length opt) 0))
                         (nreq (if req (- (length req) nopt) 0)))
                    #`((let ((pathtail (list-head-match '#,pat
                                                        path-var
                                                        (- #,npat #,ntail))))
                         ;;(pk pat npat ntail req opt nopt nreq path pathtail)
                         (if (and pathtail (>= (length pathtail) #,nreq)
                                  (<= (length pathtail) (+ #,nreq #,nopt)))
                             (append
                              pathtail
                              (make-list (- (+ #,nreq #,nopt) (length pathtail)) #f))
                             #f))
                       => (lambda (outargs)
                            (lambda args
                              (apply proc (append args outargs))))))))
            (else
             #`((equal? path-var '#,pat) proc)))))))
    (syntax-case stx ()
      ((_ path clause ...)
       (with-syntax (((cond-clause ...) (map process-clause #'(clause ...))))
         #'(let ((path-var path))
             (cond cond-clause ...)))))))

(define-syntax request-path-case
  (syntax-rules ()
    ((_ request clause ...)
     (let ((r request))
       (path-proc-case
        (cons (symbol->string (request-method r)) (request-relative-path r))
        clause ...)))))
