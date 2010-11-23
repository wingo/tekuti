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
;; Web server implementation for mod-lisp.
;;
;;; Code:

(define-module (tekuti mod-lisp)
  #:use-module (ice-9 rdelim)
  #:use-module (system repl error-handling)
  #:use-module (rnrs bytevectors)
  #:use-module (web http)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server))


;;; FIXME: ignore SIGPIPE, otherwise apache dying will kill us

(define *mod-lisp-headers* (make-hash-table))

(define (define-mod-lisp-header! sym name parser)
  (hash-set! *mod-lisp-headers* name (cons sym parser)))

(define (mod-lisp-sym-and-parser name)
  (hash-ref *mod-lisp-headers* name))

(define-mod-lisp-header! 'server-protocol
  "server-protocol"
  parse-http-version)

(define-mod-lisp-header! 'method
  "method"
  parse-http-method)

(define-mod-lisp-header! 'url
  "url"
  parse-request-uri)

(define-mod-lisp-header! 'server-ip-addr
  "server-ip-addr"
  identity)

(define-mod-lisp-header! 'server-ip-port
  "server-ip-port"
  string->number)

(define-mod-lisp-header! 'remote-ip-addr
  "remote-ip-addr"
  identity)

(define-mod-lisp-header! 'remote-ip-port
  "remote-ip-port"
  string->number)

(define-mod-lisp-header! 'server-id
  "server-id"
  identity)

(define-mod-lisp-header! 'server-ip-addr
  "server-ip-addr"
  identity)

(define-mod-lisp-header! 'server-baseversion
  "server-baseversion"
  identity)

(define-mod-lisp-header! 'modlisp-version
  "modlisp-version"
  identity)

(define-mod-lisp-header! 'modlisp-major-version
  "modlisp-major-version"
  string->number)

(define (read-headers/mod-lisp socket)
  (define (read-line*)
    (let ((line (read-line socket)))
      (if (eof-object? line)
          (error "unexpected eof")
          line)))
  (let lp ((headers '()) (meta '()))
    (let ((k (read-line*)))
      (if (string=? k "end")
          (values (reverse! headers) (reverse! meta))
          (let ((sym-and-parser (mod-lisp-sym-and-parser k))
                (v (read-line*)))
            (if sym-and-parser
                (lp headers
                    (acons (car sym-and-parser)
                           ((cdr sym-and-parser) v)
                           meta))
                (call-with-values (lambda () (parse-header k v))
                  (lambda (k v)
                    (lp (acons k v headers) meta)))))))))

(define (read-request/mod-lisp port)
  (call-with-values (lambda () (read-headers/mod-lisp port))
    (lambda (headers meta)
      (build-request
       #:method (assq-ref meta 'method)
       #:uri (assq-ref meta 'url)
       #:version (assq-ref meta 'server-protocol)
       #:headers headers
       #:meta meta
       #:port port))))

(define (write-header/mod-lisp name val port)
  (if (string? name)
      ;; assume that it's a header we don't know about...
      (begin
        (display name port) (newline port)
        (display val port) (newline port))
      (let ((decl (lookup-header-decl name)))
        (if (not decl)
            (error "Unknown header" name)
            (begin
              (display (header-decl-name decl) port) (newline port)
              ((header-decl-writer decl) val port) (newline port))))))

(define (write-response-line/mod-lisp code phrase port)
  (write-header/mod-lisp "Status"
                         (string-append (number->string code) " " phrase)
                         port))

(define (write-headers/mod-lisp headers port)
  (for-each
   (lambda (pair) 
     (write-header/mod-lisp (car pair) (cdr pair) port))
   headers))

(define (write-response/mod-lisp r port)
  (write-response-line/mod-lisp (response-code r)
                                (response-reason-phrase r) port)
  (write-headers/mod-lisp (response-headers r) port)
  (display "end" port) (newline port)
  (if (eq? port (response-port r))
      r
      (build-response #:version (response-version r)
                      #:code (response-code r)
                      #:reason-phrase (response-reason-phrase r)
                      #:headers (response-headers r)
                      #:port port)))

(define (make-default-socket family addr port)
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (bind sock family addr port)
    sock))

;; -> server
(define* (mod-lisp-open #:key
                        (host #f)
                        (family AF_INET)
                        (addr (if host
                                  (inet-pton family host)
                                  INADDR_LOOPBACK))
                        (port 8080)
                        (socket (make-default-socket family addr port)))
  (listen socket 5)
  socket)

;; For mod-lisp, we don't do keep-alive.
(define (keep-alive? response)
  #f)

;; -> (keep-alive client request body | keep-alive #f #f #f)
(define (mod-lisp-read server keep-alive)
  (let* ((client (accept server))
         (req (read-request/mod-lisp (car client)))
         (body-str (read-request-body/latin-1 req)))
    (values keep-alive (car client) req body-str)))

;; -> (#f | client)
(define (mod-lisp-write server client response body)
  (let ((response (write-response/mod-lisp response client)))
    (cond
     ((not body))                       ; pass
     ((string? body)
      (write-response-body/latin-1 response body))
     ((bytevector? body)
      (write-response-body/bytevector response body))
     (else
      (error "Expected a string or bytevector for body" body)))
    (close-port (response-port response))
    #f))

;; -> unspecified values
(define (mod-lisp-close server)
  (close-port server))

(define-server-impl mod-lisp
  mod-lisp-open
  mod-lisp-read
  mod-lisp-write
  mod-lisp-close)
