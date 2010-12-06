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
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 poll)
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
  ;; See the note in (web request) regarding chars, bytes, and strings
  ;; for more notes on charsets.
  (set-port-encoding! port "ISO-8859-1")
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

(define-record-type <mod-lisp-server>
  (make-mod-lisp-server socket poll-idx poll-set)
  mod-lisp-server?
  (socket mod-lisp-socket)
  (poll-idx mod-lisp-poll-idx set-mod-lisp-poll-idx!)
  (poll-set mod-lisp-poll-set))

(define *error-events* (logior POLLHUP POLLERR))
(define *read-events* POLLIN)
(define *events* (logior *error-events* *read-events*))

;; -> server
(define* (mod-lisp-open #:key
                    (host #f)
                    (family AF_INET)
                    (addr (if host
                              (inet-pton family host)
                              INADDR_LOOPBACK))
                    (port 8080)
                    (socket (make-default-socket family addr port)))
  (listen socket 128)
  (sigaction SIGPIPE SIG_IGN)
  (let ((poll-set (make-empty-poll-set)))
    (poll-set-add! poll-set socket *events*)
    (make-mod-lisp-server socket 0 poll-set)))

;; -> (client request body | #f #f #f)
(define (mod-lisp-read server)
  (let* ((poll-set (mod-lisp-poll-set server)))
    (let lp ((idx (mod-lisp-poll-idx server)))
      (let ((revents (poll-set-revents poll-set idx)))
        (cond
         ((zero? idx)
          ;; The server socket, and the end of our downward loop.
          (cond
           ((zero? revents)
            ;; No client ready, and no error; poll and loop.
            (poll poll-set)
            (lp (1- (poll-set-nfds poll-set))))
           ((not (zero? (logand revents *error-events*)))
            ;; An error.
            (throw 'interrupt))
           (else
            ;; A new client. Add to set, poll, and loop.
            ;;
            ;; FIXME: preserve meta-info.
            (let ((client (accept (poll-set-port poll-set idx))))
              ;; Fully buffered.
              (setvbuf (car client) _IOFBF)
              ;; From "HOP, A Fast Server for the Diffuse Web", Serrano.
              (setsockopt (car client) SOL_SOCKET SO_SNDBUF (* 12 1024))
              (poll-set-add! poll-set (car client) *events*)
              (poll poll-set)
              (lp (1- (poll-set-nfds poll-set)))))))
         ((zero? revents)
          ;; Nothing on this port.
          (lp (1- idx)))
         ;; Otherwise, a client socket with some activity on
         ;; it. Remove it from the poll set.
         (else
          (let ((port (poll-set-remove! poll-set idx)))
            (cond
             ((eof-object? (peek-char port))
              ;; EOF.
              (close-port port)
              (lp (1- idx)))
             (else
              ;; Otherwise, try to read a request from this port.
              ;; Record the next index.
              (set-mod-lisp-poll-idx! server (1- idx))
              (with-throw-handler
               #t
               (lambda ()
                 (let ((req (read-request/mod-lisp port)))
                   (values port
                           req
                           (read-request-body/latin-1 req))))
               (lambda (k . args)
                 (false-if-exception (close-port port)))))))))))))

;; -> unspecified values
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
    (close-port (response-port response))))

;; -> unspecified values
(define (mod-lisp-close server)
  (let ((poll-set (mod-lisp-poll-set server)))
    (let lp ((n (poll-set-nfds poll-set)))
      (if (positive? n)
          (begin
            (close-port (poll-set-remove! poll-set (1- n)))
            (lp (1- n)))))))

(define-server-impl mod-lisp
  mod-lisp-open
  mod-lisp-read
  mod-lisp-write
  mod-lisp-close)
