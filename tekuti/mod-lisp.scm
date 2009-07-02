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

(define-module (tekuti mod-lisp)
  #:use-module (ice-9 rdelim)
  #:use-module (tekuti config)
  #:use-module (tekuti util)
  #:use-module (tekuti index)
  #:use-module (tekuti request)
  #:use-module (tekuti web)
  #:export (event-loop))

;;; FIXME: ignore SIGPIPE, otherwise apache dying will kill us

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
          (lp (cons (string-downcase k) keys)
              (cons (read-line*) values))))))

(define (read-chars nchars port)
  (let ((buf (make-string nchars)))
    (read-delimited! "" buf port)
    buf))

(define (write-headers headers port)
  (for-each
   (lambda (pair)
     (format port "~a\n~a\n" (car pair) (cdr pair)))
   headers)
  (display "end\n" port))

(define (write-body request socket)
  ((rref request 'output error) socket))

(define (request-output-headers request)
  (rref request 'output-headers '()))

(define (connection-received socket sockaddr index)
  (let* ((headers (read-headers socket))
         (post-data (read-chars (string->number
                                 (or (assoc-ref headers "content-length")
                                     "0"))
                                socket)))
    (catch
     #t
     (lambda ()
       (let ((res (handle-request (make-request 'headers headers
                                                'post-data post-data)
                                  index)))
         (write-headers (request-output-headers res) socket)
         (write-body res socket)))
     (lambda args
       (write-headers '(("Status" . "500 Internal Server Error")
                        ("Content-Type" . "text/plain"))
                      socket)
       (display "Sorry bub, had an error. Please let the admin know. Thanks!\n"
                socket)
       (write headers) (newline)
       (write args) (newline)
       (backtrace))
     (lambda args
       (fluid-set! the-last-stack (make-stack #t 2 0))
       (apply throw args)))
             
    (close-port socket)))

(define (with-socket proc)
  (pk 'listening)
  (let ((socket (socket PF_INET SOCK_STREAM 0)))
    (setsockopt socket SOL_SOCKET SO_REUSEADDR 1)
    (bind socket AF_INET (inet-aton *host*) *port*)
    (listen socket *backlog*)
    (unwind-protect
     (proc socket)
     (shutdown socket 2))))

(define (inner-loop socket index)
  (let* ((pair (accept socket))
         (fd (car pair))
         (sockaddr (cdr pair))
         (new-index (maybe-reindex index)))
    (connection-received fd sockaddr (cdr new-index))
    (inner-loop socket new-index)))

(define (event-loop)
  (with-socket
   (lambda (socket)
     (format #t "entering inner loop\n")
     (inner-loop socket (read-index)))))

