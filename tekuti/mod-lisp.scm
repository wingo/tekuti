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
  #:use-module (ice-9 receive)
  #:use-module (ice-9 stack-catch)
  #:use-module (sxml simple)
  #:use-module (sxml transform)
  #:use-module (tekuti url)
  #:use-module (tekuti util)
  #:use-module (tekuti config)
  #:use-module (tekuti request)
  #:export (event-loop))

;;; thought: ignore SIGPIPE, otherwise apache dying will kill us

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

(define (write-headers headers port)
  (for-each
   (lambda (k v)
     (format port "~a\n~a\n" k v))
   (map car headers) (map cdr headers))
  (display "end\n" port))

(define xhtml-doctype
  (string-append
   "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" "
   "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"))

(define (templatize request)
  (let-request request (title body)
    `(html (head
            (title ,(or title "foo")))
           (body
            ,(or body '(p "what"))))))

(define *status-names*
  '((200 . "OK")
    (404 . "Not Found")
    (500 . "Internal Server Error")))

(define (status->string status)
  (format #f "~a ~a" status (or (assv-ref *status-names* status)
                                "Unknown Error")))

(define (write-body request socket)
  (display xhtml-doctype socket)
  (sxml->xml (templatize request) socket))

(define (connection-received socket sockaddr index handle-request)
  (let ((headers (read-headers socket))
        (post-data "")) ;; blocks: (read-delimited "" socket)))

    (dbg "~a" headers)
    (catch
     #t
     (lambda ()
       (let ((res (pk (handle-request
                    (make-request 'headers headers
                                  'post-data post-data)
                    index))))
         (let-request res ((status 200))
           (write-headers `(("Status" . ,(status->string status))
                            ("Content-Type" . "text/html"))
                          socket)
           (write-body res socket))))
     (lambda args
       (write-headers '(("Status" . "500 Internal Server Error")
                        ("Content-Type" . "text/plain"))
                      socket)
       (write args socket)
       (newline)
       (with-output-to-port socket backtrace))
     (lambda args
       (fluid-set! the-last-stack (make-stack #t 2 0))
       (apply throw args)))
             
    (close-port socket)))

(define (with-socket proc)
  (pk 'listening)
  (let ((socket (socket PF_INET SOCK_STREAM 0)))
    (bind socket AF_INET (inet-aton *host*) *port*)
    (listen socket *backlog*)
    (unwind-protect
     (proc socket)
     (shutdown socket 2))))

(define (inner-loop socket cookie index handle-request maybe-reindex)
  (let* ((pair (accept socket))
         (fd (car pair))
         (sockaddr (cdr pair)))
    (receive
     (new-cookie new-index) (maybe-reindex cookie index)
     (pk new-cookie new-index)
     (connection-received (car pair) (cdr pair) new-index handle-request)
     (inner-loop socket new-cookie new-index handle-request maybe-reindex))))

(define (event-loop handle-request maybe-reindex)
  (with-socket
   (lambda (socket)
     (inner-loop socket #f #f handle-request maybe-reindex))))
