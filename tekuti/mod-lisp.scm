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
  #:use-module (sxml simple)
  #:use-module (sxml transform)
  #:use-module (tekuti url)
  #:use-module (tekuti util)
  #:use-module (tekuti config)
  #:export (event-loop))

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

(define (connection-received socket sockaddr handle-request)
  (let ((headers (read-headers socket))
        (post-data "")) ;; blocks: (read-delimited "" socket)))

    (dbg "~a" headers)
    (catch #t
           (lambda ()
             (let ((sxml (handle-request headers post-data)))
               (write-headers '(("Status" . "200 OK")
                                ("Content-Type" . "text/html"))
                              socket)
               (display xhtml-doctype socket)
               (sxml->xml sxml socket)))
           (lambda args
             (write-headers '(("Status" . "500 Internal Server Error")
                              ("Content-Type" . "text/plain"))
                            socket)
             (write args socket)))
             
    (close-port socket)))

(define (event-loop handle-request)
  (pk 'listening)
  (let ((socket (socket PF_INET SOCK_STREAM 0)))
    (bind socket AF_INET (inet-aton *host*) *port*)
    (listen socket *backlog*)
    (unwind-protect
     (let lp ((pair (accept socket)))
       (pk pair)
       (connection-received (car pair) (cdr pair) handle-request)
       (pk 'done)
       (lp (accept socket)))
     (shutdown socket 2))))

