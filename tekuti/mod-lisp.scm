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
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (sxml simple)
  #:use-module (sxml transform)
  #:use-module (tekuti config)
  #:use-module (tekuti util)
  #:use-module (tekuti git) ; rev-parse
  #:use-module (tekuti request)
  #:use-module (tekuti web)

  ;; these for their reindex methods
  #:use-module (tekuti post)
  #:use-module (tekuti categories)
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
  (let ((sxml (rref request 'sxml #f)))
    (if sxml
        (begin (display (rref request 'doctype "") socket)
               (sxml->xml sxml socket))
        (display "" socket))))

(define (request-output-headers request)
  (let ((rheads '(("Content-Type" . output-type))))
    (map (lambda (h)
           (cons (car h)
                 (or (and=> (assoc-ref (car h) rheads)
                            (lambda (k) (rref request k #f)))
                     (cdr h))))
         (rref request 'output-headers '()))))

(define (connection-received socket sockaddr index)
  (let* ((headers (pk (read-headers socket)))
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
    (setsockopt socket SOL_SOCKET SO_REUSEADDR 1)
    (bind socket AF_INET (inet-aton *host*) *port*)
    (listen socket *backlog*)
    (unwind-protect
     (proc socket)
     (shutdown socket 2))))

(define (with-time-debugging proc)
  (let ((start (gettimeofday)))
    (pk 'start-clock)
    (let ((val (proc)))
      (let ((stop (gettimeofday)))
        (pk 'stop-clock (+ (- (car stop) (car start))
                           (* 1e-6 (- (cdr stop) (cdr start)))))
        
        val))))

(define (write-hash h)
  (write (hash-fold acons '() h)))

;; fixme: doesn't share structure with posts index
(define (read-hash)
  (let ((h (make-hash-table)))
    (for-each (lambda (pair)
                (hash-set! h (car pair) (cdr pair)))
              (read))
    h))

(define indices                          
  `((posts ,reindex-posts ,write ,read)
    (categories ,reindex-categories ,write-hash ,read-hash)))

(use-modules (statprof))
(define (reindex master)
  (with-backtrace
   (lambda ()
     (with-time-debugging
      (lambda ()
        (with-statprof #:hz 100
                       (fold (lambda (pair index)
                               (acons (car pair) ((cadr pair) index)
                                      index))
                             (acons 'master master '())
                             indices)))))))

(define (maybe-reindex old-index)
  (let ((master (git-rev-parse "master")))
    (if (and old-index (equal? (assq-ref (cdr old-index) 'master) master))
        old-index
        (let ((new-index (reindex master)))
          (cons (write-indices new-index (and=> old-index car) indices)
                new-index)))))

(define (inner-loop socket index)
  (let* ((pair (accept socket))
         (fd (car pair))
         (sockaddr (cdr pair))
         (new-index (maybe-reindex index)))
    (connection-received (car pair) (cdr pair) (cdr new-index))
    (inner-loop socket new-index)))

(define (event-loop)
  (with-socket
   (lambda (socket)
     (format #t "entering inner loop\n")
     (inner-loop socket (read-indices indices)))))

