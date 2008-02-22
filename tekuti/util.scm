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

(define-module (tekuti util)
  #:use-module (match-bind)
  #:use-module (srfi srfi-1)
  #:export (expanduser match-lines dbg unwind-protect dbg dsu-sort
            hash-push! list-has-length? list-head-match mapn
            take-max
            list-intersperse with-backtrace define-memoized))

(define (expanduser path)
  (let ((parts (string-split path #\/)))
    (if (eqv? (string-ref (car parts) 0) #\~)
        (let ((user (if (= (string-length (car parts)) 1)
                        (cuserid)
                        (substring (car parts) 1))))
          (string-join (cons (passwd:dir (getpwnam user)) (cdr parts)) "/"))
        path)))

(define-macro (match-lines string pattern bindings expr)
  (let ((line (gensym)) (seed (gensym)))
    `(,fold
      (lambda (,line ,seed)
        (match-bind ,pattern ,line ,bindings
                    (cons ,expr ,seed)
                    ,seed))
      '() (string-split ,string #\newline))))

(define (dbg fmt . args)
  (apply format (current-error-port) fmt args))

(define-macro (unwind-protect form . cleanups)
  `(dynamic-wind (lambda () #t)
       (lambda () ,form)
       (lambda () ,@cleanups)))

(define (dsu-sort list key less)
  (map cdr
       (stable-sort (map (lambda (x) (cons (key x) x)) list)
                    (lambda (x y) (less (car x) (car y))))))

(define (hash-push! h key value)
  (let ((handle (hash-create-handle! h key '())))
    (set-cdr! handle (cons value (cdr handle)))))

(define (take-max list n)
  (if (or (null? list) (zero? n))
      '()
      (cons (car list) (take-max (cdr list) (1- n)))))

(define (list-has-length? list len)
  (cond
   ((zero? len) (null? list))
   ((null? list) #f)
   (else (list-has-length? (cdr list) (1- len)))))

;; returns tail of l2
(define (list-head-match l1 l2 n)
  (cond
   ((zero? n) l2)
   ((null? l2) #f)
   ((not (equal? (car l1) (car l2))) #f)
   (else (list-head-match (cdr l1) (cdr l2) (1- n)))))

(define (mapn proc l nmax)
  (let lp ((in l) (out '()) (n nmax))
    (if (or (null? in) (zero? n))
        (reverse out)
        (lp (cdr in) (cons (proc (car in)) out) (1- n)))))

(define (list-intersperse src-l elem)
  (if (null? src-l) src-l
      (let loop ((l (cdr src-l)) (dest (cons (car src-l) '())))
        (if (null? l) (reverse dest)
            (loop (cdr l) (cons (car l) (cons elem dest)))))))

(define (handle-error key . args)
  (let ((cep (current-error-port))
        (highlights (if (or (eq? key 'wrong-type-arg)
                            (eq? key 'out-of-range))
                        (list-ref args 3)
                        '())))
    (newline cep)
    (display "Backtrace:\n")
    (display-backtrace (fluid-ref the-last-stack) cep
                       #f #f highlights)
    (newline cep)
    (if (= (length args) 4)
        (apply display-error (fluid-ref the-last-stack) cep args)
        (format cep "~a" args))
    (force-output cep)
    (apply throw key args)))

(define (with-backtrace proc)
  (debug-enable 'backtrace)
  (start-stack 'with-backtrace
               (catch #t
                      proc
                      handle-error
                      (lambda args
                        (fluid-set! the-last-stack (make-stack #t 2 0))
                        (apply throw args)))))

(define (memoize1 proc)
  (let ((old-args #f) (cache #f) (proc proc))
    (lambda args
      (if (equal? args old-args)
          cache
          (let ((val (apply proc args)))
            (set! old-args args)
            (set! cache val)
            val)))))

(define-macro (define-memoized form . body)
  `(begin
     (define ,form ,@body)
     (set! ,(car form) (,memoize1 ,(car form)))))
