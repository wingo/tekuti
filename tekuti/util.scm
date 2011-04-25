;; Tekuti
;; Copyright (C) 2008, 2010, 2011 Andy Wingo <wingo at pobox dot com>

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
;; Utility procedures and macros.
;;
;;; Code:

(define-module (tekuti util)
  #:use-module (tekuti match-bind)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:export (with-output-to-string* with-input-from-string*
            expanduser match-lines dbg unwind-protect dsu-sort
            hash-push! list-has-length? list-head-match mapn filter-mapn
            take-max read-hash write-hash shell:quote foldn
            call-with-temp-file emailish? urlish?
            date-increment date-comparator date-before? date-after? compose1
            rfc822-date->timestamp timestamp->rfc822-date timestamp->atom-date
            date->timestamp timestamp->date string-split/trimming
            list-intersperse with-time-debugging))

(define (with-output-to-string* thunk)
  (let ((port (open-output-string)))
    (with-output-to-port port thunk)
    (let ((str (get-output-string port)))
      (close-port port)
      str)))

(define (with-input-from-string* str thunk)
  (let* ((port (open-input-string str))
         (res (with-input-from-port port thunk)))
    (close-port port)
    res))

(define (emailish? x)
  (match-bind "^([a-zA-Z0-9._+-]+)@([a-zA-Z0-9-]+\\.)+[a-zA-Z]+$"
              x (_ . args)
              x
              #f))

;; hacky #fragment interpreting...
(define (urlish? x)
  (match-bind "^https?://([a-zA-Z0-9-]+\\.)+[a-zA-Z]+(/[a-zA-Z0-9$_.+!*'(),;/?:#@&=-]*)?$"
              x (_ . args)
              x
              #f))

;; bad name relative to mapn...
(define (foldn kons n knil values)
  (if (null? values)
      knil
      (foldn kons n
             (apply kons knil (list-head values n))
             (list-tail values n))))

(define-syntax unwind-protect
  (syntax-rules ()
    ((_ form cleanup0 cleanups ...)
     (dynamic-wind (lambda () #t)
                   (lambda () form)
                   (lambda () cleanup0 cleanups ...)))))

(define (call-with-temp-file contents proc)
  (let* ((template (string-copy "/tmp/tekutiXXXXXX"))
         (tmp (mkstemp! template)))
    (display contents tmp)
    (close tmp)
    (unwind-protect
     (proc template)
     (delete-file template))))

(define (shell:quote str)
  (with-output-to-string*
    (lambda ()
      (display #\')
      (string-for-each (lambda (ch)
                         (if (eqv? ch #\')
                             (begin (display #\\) (display #\'))
                             (display ch)))
                       str)
      (display #\'))))
      
(define (expanduser path)
  (let ((parts (string-split path #\/)))
    (if (eqv? (string-ref (car parts) 0) #\~)
        (let ((user (if (= (string-length (car parts)) 1)
                        (passwd:name (getpwuid (geteuid)))
                        (substring (car parts) 1))))
          (string-join (cons (passwd:dir (getpwnam user)) (cdr parts)) "/"))
        path)))

(define-syntax match-lines
  (syntax-rules ()
    ((_ string pattern bindings expr)
     (fold
      (lambda (line seed)
        (match-bind pattern line bindings
                    (cons expr seed)
                    seed))
      '() (string-split string #\newline)))))

(define (dbg fmt . args)
  (apply format (current-error-port) fmt args))

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

(define (filter-mapn proc l nmax)
  (let lp ((in l) (out '()) (n nmax))
    (if (or (null? in) (zero? n))
        (reverse out)
        (let ((val (proc (car in))))
          (if val
              (lp (cdr in) (cons val out) (1- n))
              (lp (cdr in) out n))
          ))))

(define (list-intersperse src-l elem)
  (if (null? src-l) src-l
      (let loop ((l (cdr src-l)) (dest (cons (car src-l) '())))
        (if (null? l) (reverse dest)
            (loop (cdr l) (cons (car l) (cons elem dest)))))))

(define (gettimeofday-diff prev)
  (let ((now (gettimeofday)))
   (+ (- (car now) (car prev))
      (* 1e-6 (- (cdr now) (cdr prev))))))

(define (with-time-debugging* proc)
  (pk 'start-clock)
  (let ((start (gettimeofday)))
    (unwind-protect
     (proc)
     (pk 'stop-clock (gettimeofday-diff start)))))

(define-syntax with-time-debugging
  (syntax-rules ()
    ((_ form0 forms ...)
     (with-time-debugging* (lambda () form0 forms ...)))))

(define (memoize1 proc)
  (let ((old-args #f) (cache #f) (proc proc))
    (lambda args
      (if (equal? args old-args)
          cache
          (let ((val (apply proc args)))
            (set! old-args args)
            (set! cache val)
            val)))))

(define (write-hash h)
  (write (hash-fold acons '() h)))

(define (read-hash)
  (let ((h (make-hash-table)))
    (for-each (lambda (pair)
                (hash-set! h (car pair) (cdr pair)))
              (read))
    h))

(define* (date-increment date #:key (day 0) (month 0) (year 0))
  (make-date (date-nanosecond date) (date-second date)
             (date-minute date) (date-minute date)
             (+ (date-day date) day) (+ (date-month date) month)
             (+ (date-year date) year) (date-zone-offset date)))

(define (date-comparator date comp)
  (let ((this (time-second (date->time-utc date))))
    (lambda (that)
      (comp that this))))

(define (date-before? date)
  (date-comparator date <))

(define (date-after? date)
  (date-comparator date >))

(define (compose1 proc . procs)
  (if (null? procs)
      proc
      (let ((other (apply compose1 procs)))
        (lambda (x)
          (proc (other x))))))

(define (string-split/trimming string delimiter)
  (map string-trim-both (string-split string delimiter)))

(define (rfc822-date->timestamp str)
  (+ (time-second (date->time-utc
                   (string->date str "~a, ~d ~b ~Y ~H:~M:~S GMT")))
     (date-zone-offset (current-date))))

(define (timestamp->date timestamp)
  (time-utc->date (make-time time-utc 0 timestamp) 0))

(define (date->timestamp date)
  (time-second (date->time-utc date)))

(define (timestamp->atom-date timestamp)
  (date->string (timestamp->date timestamp)
                "~Y-~m-~dT~H:~M:~SZ"))

(define (timestamp->rfc822-date timestamp)
  (date->string (timestamp->date timestamp)
                "~a, ~d ~b ~Y ~H:~M:~S GMT"))

