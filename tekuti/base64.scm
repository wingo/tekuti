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
;; base 64 y'all
;;
;;; Code:

(define-module (tekuti base64)
  #:export (base64-encode base64-decode))

(define b64-chars
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwzyz0123456789+/")

(define (int->b64-char i)
  (string-ref b64-chars (logand i 63)))
 
(define b64-char-ranges
  (map cons
       (map char->integer '(#\A #\a #\0 #\+ #\/))
       (map char->integer '(#\Z #\z #\9 #\+ #\/))))

(define (b64-char->int c)
  (let ((i (char->integer c)))
    (let lp ((ranges b64-char-ranges) (out 0))
      (cond ((null? ranges)
             (error "bad base64 char" c))
            ((and (>= i (caar ranges)) (<= i (cdar ranges)))
             (+ out (- i (caar ranges))))
            (else
             (lp (cdr ranges) (+ out (1+ (- (cdar ranges)
                                            (caar ranges))))))))))

(define make-bytevector make-string)
(define bytevector-ref string-ref)
(define bytevector-length string-length)
(define bytevector-set! string-set!)
(define bytevector-pad string-pad-right)
(define byte->integer char->integer)
(define integer->byte integer->char)

(define-macro (bytevector-map-n-to-m n m)
  `(lambda (proc s)
     (let* ((len (bytevector-length s))
            (out (make-bytevector (* len (/ ,m ,n)))))
       (let lp ((i 0) (j 0))
         (cond
          ((< i len)
           (let inner ((k 0) (bytes (proc ,@(map (lambda (x)
                                                   `(bytevector-ref s (+ i ,x)))
                                                 (iota n)))))
             (if (not (null? bytes))
                 (begin (bytevector-set! out (+ j k) (car bytes))
                        (inner (1+ k) (cdr bytes)))))
           (lp (+ i ,n) (+ j ,m)))
          (else out))))))

(define bytevector-map-3-to-4
  (bytevector-map-n-to-m 3 4))
(define bytevector-map-4-to-3
  (bytevector-map-n-to-m 4 3))

(define (base64-encode s)
  (let* ((npad (remainder (- 3 (remainder (bytevector-length s) 3)) 3))
         (out (bytevector-map-3-to-4
               (lambda (x y z)
                 (let ((n (logior (ash (byte->integer x) 16)
                                  (ash (byte->integer y) 8)
                                  (byte->integer z))))
                   (map int->b64-char
                        (list (ash n -18) (ash n -12) (ash n -6) n))))
               (bytevector-pad s (+ (bytevector-length s) npad)
                               (integer->byte 0)))))
    (string-append (substring out 0 (- (string-length out) npad))
                   (make-string npad #\=))))

(define (base64-decode s)
  (let* ((npad (cond ((string-suffix? "==" s) 2)
                     ((string-suffix? "=" s) 1)
                     (else 0)))
         (out (bytevector-map-4-to-3
               (lambda (w x y z)
                 (let ((n (logior (ash (b64-char->int w) 18)
                                  (ash (b64-char->int x) 12)
                                  (ash (b64-char->int y) 6)
                                  (b64-char->int z))))
                   (map integer->byte
                        (list (ash n -16) (logand (ash n -8) 255)
                              (logand n 255)))))
               (string-append (substring s 0 (- (string-length s) npad))
                              (make-string npad #\A)))))
    (substring out 0 (- (string-length out) npad))))
