;; Tekuti
;; Copyright (C) 2008, 2010 Andy Wingo <wingo at pobox dot com>
;; Copyright (C) 2009 Andreas Rottmann <a dot rottmann at gmx dot at>

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
  #:use-module (rnrs bytevectors)
  #:export (base64-encode base64-decode))

(define b64-bytes
  (string->utf8
   "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwzyz0123456789+/"))

(define (int->b64-byte i)
  (bytevector-u8-ref b64-bytes (logand i 63)))
 
(define b64-byte-ranges
  (map cons
       (map char->integer '(#\A #\a #\0 #\+ #\/))
       (map char->integer '(#\Z #\z #\9 #\+ #\/))))

(define (b64-byte->int i)
  (let lp ((ranges b64-byte-ranges) (out 0))
    (cond ((null? ranges)
           (error "bad base64 byte" i))
          ((and (>= i (caar ranges)) (<= i (cdar ranges)))
           (+ out (- i (caar ranges))))
          (else
           (lp (cdr ranges) (+ out (+ 1 (- (cdar ranges)
                                           (caar ranges)))))))))

(define (bytevector-pad bv n fill)
  (let ((result (make-bytevector n fill)))
    (bytevector-copy! bv 0 result 0 (bytevector-length bv))
    result))

(define-syntax bytevector-map-n-to-m
  (lambda (stx)
    (syntax-case stx ()
      ((_ n m)
       (with-syntax (((byte-arg ...)
                      (map (lambda (x)
                             #`(bytevector-u8-ref s (+ i #,x)))
                           (iota (syntax->datum #'n)))))
         #'(lambda (proc s)
             (let* ((len (bytevector-length s))
                    (out (make-bytevector (* len (/ m n)))))
               (let lp ((i 0) (j 0))
                 (cond
                  ((< i len)
                   (let inner ((k 0) (bytes (proc byte-arg ...)))
                     (if (not (null? bytes))
                         (begin (bytevector-u8-set! out (+ j k) (car bytes))
                                (inner (+ k 1) (cdr bytes)))))
                   (lp (+ i n) (+ j m)))
                  (else out))))))))))

(define bytevector-map-3-to-4
  (bytevector-map-n-to-m 3 4))
(define bytevector-map-4-to-3
  (bytevector-map-n-to-m 4 3))

(define (bytevector-fill-range! bv start end u8)
  (do ((i (- end 1) (- i 1)))
      ((< i start))
      (bytevector-u8-set! bv i u8)))

(define (bytevector-copy/padding bv npad pad-byte)
  (let ((result (bytevector-copy bv))
        (len (bytevector-length bv)))
    (bytevector-fill-range! result (- len npad) len pad-byte)
    result))

(define (base64-encode bv)
  (let* ((npad (remainder (- 3 (remainder (bytevector-length bv) 3)) 3))
         (out (bytevector-map-3-to-4
               (lambda (x y z)
                 (let ((n (logior (ash x 16) (ash y 8) z)))
                   (map int->b64-byte
                        (list (ash n -18) (ash n -12) (ash n -6) n))))
               (bytevector-pad bv (+ (bytevector-length bv) npad) 0))))
    (bytevector-fill-range! out
                            (- (bytevector-length out) npad)
                            (bytevector-length out)
                            (char->integer #\=))
    (utf8->string out)))

(define eql-byte (char->integer #\=))

(define (b64-bv-npad bv)
  (let ((len (bytevector-length bv)))
    (if (> len 0)
        (if (= (bytevector-u8-ref bv (- len 1)) eql-byte)
            (if (> len 1)
                (if (= (bytevector-u8-ref bv (- len 2)) eql-byte)
                    2
                    1)
                1)
            0)
        0)))

(define (base64-decode str)
  (let* ((bv (string->utf8 str))
         (npad (b64-bv-npad bv))
         (out (bytevector-map-4-to-3
               (lambda (w x y z)
                 (let ((n (logior (ash (b64-byte->int w) 18)
                                  (ash (b64-byte->int x) 12)
                                  (ash (b64-byte->int y) 6)
                                  (b64-byte->int z))))
                   (list (ash n -16) (logand (ash n -8) 255)
                         (logand n 255))))
               (bytevector-copy/padding bv npad (char->integer #\A))))
         (result (make-bytevector (- (bytevector-length out) npad))))
    (bytevector-copy! out 0 result 0 (bytevector-length result))
    result))
