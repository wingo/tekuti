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

(define-module (tekuti template)
  #:use-module (tekuti request)
  #:use-module (tekuti config)
  #:export (templatize))

(define (templatize request)
  (define (href . args)
    `(href ,(apply string-append *public-url-base* args)))
  (define (list-join l infix)
    "Infixes @var{infix} into list @var{l}."
    (if (null? l) l
        (let lp ((in (cdr l)) (out (list (car l))))
          (cond ((null? in) (reverse out))
                (else (lp (cdr in) (cons* (car in) infix out)))))))
  (define (make-navbar)
    `(div (@ (id "navbar"))
          ,@(list-join
             (map (lambda (x) `(a (@ ,(href x "/")) ,x))
                  '("about" "software" "writings" "photos"))
             " | ")))
  `(html
    (head (title ,(rref request 'title *title*))
          (meta (@ (name "Generator")
                   (content "An unholy concoction of parenthetical guile")))
          (link (@ (rel "stylesheet")
                   (type "text/css")
                   (media "screen")
                   (href "/base.css")))) ;fixme
    (body
     (div (@ (id "rap"))
          (h1 (@ (id "header"))
              (a (@ ,(href "")) ,*title*))
          ,(make-navbar)
          (div (@ (id "content"))
               ,@(rref request 'body '((p "(missing content?)"))))
          (div (@ (id "footer"))
               "powered by "
               (a (@ (href "http://wingolog.org/software/tekuti/"))
                  "parentheses"))))))
