;; Tekuti
;; Copyright (C) 2008, 2010, 2012, 2023 Andy Wingo <wingo at pobox dot com>

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
  #:use-module (web uri)
  #:use-module (tekuti config)
  #:use-module (tekuti page-helpers)
  #:export (templatize))

(define* (templatize #:key
                     (title *title*)
                     (subtitle *subtitle*)
                     (body '((p "(missing content?)")))
                     (keywords '())
                     (nav-items '()))
  (define (href . args)
    `(href ,(string-append "/" (encode-and-join-uri-path
                                (append *public-path-base* args)))))
  `(html
    (@ (lang "en"))
    (head (title ,title)
          (meta (@ (name "generator")
                   (content "tekuti: https://wingolog.org/software/tekuti")))
          (meta (@ (name "description")
                   (content ,(format #f "~a: ~a" *title* subtitle))))
          (meta (@ (name "keywords")
                   (content ,(string-join keywords ", "))))
          (meta (@ (name "viewport")
                   (content "width=device-width, initial-scale=1.0")))
          (link (@ (rel "stylesheet")
                   (type "text/css")
                   (media "screen")
                   (href ,*css-file*)))
          (link (@ (rel "alternate")
                   (type "application/rss+xml")
                   (title ,*title*)
                   (href ,(relurl `("feed" "atom"))))))
    (body
     (header (@ (id "header"))
             (h1 (a (@ ,(href "")) ,*title*)))
     (nav (@ (id "navbar"))
          (ul ,@(map (lambda (x) `(li ,x))
                     (append (map (lambda (x)
                                    `(a (@ (href ,(cdr x))) ,(car x)))
                                  *navbar-links*)
                             nav-items))))
     (main (@ (id "content"))
           ,@body)
     (footer
      (@ (id "footer"))
      "powered by "
      (a (@ (href "//wingolog.org/software/tekuti/"))
         "tekuti")))))
