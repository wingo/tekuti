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
;; Helper bits, mostly verbose-like SXML stuff.
;;
;;; Code:

(define-module (tekuti page-helpers)
  #:use-module (tekuti config)
  #:use-module (tekuti util)
  #:use-module (tekuti git)
  #:use-module (tekuti post)
  #:use-module (tekuti comment)
  #:use-module (tekuti url)
  #:use-module (tekuti request)
  #:use-module (srfi srfi-19)
  #:use-module (scheme kwargs)
  #:export (relurl rellink redirect post-url
            published-posts
            post-editing-form
            sidebar-ul main-sidebar tag-cloud
            post-link admin-post-link admin-post-redirect
            show-post with-authentication
            atom-header atom-entry))

(define (relurl . paths)
  (apply string-append *public-url-base* paths))

(define (rellink path . body)
  `(a (@ (href ,(relurl path)))
      ,@body))

(define (published-posts index n)
  (filter-mapn (lambda (post)
                 (and (post-published? post) post))
               (assq-ref index 'posts)
               n))

(define (post-editing-form post)
  `(div
    (form (@ (method "POST")
             (action ,(relurl (if post
                                  (string-append "admin/modify-post/"
                                                 (url:encode (post-key post)))
                                  "admin/new-post"))))
          (p (input (@ (name "title") (type "text")
                       (value ,(if post (post-title post) ""))))
             (label (@ (for "title")) " <- title"))
          (p (input (@ (name "tags") (type "text")
                       (value ,(if post
                                   (string-join (post-tags post) ", ")
                                   ""))))
             (label (@ (for "tags")) " <- tags, comma-separated"))
          (p (input (@ (name "date") (type "text")
                       (value ,(if (and=> post post-published?)
                                   (timestamp->rfc822-date (post-timestamp post))
                                   ""))))
             (label (@ (for "date")) " <- date (empty == now)"))
          (p (input (@ (name "comments") (type "checkbox")
                       ,@(if (or (not post) (post-comments-open? post))
                             `((checked "checked")) '())))
             (label (@ (for "comments")) " comments open?"))
          (div (textarea (@ (name "body") (rows "20") (cols "60"))
                         ,(if post (post-raw-content post) "")))
          (input (@ (type "submit") (name "status")
                    (value "publish")))
          " "
          (input (@ (type "submit") (name "status")
                    (value "draft"))))
    ,@(if post
          `((h2 "preview")
            ,(show-post post #f))
          '())))

(define (sidebar-ul body)
  `(div (@ (id "menu"))
        (ul ,@body)))

;; double-encoding is a hack to trick apache
(define (admin-post-url post)
  (relurl "admin/posts/" (url:encode (post-key post))))

(define (admin-post-link post)
  `(a (@ (href ,(admin-post-url post))) ,(post-title post)))

(define (post-url post . tail)
  (apply relurl "archives/" (url:decode (post-key post)) tail))

(define (post-link post . tail)
  `(a (@ (href ,(apply post-url post tail))) ,(post-title post)))

(define (comment-form post author email url comment)
  `(form
    (@ (action ,(post-url post)) (method "POST"))
    (p (input (@ (type "text") (name "author") (value ,author)
                 (size "22") (tabindex "1")))
       " " (label (@ (for "author")) (small "Name")))
    (p (input (@ (type "text") (name "email") (value ,email)
                 (size "22") (tabindex "2")))
       " " (label (@ (for "email")) (small "Mail (will not be published)")))
    (p (input (@ (type "text") (name "url") (value ,url)
                 (size "22") (tabindex "3")))
       " " (label (@ (for "url")) (small "Website")))
    (p (input (@ (type "text") (name "x") (value "")
                 (size "22") (tabindex "3")))
       " " (label (@ (for "x")) (small "What's your favorite number?")))
    ;(p (small "allowed tags: "))
    (p (textarea (@ (name "comment") (id "comment") (cols "65")
                    (rows "10") (tabindex "4"))
                 ,comment))
    (p (input (@ (name "submit") (type "submit") (id "submit") (tabindex "5")
                 (value "Submit Comment"))))))

(define (post-sxml-comments post)
  (let ((comments (post-comments post))
        (comments-open? (post-comments-open? post)))
    (define (n-comments-header)
      (and (or (not (null? comments)) comments-open?)
           `(h3 (@ (id "comments"))
                ,(let ((len (length comments)))
                   (case len
                     ((0) "No responses")
                     ((1) "One response")
                     (else (format #f "~d responses" len)))))))
    `(div
      ,@(or (and=> (n-comments-header) list) '())
      ,@(let ((l (map comment-sxml-content comments)))
          (if (null? l) l
              `((ol (@ (class "commentlist")) ,@l))))
      ,(if (not comments-open?)
           `(p (@ (id "nocomments")) "Comments are closed.")
           `(div (h3 "Leave a Reply")
                 ,(comment-form post "" "" "" ""))))))

(define (tag-link tagname)
  (rellink (string-append "tags/" (url:encode tagname))
           tagname))

(define (show-post post comments?)
  `((h2 (@ (class "storytitle"))
        ,(post-link post))
    (div (@ (class "post"))
         (h3 (@ (class "meta"))
             ,(post-readable-date post)
             " (" ,@(list-intersperse
                     (map tag-link (post-tags post))
                     " | ")
             ")")
         (div (@ (class "storycontent"))
              ,(post-sxml-content post))
         ,@(if comments?
               '()
               `((div (@ (class "feedback"))
                      (a (@ (href ,(post-url post "#comments")))
                         "(" ,(post-n-comments post) ")")))))
    ,@(if comments?
          (list (post-sxml-comments post))
          '())))

(define (redirect request location)
  (rpush 'output-headers (cons "Location" location)
         (rcons 'status 302 request)))

(define (admin-post-redirect request post)
  (redirect request (admin-post-url post)))

(define (top-tags index n)
  (let ((hash (assq-ref index 'tags)))
    (if hash
        (dsu-sort
         (take-max
          (dsu-sort         
           (hash-fold (lambda (k v seed) (acons k (length v) seed))
                      '() hash)
           cdr >) n)
         car string<?)
        '())))

(define (tag-cloud tags)
  (define (determine-sizes counts)
    (let ((maxcount (if (null? counts) '() (apply max counts))))
      (map (lambda (x)
             (floor (+ 80 (* 120 (/ x maxcount)))))
           counts)))
  (list-intersperse
   (map (lambda (name size)
          `(a (@ (href ,(relurl "tags/" (url:encode name)))
                 (rel "tag")
                 (style ,(format #f "font-size: ~d%" size)))
              ,name))
        (map car tags)
        (determine-sizes (map cdr tags)))
   " "))

(define (main-sidebar request index)
  (sidebar-ul
   `((li (h2 (a (@ (href ,(relurl "feed/atom")))
                "subscribe "
                (img (@ (src ,(relurl "wp-content/feed-icon-14x14.png"))
                        (alt "[feed]")))
                )))
     (li (h2 "tags " ,(rellink "tags/" ">>"))
         (ul (li (@ (style "line-height: 150%"))
                 ,@(tag-cloud (top-tags index 30))))))))

(define (with-authentication request thunk)
  (if (request-authenticated? request)
      (thunk)
      (rcons* (rpush 'output-headers
                     '("WWW-Authenticate" . "Basic realm=\"Tekuti\"")
                     request)
              'status 401
              'body `((p "Authentication required, yo")))))

(define (atom-header server-name last-modified)
  (define (relurl tail)
    (string-append "http://" server-name *public-url-base* tail))
  `(feed
     (@ (xmlns "http://www.w3.org/2005/Atom") (xml:base ,(relurl "")))
     (title (@ (type "text")) ,*title*)
     (subtitle (@ (type "text")) ,*subtitle*)
     (updated ,(timestamp->atom-date last-modified))
     (generator (@ (uri "http://wingolog.org/software/tekuti")
                   (version "what"))
                "tekuti")
     (link (@ (rel "alternate") (type "text/html")
              (href ,(relurl ""))))
     (id ,(relurl "feed/atom"))
     (link (@ (rel "self") (type "application/atom+xml")
              (href ,(relurl "feed/atom"))))))

(define (atom-entry server-name post)
  (define (relurl . tail)
    (apply string-append "http://" server-name *public-url-base* tail))
  `(entry
    (author (name ,*name*) (uri ,(relurl "")))
    (title (@ (type "text")) ,(post-title post))
    (id ,(relurl (url:decode (post-key post)))) ;hack -- should include archives...
    (link (@ (rel "alternate") (type "text/html")
             (href ,(relurl "archives/" (url:decode (post-key post))))))
    (published ,(timestamp->atom-date (post-timestamp post)))
    (updated ,(timestamp->atom-date (post-timestamp post)))
    (content (@ (type "xhtml"))
             (div (@ (xmlns "http://www.w3.org/1999/xhtml"))
                  ,(post-sxml-content post)))))
