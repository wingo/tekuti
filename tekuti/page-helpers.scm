;; Tekuti
;; Copyright (C) 2008, 2010, 2012, 2014 Andy Wingo <wingo at pobox dot com>

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
  #:use-module (sxml simple)
  #:use-module (web uri)
  #:use-module (web http)
  #:use-module (web response)
  #:use-module (tekuti config)
  #:use-module (tekuti util)
  #:use-module (tekuti git)
  #:use-module (tekuti post)
  #:use-module (tekuti tags)
  #:use-module (tekuti comment)
  #:use-module (tekuti request)
  #:use-module (tekuti template)
  #:use-module (srfi srfi-19)
  #:export (respond
            relurl rellink
            post-url
            post-editing-form
            sidebar-ul top-tags tag-cloud
            main-sidebar post-sidebar related-tag-cloud
            post-link admin-post-url admin-post-link
            show-post with-authentication
            find-posts-matching
            atom-header atom-entry))

(define xhtml-doctype
  (string-append
   "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" "
   "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"))

(define-syntax build-headers
  (syntax-rules ()
    ((_ k v-exp rest ...)
     (let ((v v-exp))
       (let ((tail (build-headers rest ...)))
         (if v
             (acons 'k v tail)
             tail))))
    ((_ tail)
     tail)))

(define (ensure-uri x)
  (cond
   ((uri? x) x)
   ((string? x)
    (build-uri *public-scheme* #:host *public-host* #:port *public-port*
               #:path x))
   ((list? x)
    (ensure-uri (relurl x)))
   (else (error "can't turn into a uri" x))))

(define (ensure-uri-reference x)
  (cond
   ((uri? x) x)
   ((string? x)
    (if (defined? 'build-uri-reference)
        (build-uri-reference #:path x)
        ;; Absolute URIs on older Guile.
        (ensure-uri x)))
   ((list? x)
    (ensure-uri-reference (relurl x)))
   (else (error "can't turn into a uri" x))))

(define* (respond #:optional body #:key
                  redirect
                  (status (if redirect 302 200))
                  (title *title*)
                  last-modified
                  etag
                  (doctype xhtml-doctype)
                  (content-type-params '((charset . "utf-8")))
                  (content-type 'text/html)
                  (extra-headers '())
                  (sxml (and body (templatize #:title title #:body body))))
  (values (build-response
           #:code status
           #:headers (build-headers
                      location (and=> redirect ensure-uri-reference)
                      last-modified last-modified
                      content-type (cons content-type content-type-params)
                      date (current-date)
                      etag (if (string? etag) (cons etag #t) etag)
                      extra-headers))
          (and sxml
               (lambda (port)
                 (if doctype (display doctype port))
                 (sxml->xml sxml port)))))

(define (unparse-www-form-urlencoded alist)
  (string-join (map (lambda (pair)
                      (if (cdr pair)
                          (string-append (uri-encode (car pair))
                                         "="
                                         (uri-encode (cdr pair)))
                          (uri-encode (car pair))))
                    alist)
               "&"))

(define* (relative-url uri path-components #:key query fragment)
  (uri->string
   (build-uri (uri-scheme uri)
              #:userinfo (uri-userinfo uri) #:host (uri-host uri)
              #:port (uri-port uri)
              #:path (encode-and-join-uri-path
                      (append (split-and-decode-uri-path (uri-path uri))
                              path-components))
              #:query (and=> query unparse-www-form-urlencoded)
              #:fragment fragment)))

(define* (relative-link uri path-components text #:key query fragment)
  `(a (@ (href ,(relative-url uri path-components #:query query
                              #:fragment fragment)))
      ,@text))

(define* (relative-path base path-components #:key query fragment)
  (let ((path (encode-and-join-uri-path (append base path-components)))
        (query (and=> query unparse-www-form-urlencoded)))
    (if query
        (if fragment
            (string-append "/" path "?" query "#" fragment)
            (string-append "/" path "?" query))
        (if fragment
            (string-append "/" path "#" fragment)
            (string-append "/" path)))))

(define* (relative-path-link base path-components text #:key query fragment)
  `(a (@ (href ,(relative-path base path-components #:query query
                               #:fragment fragment)))
      ,text))

(define* (relurl path-components #:key query fragment)
  (relative-path *public-path-base* path-components #:query query
                 #:fragment fragment))

(define* (rellink path-components text #:key query fragment)
  (relative-path-link *public-path-base* path-components text #:query query
                      #:fragment fragment))

(define (post-editing-form post)
  `(div
    (form (@ (method "POST")
             (action ,(if post
                          (relurl `("admin" "modify-post" ,(post-key post)))
                          (relurl '("admin" "new-post")))))
          (p (input (@ (name "title") (type "text")
                       (value ,(if post (post-title post) ""))))
             (label (@ (for "title")) " <- title"))
          (p (input (@ (name "tags") (type "text")
                       (value ,(if post
                                   (string-join (post-tags post) ", ")
                                   ""))))
             (label (@ (for "tags")) " <- tags, comma-separated"))
          (p (input (@ (name "date") (type "text")
                       (value ,(if (and=> post post-public?)
                                   (timestamp->rfc822-date (post-timestamp post))
                                   ""))))
             (label (@ (for "date")) " <- date (empty == now)"))
          (p (input (@ (name "comments") (type "checkbox")
                       ,@(if (or (not post) (post-comments-open? post))
                             `((checked "checked")) '())))
             (label (@ (for "comments")) " comments open?"))
          (div (textarea (@ (name "body") (rows "20") (cols "60"))
                         ,(if post (post-raw-content post) "")))
          (p (label (input (@ (type "radio") (name "status") (value "private")
                              ,@(if (or (not post) (post-private? post))
                                    '((checked "checked"))
                                    '())))
                    "private (only visible to admin)") (br)
             (label (input (@ (type "radio") (name "status") (value "draft")
                              ,@(if (and post (post-draft? post))
                                    '((checked "checked"))
                                    '())))
                    "draft (only accessible via "
                    ,(if post
                         `(a (@ (href ,(post-url post)))
                             ,(post-url post))
                         "direct link")
                    ")")

             (br)
             (label (input (@ (type "radio") (name "status") (value "publish")
                              ,@(if (and post (post-public? post))
                                    '((checked "checked"))
                                    '())))
                    "public"))
          (p (input (@ (type "submit")
                       (value ,(if post "modify post" "new post"))))))
    ,@(if post
          `((form (@ (method "POST")
                     (action ,(relurl `("admin" "delete-post" ,(post-key post)))))
                  (p (input (@ (type "submit") (value "delete post")))))
            ,@(let ((l (comments-sxml-content-edit post)))
                (if (null? l) l
                    `((h2 "comments")
                      (ol (@ (class "commentlist")) ,@l))))
            (h2 "preview")
            ,(show-post post #f))
          '())))

(define (sidebar-ul body)
  `(div (@ (id "menu"))
        (ul ,@body)))

(define (admin-post-url post)
  (relurl `("admin" "posts" ,(post-key post))))

(define (admin-post-link post)
  `(a (@ (href ,(admin-post-url post))) ,(post-title post)))

(define* (post-url post #:key fragment)
  (relative-path *public-path-base*
                 (cons "archives"
                       (split-and-decode-uri-path (uri-decode (post-key post))))
                 #:fragment fragment))

(define* (post-link post #:key fragment)
  `(a (@ (href ,(post-url post #:fragment fragment)))
      ,(post-title post)))

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
       " " (label (@ (for "x")) (small "What's a number between 34 and 42?")))
    ;(p (small "allowed tags: "))
    (p (textarea (@ (name "comment") (id "comment") (cols "65")
                    (rows "10") (tabindex "4"))
                 ,comment))
    (p (input (@ (name "submit") (type "submit") (id "submit") (tabindex "5")
                 (value "Submit Comment"))))))

(define (comments-sxml-content-edit post)
  (map
   (lambda (comment)
     (let ((id (assq-ref comment 'key)))
       `(,(comment-sxml-content comment)
         (form (@ (method "POST")
                   (action ,(relurl `("admin" "delete-comment"
                                      ,(post-key post) ,id))))
                (input (@ (type "submit") (name "delete") (value "delete"))))
         (br))))
   (post-comments post)))

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
  (rellink `("tags" ,tagname) tagname))

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
                      (a (@ (href ,(post-url post #:fragment "comments")))
                         "(" ,(post-n-comments post) ")")))))
    ,@(if comments?
          (list (post-sxml-comments post))
          '())))

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
          `(a (@ (href ,(relurl `("tags" ,name)))
                 (rel "tag")
                 (style ,(format #f "font-size: ~d%" size)))
              ,name))
        (map car tags)
        (determine-sizes (map cdr tags)))
   " "))

(define (main-sidebar request index)
  (sidebar-ul
   `((li (h2 (a (@ (href ,(relurl '("feed" "atom"))))
                "subscribe "
                (img (@ (src ,(relurl '("wp-content" "feed-icon-14x14.png")))
                        (alt "[feed]")))
                )))
     (li (h2 "search")
         (form (@ (method "POST")
                  (action ,(relurl '("search"))))
               (input (@ (name "string") (type "text") (size "15")
                         (value "")))))
     (li (h2 "tags " ,(rellink '("tags") ">>"))
         (ul (li (@ (style "line-height: 150%"))
                 ,@(tag-cloud (top-tags index 30))))))))

(define (post-sidebar post index)
  (sidebar-ul
   `((li (h2 (a (@ (href ,(relurl '("feed" "atom"))))
                "subscribe "
                (img (@ (src ,(relurl '("wp-content" "feed-icon-14x14.png")))
                        (alt "[feed]")))
                )))
     (li (h2 "related")
         (ul ,@(map (lambda (post-and-tags)
                      `(li (@ (style "margin-top: 5px"))
                           ,(post-link (car post-and-tags))))
                    (take-max (compute-related-posts post index) 10)))))))

(define (related-tag-cloud tag index)
  `(div (@ (id "tag-cloud"))
        (h2 "related tags")
        ,@(tag-cloud (compute-related-tags tag index))))

(define (find-posts-matching string index)
  (let ((master (assq-ref index 'master)))
    (dsu-sort
     (filter
      identity
      (match-lines (or (false-if-git-error
                        ;; dunno why git errors sometimes here...
                        (git "grep" "-l" "-F" string master "--" "*/content"))
                       "")
                   ":(.+)/content$" (_ key)
                   (post-from-key index key)))
     post-timestamp
     >)))

(define (with-authentication request thunk)
  (if (request-authenticated? request)
      (thunk)
      (let ((header (parse-header 'www-authenticate "Basic realm=\"Tekuti\"")))
        (respond `((p "Authentication required, yo"))
                 #:status 401
                 #:extra-headers `((www-authenticate . ,header))))))

(define (atom-header last-modified)
  (define (relurl . tail)
    (uri->string (ensure-uri tail)))
  `(feed
     (@ (xmlns "http://www.w3.org/2005/Atom") (xml:base ,(relurl)))
     (title (@ (type "text")) ,*title*)
     (subtitle (@ (type "text")) ,*subtitle*)
     ,@(if last-modified
           `((updated ,(timestamp->atom-date last-modified)))
           '())
     (generator (@ (uri "http://wingolog.org/software/tekuti")
                   (version "what"))
                "tekuti")
     (link (@ (rel "alternate") (type "text/html")
              (href ,(relurl))))
     (id ,(relurl "feed" "atom"))
     (link (@ (rel "self") (type "application/atom+xml")
              (href ,(relurl "feed" "atom"))))))

(define (atom-entry post)
  (define (relurl . tail)
    (uri->string (ensure-uri tail)))
  `(entry
    (author (name ,*name*) (uri ,(relurl)))
    (title (@ (type "text")) ,(post-title post))
    (id ,(apply relurl
                ;; hack -- should include archives...
                (split-and-decode-uri-path (uri-decode (post-key post)))))
    (link (@ (rel "alternate") (type "text/html")
             (href ,(apply relurl "archives" (split-and-decode-uri-path
                                              (uri-decode (post-key post)))))))
    (published ,(timestamp->atom-date (post-timestamp post)))
    (updated ,(timestamp->atom-date (post-timestamp post)))
    (content (@ (type "xhtml"))
             (div (@ (xmlns "http://www.w3.org/1999/xhtml"))
                  ,(post-sxml-content post)))))
