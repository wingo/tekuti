;; Tekuti
;; Copyright (C) 2008, 2010, 2012, 2014, 2019, 2021, 2023 Andy Wingo <wingo at pobox dot com>

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
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
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
  #:use-module ((srfi srfi-1) #:select (append-map))
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

(define html-doctype "<!doctype html>\n")

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

(define-syntax-rule (define-tag-set pred tag ...)
  (define pred
    (let ((set (make-hash-table)))
      (hashq-set! set 'tag #t) ...
      (lambda (t) (hashq-ref set t)))))

(define-tag-set void-element?
  area base br col embed hr img input link meta param source track wbr)
(define-tag-set template-element?
  template)
(define-tag-set raw-text-element?
  script style)
(define-tag-set escapable-raw-text-element?
  textarea title)
(define (foreign-element? tag)
  (string-index (symbol->string tag) #\:))
;; Otherwise it's a normal element.

(define (make-char-quotator char-encoding)
  (let ((bad-chars (list->char-set (map car char-encoding))))

    ;; Check to see if str contains one of the characters in charset,
    ;; from the position i onward. If so, return that character's index.
    ;; otherwise, return #f
    (define (index-cset str i charset)
      (string-index str charset i))

    ;; The body of the function
    (lambda (str port)
      (let ((bad-pos (index-cset str 0 bad-chars)))
        (if (not bad-pos)
            (display str port)          ; str had all good chars
            (let loop ((from 0) (to bad-pos))
              (cond
               ((>= from (string-length str)) *unspecified*)
               ((not to)
                (display (substring str from (string-length str)) port))
               (else
                (let ((quoted-char
                       (cdr (assv (string-ref str to) char-encoding)))
                      (new-to
                       (index-cset str (+ 1 to) bad-chars)))
                  (if (< from to)
                      (display (substring str from to) port))
                  (display quoted-char port)
                  (loop (1+ to) new-to))))))))))

(define (attribute-value-empty? value)
  (string-null? value))

(define attribute-value-needs-quotes-chars
  (char-set-union (string->char-set "\"'=<>`") char-set:whitespace))
(define (attribute-value-needs-quotes? value)
  (or (string-null? value)
      (string-index value attribute-value-needs-quotes-chars)))

(define print-attribute-value/quoted
  (make-char-quotator
   '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;"))))

(define print-text/quoted
  (make-char-quotator
   '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;"))))

(define* (shtml->html tree #:optional (port (current-output-port)))
  "Serialize the shtml tree @var{tree} as HTML. The output will be written
to the current output port, unless the optional argument @var{port} is
present."
  (define (attribute->html attr value)
    (display attr port)
    (unless (attribute-value-empty? value)
      (display #\= port)
      (cond
       ((attribute-value-needs-quotes? value)
        (display #\" port)
        (print-attribute-value/quoted value port)
        (display #\" port))
       (else
        (display value port)))))

  (define (element->html tag attrs body)
    (display #\< port)
    (display tag port)
    (let lp ((attrs attrs))
      (match attrs
        (() #t)
        ((((? symbol? attr) val) . attrs)
         (display #\space port)
         (attribute->html attr val)
         (lp attrs))))

    (cond
     ((and (null? body) (foreign-element? tag))
      (display " />" port))
     ((void-element? tag)
      (unless (null? body) (error "unexpected body for void element"))
      (display #\> port))
     (else
      (display #\> port)
      (cond
       ((raw-text-element? tag)
        (let ((body (string-concatenate body)))
          (let ((needle (string-append "</" (symbol->string tag))))
            (let lp ((idx 0))
              (let ((idx (string-contains-ci body needle idx)))
                (when idx
                  (let ((idx (+ idx (string-length needle))))
                    (let ((ch (and (< idx (string-length body))
                                   (string-ref body idx))))
                      (when (and ch (string-index "\t\n\f\r >/" ch))
                        (error "raw text element body contains end tag"
                               needle body)))
                    (lp idx))))))
          (display body port)))
       ((escapable-raw-text-element? tag)
        (for-each
         (lambda (str)
           (unless (string? str)
             (error "bad escapable raw text content" str))
           (print-text/quoted str port))
         body))
       (else
        (for-each ->html body)))
      (display "</" port)
      (display tag port)
      (display ">" port))))

  (define (->html tree)
    (match tree
      (((? symbol? tag) ('@ . attrs) . body)
       (element->html tag attrs body))
      (((? symbol? tag) . body)
       (element->html tag '() body))
      ((_ . _)
       (error "nodelists unsupported" tree))
      ((or #f #t ()) #f)
      ((? string?)
       (print-text/quoted tree port))
      ((? procedure?)
       (with-output-to-port port tree))
      ((? number?)
       (display tree port))
      (tree
       (error "unexpected shtml" tree))))

  (match tree
    (('html . _)
     (->html tree))))

(define* (respond #:optional body #:key
                  redirect
                  (status (if redirect 302 200))
                  (title *title*)
                  (subtitle *subtitle*)
                  (keywords '())
                  last-modified
                  etag
                  (doctype html-doctype)
                  (content-type-params '((charset . "utf-8")))
                  (content-type 'text/html)
                  (extra-headers '())
                  (serialize
                   (match content-type
                     ('text/html shtml->html)
                     ('application/atom+xml sxml->xml)))
                  (sxml (and body
                             (templatize #:title title
                                         #:subtitle subtitle
                                         #:keywords keywords
                                         #:body body))))
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
                 (serialize sxml port)))))

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
  `(section
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
          (p (input (@ (name "comments-closed-date") (type "text")
                       (value ,(or (and=> (and=> post
                                                 post-comments-closed-timestamp)
                                          timestamp->rfc822-date)
                                   ""))))
             (label (@ (for "comments-closed-date"))
                    " <- close comments on date (empty == in "
                    ,(floor/ *comments-open-window* (* 24 60 60))
                    " days)"))
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
                    `((section
                       (h2 "comments")
                       (ol (@ (class "commentlist")) ,@l)))))
            (h2 "preview")
            ,(show-post post #f))
          '())))

(define (sidebar-ul body)
  `(nav (@ (id "menu"))
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
  (append-map
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
    `(section
      ,@(or (and=> (n-comments-header) list) '())
      ,@(let ((l (map comment-sxml-content comments)))
          (if (null? l) l
              `((ol (@ (class "commentlist")) ,@l))))
      ,(if (not comments-open?)
           `(p (@ (id "nocomments")) "Comments are closed.")
           `(section
             (h3 "Leave a Reply")
             ,(comment-form post "" "" "" ""))))))

(define (tag-link tagname)
  (rellink `("tags" ,tagname) tagname))

(define (show-post post comments?)
  `(article
    (header
     (h2 (@ (class "storytitle"))
         ,(post-link post))
     (h3 (@ (class "meta"))
         ,(post-readable-date post)
         " (" ,@(list-intersperse
                 (map tag-link (post-tags post))
                 " | ")")"))
    ,(post-sxml-content post)
    ,(if comments?
         (post-sxml-comments post)
         `(footer
           (@ (class "feedback"))
           (a (@ (href ,(post-url post #:fragment "comments")))
              "(" ,(post-n-comments post) ")")))))

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
