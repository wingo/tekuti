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
  #:use-module (tekuti base64)
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
            main-nav-items related-posts-section related-tag-cloud
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
                  (nav-items '())
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
                                         #:nav-items nav-items
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

(define (post-editing-form post index)
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
            ,(show-post post index #f))
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
  `(section
    (h3 (label (@ (for "comment")) "Leave a Reply"))
    (form
     (@ (action ,(post-url post)) (method "POST"))
     (p (label (input (@ (type "text") (name "author") (value ,author)
                         (size "22")))
               " " (small "Name")))
     (p (label (input (@ (type "text") (name "email") (value ,email)
                         (size "22")))
               " " (small "Mail (will not be published)")))
     (p (label (input (@ (type "text") (name "url") (value ,url) (size "22")))
               " " (small "Website")))
     (p (label (input (@ (type "text") (name "x") (value "") (size "22")))
               " " (small "What's a number between 34 and 42?")))
                                        ;(p (small "allowed tags: "))
     (p (textarea (@ (name "comment") (id "comment") (cols "65") (rows "10"))
                  ,comment))
     (p (input (@ (name "submit") (type "submit") (id "submit")
                  (value "Submit Comment")))))))

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
           (comment-form post "" "" "" "")))))

(define (tag-link tagname)
  (rellink `("tags" ,tagname) tagname))

(define (show-post post index standalone?)
  `(article
    (h2 (@ (class "storytitle")) ,(post-link post))
    (aside (@ (class "meta"))
           (div (@ (class "post-date"))
                ,(post-readable-date post))
           (ul (@ (class "post-tags"))
               ,@(map (lambda (tag) `(li ,(tag-link tag)))
                      (post-tags post))))
    ,(post-sxml-content post)
    ,@(if standalone?
          (list (related-posts-section post index))
          '())
    ,(if standalone?
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

(define feed-icon-14x14
  #vu8(137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0 14 0 0 0 14 8 6 0 0 0 31 72 45 209 0 0 0 4 103 65 77 65 0 0 175 200 55 5 138 233 0 0 0 25 116 69 88 116 83 111 102 116 119 97 114 101 0 65 100 111 98 101 32 73 109 97 103 101 82 101 97 100 121 113 201 101 60 0 0 2 42 73 68 65 84 40 21 5 193 49 104 92 101 0 0 224 239 255 223 189 123 103 228 8 134 210 158 109 45 212 104 209 18 212 90 137 8 10 14 102 144 52 224 160 56 148 234 210 193 201 34 84 234 38 130 139 46 78 17 41 46 14 34 136 6 66 171 85 164 40 46 85 176 73 53 66 67 91 181 209 52 54 52 185 196 179 38 119 247 238 189 231 247 133 107 83 90 21 179 101 165 133 59 2 101 8 4 0 0 2 224 6 38 195 213 41 63 198 214 189 227 91 121 105 228 153 151 132 94 71 217 254 131 91 139 210 44 145 101 81 146 69 73 154 136 177 68 105 249 155 133 249 112 229 136 205 170 53 58 92 236 62 224 193 183 190 4 0 91 191 156 149 255 48 173 94 180 165 105 36 137 12 186 150 206 204 9 87 167 116 146 125 247 55 171 93 251 101 251 31 22 98 144 12 239 52 52 122 88 243 129 167 65 127 238 35 46 157 22 99 164 40 44 205 94 16 81 198 52 168 54 255 84 173 255 174 90 249 73 249 235 39 254 57 123 202 205 153 147 122 55 47 171 63 250 178 48 254 42 162 170 4 34 136 84 59 246 217 123 226 51 123 78 158 119 215 243 239 25 190 239 49 201 202 247 86 103 95 211 93 89 144 142 189 168 40 40 58 107 32 66 90 79 100 245 194 250 183 211 54 47 126 170 49 250 148 230 115 211 154 143 188 32 219 88 180 118 238 77 96 236 152 65 119 0 98 64 227 206 84 211 134 120 241 125 221 175 79 185 113 122 210 246 245 11 26 79 190 110 104 239 97 131 229 57 237 239 166 101 143 31 151 247 3 136 101 69 89 245 149 221 91 106 131 13 205 44 87 111 47 88 157 57 1 234 227 175 80 208 107 255 13 170 145 131 32 22 21 183 183 55 212 38 222 214 124 99 69 54 241 142 70 61 161 179 170 51 255 185 236 192 132 208 216 37 255 239 54 40 106 35 32 22 149 144 99 232 224 179 160 246 208 81 177 10 106 33 200 215 175 131 164 177 131 2 72 90 99 32 38 65 28 106 36 182 22 191 2 249 207 31 83 4 89 140 202 197 243 218 31 30 83 254 117 89 241 219 37 203 239 30 181 54 243 1 8 87 142 152 219 249 196 61 135 164 137 32 72 170 160 204 25 244 200 251 148 125 250 219 228 93 138 30 49 20 194 191 75 243 225 218 148 187 241 5 14 1 0 0 0 0 230 49 249 63 184 194 217 131 141 86 155 222 0 0 0 0 73 69 78 68 174 66 96 130))
(define inline-feed-icon-14x14
  (format #f "data:image/png;base64,~a" (base64-encode feed-icon-14x14)))

(define (main-nav-items request index)
  (list `(a (@ (href ,(relurl '("feed" "atom"))))
            "subscribe "
            (img (@ (src ,inline-feed-icon-14x14)
                    (alt "[feed]")
                    (width "14") (height "14"))))
        `(form (@ (method "POST")
                  (action ,(relurl '("search"))))
               (label "search "
                      (input (@ (name "string") (type "text") (size "10")
                                (value "")))))
        (rellink '("tags") "tags")
        `(div (@ (id "tag-cloud"))
              ,@(tag-cloud (top-tags index 30)))))

(define (related-posts-section post index)
  `(section (@ (class "related-posts"))
            ,@(match (take-max (compute-related-posts post index) 6)
                (() '())
                (posts
                 `((h2 "related articles")
                   (ul ,@(map (lambda (post-and-tags)
                                `(li (@ (style "margin-top: 5px"))
                                     ,(post-link (car post-and-tags))))
                              posts)))))))

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
