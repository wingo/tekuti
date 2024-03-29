;; Tekuti
;; Copyright (C) 2008, 2010, 2011, 2012, 2019, 2021, 2023 Andy Wingo <wingo at pobox dot com>

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

(define-module (tekuti page)
  #:use-module (ice-9 match)
  #:use-module (tekuti config)
  #:use-module (tekuti util)
  #:use-module (tekuti git)
  #:use-module (tekuti post)
  #:use-module (tekuti comment)
  #:use-module (tekuti classifier)
  #:use-module (web uri)
  #:use-module (web request)
  #:use-module (tekuti request)
  #:use-module (tekuti page-helpers)
  #:use-module ((srfi srfi-1) #:select (fold append-map))
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-19)
  #:export (page-admin
            page-admin-posts
            page-admin-post
            page-admin-new-post
            page-admin-modify-post
            page-admin-delete-post
            page-admin-delete-comment
            page-admin-changes
            page-admin-change
            page-admin-revert-change
            page-index
            page-show-post
            page-show-static
            page-new-comment
            page-archives
            page-show-tags
            page-show-tag
            page-debug
            page-search
            page-feed-atom
            page-debug
            page-not-found))

;; Encoded twice, so as to form a single path-component.
(define (make-post-key . parts)
  (string-downcase (uri-encode (encode-and-join-uri-path parts))))

(define (not-implemented request . args)
  (respond `((section
              (h2 "Not yet implemented")
              (p "Path handler not yet implemented: "
                 ,(request-relative-path-str request))))
           #:status 500))

(define (page-admin request body index)
  (with-authentication
   request
   (lambda ()
     ;; here we need to be giving a dashboard view instead of this
     (define (post-links n)
       (map (lambda (post)
              `(li ,(admin-post-link post)))
            (latest-posts index #:allow-unpublished? #t #:limit n)))
     (define (recent-changes n)
       (map (lambda (rev)
              `(li ,(rellink `("admin" "changes" ,(car rev))
                             (caddr rev))))
            (git-rev-list "refs/heads/master" n)))
     (respond `((section
                 (h2 "new post")
                 (aside (@ (class "meta"))
                        (h3 "posts " ,(rellink '("admin" "posts") ">>"))
                        (ul ,@(post-links 5))
                        (h3 "changes" ,(rellink '("admin" "changes") ">>"))
                        (ul ,@(recent-changes 5)))
                 ,(post-editing-form #f index)))))))

(define (page-admin-posts request body index)
  (with-authentication
   request
   (lambda ()
     (define (post-headers)
       (map (lambda (post)
              `(h3 ,(admin-post-link post)))
            (latest-posts index #:allow-unpublished? #t #:limit -1)))
     (respond `((section
                 (h2 "all your posts are belong to tekuti")
                 ,@(post-headers)))))))

(define (page-admin-post request body index key)
  (with-authentication
   request
   (lambda ()
     (let ((post (post-from-key index key #:allow-unpublished? #t)))
       (respond `((section
                   (h2 ,(post-title post))
                   ,(post-editing-form post index))))))))

(define (page-admin-new-post request body index)
  (with-authentication
   request
   (lambda ()
     (let ((post (make-new-post (request-form-data request body))))
       (respond `((section (h2 "redirecting...")))
                #:redirect (admin-post-url post))))))

(define (page-admin-modify-post request body index key)
  (with-authentication
   request
   (lambda ()
     (let ((post (modify-post key (request-form-data request body))))
       (respond `((section (h2 "redirecting...")))
                #:redirect (admin-post-url post))))))

(define (page-admin-delete-post request body index key)
  (with-authentication
   request
   (lambda ()
     (delete-post (post-from-key index key #:allow-unpublished? #t))
     (respond `((section (h2 "redirecting...")))
              #:redirect (relurl `("admin"))))))

(define (page-admin-delete-comment request body index key comment-id)
  (with-authentication
   request
   (lambda ()
     (let ((post (post-from-key index key #:allow-unpublished? #t)))
       (delete-comment post comment-id)
       (respond `((section (h2 "redirecting...")))
                #:redirect (admin-post-url post))))))

(define (page-admin-changes request body index)
  (with-authentication
   request
   (lambda ()
     (let ((revs (git-rev-list (request-query-ref request "start"
                                                  "refs/heads/master")
                               10)))
       (respond `((section
                   (h2 "recent changes")
                   ,@(map (lambda (rev)
                            `(div (h3 ,(rellink `("admin" "changes" ,(car rev))
                                                (caddr rev)))
                                  ,(timestamp->rfc822-date (cadr rev))))
                          revs)
                   (h3 ,(rellink '("admin" "changes")
                                 "more"
                                 #:query
                                 `(("start" . ,(caar (last-pair revs)))))))))))))

(define (page-admin-change request body index sha1)
  (with-authentication
   request
   (lambda ()
     (let ((commit (parse-commit sha1)))
       (respond
        `((section
           (h2 ,(assq-ref commit 'message))
           (p "Committed on "
              ,(timestamp->rfc822-date
                ;; needlessly goes to git again...
                (commit-utc-timestamp sha1)))
           (pre ,(git "diff-tree" "-M" "-p" sha1))
           (form (@ (action ,(relurl `("admin" "revert-change" ,sha1)))
                    (method "POST"))
                 (input (@ (type "submit") (value "Undo this change")))))))))))


(define (page-admin-revert-change request body index sha1)
  (with-authentication
   request
   (lambda ()
     (let ((new-master (git-revert "refs/heads/master" sha1)))
       (respond `((section (h2 "Change reverted")))
                #:redirect (relurl '("admin")))))))

(define (page-index request body index)
  (respond (map (lambda (post) (show-post post index #f))
                (latest-posts index #:limit 10))
           #:etag (assq-ref index 'master)
           #:title *title*
           #:subtitle *subtitle*
           #:keywords (map car (top-tags index 10))
           #:nav-items (main-nav-items request index)))

(define (page-show-post request body index year month day post)
  (cond
   ((post-from-key index (make-post-key year month day post)
                   #:allow-draft? #t)
    => (lambda (post)
         (define title
           (format #f "~a — ~a" (post-title post) *title*))
         (define subtitle
           (format #f "article: ~a" (post-title post)))
         (respond (list (show-post post index #t))
                  #:title title
                  #:etag (assq-ref index 'master)
                  #:subtitle subtitle
                  #:keywords (post-tags post)
                  #:nav-items (main-nav-items request index))))
   (else
    (page-not-found request body index))))

(define (page-show-static request body index)
  (cond
   ((static-post-from-key index
                          (apply make-post-key (request-relative-path request)))
    => (lambda (post)
         (define title
           (format #f "~a — ~a" (post-title post) *title*))
         (respond (list (show-static-post post index))
                  #:title title
                  #:etag (assq-ref index 'master)
                  #:subtitle (post-title post)
                  #:keywords (post-tags post)
                  #:nav-items (main-nav-items request index))))
   (else
    (page-not-found request body index))))

(define (page-new-comment request body index year month day name)
  (let ((data (request-form-data request body)))
    (cond
     ((post-from-key index (make-post-key year month day name))
      => (lambda (post)
           (let ((comment (parse-new-comment data)))
             (cond
              ((not (post-comments-open? post))
               (respond `((section
                           (h2 "Comments closed")
                           (p "Comments on this post are closed.")))))
              ((bad-new-comment-post? data)
               => (lambda (reason)
                    (respond
                     `((section
                        (h2 "Error processing comment")
                        (p "Bad post data: " ,(pk reason)))))))
              ((comment-is-bogus? index comment)
               (respond `((section
                           (h2 "Error processing comment")
                           (p "Comment appears to be bogus; ignoring.")
                           (p "I'm testing out a new automated bogus "
                              "comment detector.  If you feel your comment "
                              "was caught unfairly, tweet it to me or send "
                              "it by email.  Or press back and reword it.")
                           (p "If you are a spammer, note that I fixed "
                              "the comment renderer to properly add "
                              (tt "rel='external nofollow'") " on all "
                              "links in comments.  Go take a look at any "
                              "comment with a link to see for yourself.  "
                              "Trying to linkbomb this site probably won't "
                              "give you any link juice so it's not worth "
                              "the trouble to either one of us :)")))))
              (else
               (make-new-comment (post-key post) (post-title post) comment)
               ;; nb: at this point, `post' is out-of-date
               (respond `((section
                           (h2 "Comment posted")
                           (p "Thanks.")))
                        #:redirect (post-url post #:fragment "comments")
                        #:title "comment posted"))))))
     (else
      (page-not-found request body index)))))

;; fixme exception handling for input
(define (page-archives request body index year month day)
  (let ((year (and=> year string->number))
        (month (and=> month string->number))
        (day (and=> day string->number)))
    (let ((start (make-date 0 0 0 0 (or day 1) (or month 1) (or year 1980) 0)))
      (define too-early?
        (compose1 (date-before? start) post-timestamp))
      (define early-enough?
        (if year
            (compose1 (date-before?
                       (cond (day (date-increment start #:day 1))
                             (month (date-increment start #:month 1))
                             (else (date-increment start #:year 1))))
                      post-timestamp)
            (lambda (post) #t)))
      (define (make-date-header post)
        (lambda (x) #f))
      (define subtitle
        (cond
         (day (format #f "articles written in ~A/~A/~A." year month day))
         (month (format #f "articles written in ~A/~A." year month))
         (year (format #f "articles written in ~A." year))
         (else (format #f "all articles."))))

      (let lp ((posts (latest-posts index #:limit -1)))
        (cond ((or (null? posts) (too-early? (car posts)))
               (respond `((section
                           (h2 "No posts found")
                           (p "No posts were found in the specified period.")))
                        #:status 404
                        #:title *title*))
              ((early-enough? (car posts))
               (let lp ((posts posts) (new-header (make-date-header #t)) (out '()))
                 (cond
                  ((or (null? posts) (too-early? (car posts)))
                   (respond (reverse out)
                            #:title (string-append "archives: " *title*)
                            #:etag (assq-ref index 'master)
                            #:subtitle subtitle))
                  ((new-header (car posts))
                   => (lambda (sxml)
                        (lp (cdr posts) (make-date-header (car posts))
                            (cons (post-link (car posts)) (append sxml out)))))
                  (else
                   (lp (cdr posts) new-header (cons `(p ,(post-link (car posts))) out))))))
              (else (lp (cdr posts))))))))

(define (page-search request body index)
  (let* ((string (or (assoc-ref (request-form-data request body) "string") ""))
         (posts (find-posts-matching string index)))
    (define subtitle
      (format #f "articles containing the string ~S." string))
    (respond `((section
                (h2 "search results: \"" ,string "\"")
                ,@(if (null? posts)
                      `((p "No posts matched your search string."))
                      (map (lambda (post)
                             `(p ,(post-link post)))
                           posts))))
             #:status (if (null? posts) 404 200)
             #:subtitle subtitle)))

(define (page-show-tags request body index)
  (define subtitle
    "a clickable tag cloud of all tags used in published articles.")
  (respond `((section
              (@ (id "tag-cloud"))
              (h2 "all tags")
              ,@(tag-cloud (top-tags index 200))))
           #:etag (assq-ref index 'master)
           #:title (string-append "all tags — " *title*)
           #:subtitle subtitle))

(define (page-show-tag request body index tag)
  (let* ((tags (assq-ref index 'tags))
         (posts (map (lambda (key)
                       (post-from-key index key))
                     (hash-ref tags tag '()))))
    (match posts
      (()
       (respond `((section
                   (h2 "Unknown tag " ,tag)
                   (p "No posts were found tagged as \"" ,tag "\".")))
                 #:status 404))
      (_
       (define subtitle
         (format #f "articles with the tag ~S." tag))
       (respond `((section
                   (h2 "posts tagged \"" ,tag "\" ("
                       ,(rellink '("feed" "atom") "feed"
                                 #:query `(("with" . ,tag)))
                       ")")
                   ,@(map (lambda (post) `(p ,(post-link post)))
                          posts)
                   ,(related-tag-cloud tag index)))
                #:etag (assq-ref index 'master)
                #:title (string-append "posts tagged \"" tag "\"")
                #:subtitle subtitle
                ;; Could add related tags.
                #:keywords (list tag))))))

(define (page-debug request body index)
  (respond `((section
              (h2 "hello world!")
              (table
               (tr (th "header") (th "value"))
               ,@(map (lambda (pair)
                        `(tr (td (tt ,(with-output-to-string
                                        (lambda () (display (car pair))))))
                             (td (tt ,(with-output-to-string
                                        (lambda ()
                                          (write (cdr pair))))))))
                      (request-headers request)))))
           #:title "debug"))

(define (page-not-found request body index)
  (respond `((section
              (h2 "Page not found")
              (p "Unknown path: "
                 ,(request-relative-path-str (pk 'not-found request)))))
           #:status 404))


(define (atom-feed-from-posts request body index posts)
  (let ((last-modified (and (pair? posts)
                            (post-timestamp (car posts)))))
    (cond
     ((let ((since (request-if-modified-since request)))
        (and since last-modified (>= (date->timestamp since) last-modified)))
      (respond #f #:status 304
               #:last-modified (timestamp->date last-modified)
               #:etag (assq-ref index 'master)))
     (else
      (respond #f
               #:last-modified (and=> last-modified timestamp->date)
               #:doctype #f
               #:content-type 'application/atom+xml
               #:etag (assq-ref index 'master)
               #:sxml (append (atom-header last-modified)
                              (map
                               (lambda (post)
                                 (atom-entry post))
                               posts)))))))

(define (page-feed-atom request body index)
  (let ((with (request-query-ref-all request "with"))
        (without (request-query-ref-all request "without"))
        (tags (assq-ref index 'tags)))
    (define include?
      (if (pair? with)
          (fold (lambda (tag cont)
                  (let ((posts (hash-ref tags tag '())))
                    (if (pair? posts)
                        (lambda (post)
                          (or (member (post-key post) posts)
                              (cont post)))
                        cont)))
                (lambda (post) #f)
                with)
          (lambda (post) #t)))
    (define exclude?
      (fold (lambda (tag cont)
              (let ((posts (hash-ref tags tag '())))
                (lambda (post)
                  (or (member (post-key post) posts)
                      (cont post)))))
            (lambda (post) #f)
            without))

    (atom-feed-from-posts
     request body index
     (latest-posts index
                   #:filter
                   (lambda (post)
                     (and (include? post) (not (exclude? post))))
                   #:limit 10))))
