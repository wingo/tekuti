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

;;hack!
(define-module (tekuti comment)
  #:use-module (tekuti git)
  #:use-module (tekuti util)
  #:use-module (tekuti filters)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (sxml transform)
  #:use-module (match-bind)
  #:export (comment-from-object comment-sxml-content comment-timestamp build-comment-skeleton comment-readable-date
            bad-new-comment-post? make-new-comment))

(define *comment-spec*
  `((timestamp . ,string->number)))
(define (comment-from-object encoded-name sha1)
  (let ((blob (git "show" sha1)))
    (match-bind
     "\n\n(.*)$" blob (_ content)
     (fold cons
           (filter
            identity
            (match-lines (substring blob 0 (- (string-length blob)
                                              (string-length _)))
                         "^([^: ]+): +(.*)$" (_ k v)
                         (let* ((k (string->symbol k))
                                (parse (assq-ref *comment-spec* k)))
                           (if parse
                               (catch 'parse-error
                                      (lambda ()
                                        (cons k (parse v)))
                                      (lambda args #f))
                               (cons k v)))))
           `((raw-content . ,content)
             (sha1 . ,sha1)
             (key . ,encoded-name))))))

(define (comment-readable-date comment)
  (let ((date (time-utc->date
               (make-time time-utc 0 (assq-ref comment 'timestamp)))))
    (date->string date "~e ~B ~Y ~l:~M ~p")))

(define (comment-raw-content comment)
  (assq-ref comment 'raw-content))

(define (comment-sxml-content comment)
  (let ((format (or (assq-ref comment 'format) 'wordpress)))
    ((case format
       ((wordpress) wordpress->sxml)
       (else (lambda (text) `(pre ,text))))
     (comment-raw-content comment))))

(define (comment-timestamp comment-alist)
  (or (assq-ref comment-alist 'timestamp) #f))

(define (build-comment-skeleton comments)
  (fold (lambda (sha1 parent)
          (let* ((ts (comment-timestamp sha1))
                 (env (list "GIT_COMMMITTER=tekuti"
                            (format #f "GIT_COMMITTER_DATE=~a +0100" ts)
                            (format #f "GIT_AUTHOR_DATE=~a +0100" ts))))
            (string-trim-both
             (git* (cons* "commit-tree" sha1 (if parent (list "-p" parent) '()))
                   #:input "comment\n" #:env env))))
        #f
        comments))

(define (emailish? x)
  (match-bind "^([a-zA-Z0-9.+-]+)@([a-zA-Z0-9-]+\\.)+[a-zA-Z]+$"
              x (_ . args)
              x
              #f))

(define (bad-email? x)
  (if (emailish? x)
      #f
      `(p "Please pretend to specify a valid email address.")))

(define (urlish? x)
  (match-bind "^https?://([a-zA-Z0-9-]+\\.)+[a-zA-Z]+/[a-zA-Z0-9$_.+!*'(),;/?:@&=-]*$"
              x (_ . args)
              x
              #f))

(define (bad-url? x)
  (if (or (string-null? x) (urlish? x))
      #f
      `(p "Bad URL. (Only http and https are allowed.)")))

(define *allowed-tags*
  `((a (href . ,urlish?) title)
    (abbr title)
    (acronym title)
    (b)
    (br)
    (blockquote (cite . ,urlish?))
    (code)
    (em)
    (i)
    (p)
    (pre)
    (strike)
    (strong)))

(define (compile-sxslt-rules tags)
  (define (ok . body)
    body)
  `(,@(map (lambda (spec)
             `(,(car spec)
               ((@ (,@(map (lambda (attr)
                             (if (symbol? attr)
                                 `(,attr . ,ok)
                                 `(,(car attr)
                                   . ,(lambda (tag text)
                                        (or ((cdr attr) text)
                                            (throw 'bad-attr-value text))
                                        (list tag text)))))
                           (cdr spec)))
                   . ,ok))
               . ,ok))
           *allowed-tags*)
    (*text* . ,(lambda (tag text)
                 text))
    (@ . ,(lambda (tag text)
            (throw 'bad-attr tag)))
    (*default* . ,(lambda (tag . body)
                    (throw 'bad-tag tag)))))

;; could be better, reflect nesting rules...
(define *valid-xhtml-rules*
  `((div ,(compile-sxslt-rules *allowed-tags*)
         . ,(lambda body body))))

(use-modules (sxml transform) (tekuti filters))
(define (bad-xhtml? x)
  (catch #t
         (lambda ()
           (pre-post-order (wordpress->sxml x) *valid-xhtml-rules*)
           #f)
         (lambda (key . args)
           `(div (p "Invalid XHTML")
                 ,(case key
                    ((parser-error)
                     `(pre ,(with-output-to-string
                              (lambda () (write args)))))
                    ((bad-tag)
                     `(p "XHTML tag disallowed: " ,(symbol->string (car args))))
                    ((bad-attr)
                     `(p "XHTML attribute disallowed: " ,(symbol->string (car args))))
                    ((bad-attr-value)
                     `(p "XHTML attribute has bad value: " ,(car args)))
                    (else
                     (pk key args)
                     `(p "Jesus knows why, and so do you")))))))

(define *new-comment-spec*
  `(("author" ,(lambda (x) #f))
    ("email" ,bad-email?)
    ("url" ,bad-url?)
    ("comment" ,bad-xhtml?)
    ("submit" ,(lambda (x) #f))))

(define (bad-new-comment-post? post-data)
  (or (or-map (lambda (pair)
                (and (not (assoc (car pair) *new-comment-spec*))
                     `(p "Bad post data: " ,(car pair))))
              post-data)
      (or-map (lambda (pair)
                (and (not (assoc (car pair) post-data))
                     `(p "Incomplete post data:" ,(car pair))))
              *new-comment-spec*)
      (or-map (lambda (pair)
                ((cadr pair) (assoc-ref post-data (car pair))))
              *new-comment-spec*)))

(use-modules (srfi srfi-11))
(define (make-tree-deep treeish add remove change)
  (define (local? x) (null? (car x)))
  (define (assert-added-files-not-present names dents)
    (for-each
     (lambda (dent)
       (if (member (car dent) names)
           (error "file already added" dent)))
     dents))
  (define (assert-referenced-files-present names dents)
    (for-each
     (lambda (name)
       (if (not (assoc name dent-names))
           (error "file already removed" name)))
     names))
  (let-values (((dents) (if treeish (git-ls-tree treeish #f) '()))
               ((ladd dadd) (partition local? add))
               ((lremove dremove) (partition local? remove))
               ((lchange dchange) (partition local? change)))
    (assert-added-files-not-present (map cadr ladd) dents)
    (assert-referenced-files-present
     (append (map cdr lremove) (map caar lchange)) dents)
    ; (trc 'make-tree-deep treeish add remove change)
    (make-tree-full
     (append
      (map cdr ladd)
      (filter-map
       (lambda (dent)
         (cond
          ((member (car dent) (map cdr lremove))
           #f)
          ((member (car dent) (map cadr lchange))
           (cdr lchange))
          ((and (equal? (caddr dent) "tree")
                (member (car dent)
                        (map caar (append dadd dremove dchange))))
           (let ((level-down (lambda (x)
                               (if (equal? (caar x) (car dent))
                                   (cons (cdar x) (cdr x))
                                   #f))))
             (list (car dent)
                   (make-tree-deep (cadr dent)
                                   (filter-map level-down dadd)
                                   (filter-map level-down dremove)
                                   (filter-map level-down dchange))
                   "tree" "040000")))
          (else dent)))
       (append (filter-map (lambda (x)
                             (and (not (assoc (caar x) dents))
                                  (list (caar x) #f "tree" #f)))
                           dadd)
               dents))))))
    
(define (mutate-tree master add remove change message)
  (let ((tree (make-tree-deep master add remove change)))
    (string-trim-both
     (git* `("commit-tree" ,tree "-p" ,master) #:input message
           #:env '("GIT_COMMMITTER=tekuti")))))

(define (make-new-comment post post-data)
  (let ((content (assoc-ref post-data "comment"))
        (author (assoc-ref post-data "author"))
        (email (assoc-ref post-data "email"))
        (url (assoc-ref post-data "url"))) 
    (let ((sha1 (create-blob 
                 (with-output-to-string
                   (lambda ()
                     (for-each
                      (lambda (pair)
                        (format #t "~a: ~a\n" (car pair) (cdr pair)))
                      `((timestamp . ,(time-second (current-time)))
                        (author . ,(string-join
                                    ;; security foo
                                    (string-split author #\newline)
                                    " "))
                        (author_email . ,email)
                        (author_url . ,url)))
                     (display "\n")
                     (display content))))))
      (git-update-ref
       "refs/heads/master"
       (lambda (master)
         (mutate-tree master
                      `(((,(assq-ref post 'key) "comments") . (,sha1 ,sha1 "blob" "100644")))
                      '()
                      '()
                      "new comment"))
       5))))


