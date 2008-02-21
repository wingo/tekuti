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

(define-module (tekuti post)
  #:use-module (srfi srfi-1)
  #:use-module (match-bind)
  #:use-module (tekuti util)
  #:use-module (tekuti comment)
  #:use-module (tekuti git)
  #:use-module (tekuti filters)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:export (reindex-posts post-from-tree post-from-key post-categories
            post-sxml-content post-raw-content all-published-posts
            post-readable-date post-category-links))
  
;; introducing new assumption: post urls like yyyy/dd/mm/post; post dirnames the urlencoded post

;; perhaps push this processing into post-from-tree
(define (post-published? post-alist)
  (equal? (assq-ref post-alist 'status) "publish"))

(define (post-timestamp post-alist)
  (or (assq-ref post-alist 'timestamp) #f))

(define (post-categories post-alist)
  (or (assq-ref post-alist 'categories) '()))

(define *post-spec*
  `((timestamp . ,string->number)
    (categories . ,(lambda (v) (map string-trim-both (string-split v #\,))))
    (title . ,identity)))

(define-memoized (post-from-tree encoded-name sha1)
  (acons 'key encoded-name
         (acons 'content-ref (string-append sha1 ":content")
                (parse-metadata (string-append sha1 ":metadata")))))

(define (post-raw-content post)
  (git "show" (assq-ref post 'content-ref)))

(define (post-sxml-content post)
  (let ((format (or (assq-ref post 'format) 'wordpress)))
    ((case format
       ((wordpress) wordpress->sxml)
       (else (lambda (text) `(pre ,text))))
     (post-raw-content post))))

(define (post-readable-date post)
  (let ((date (time-utc->date
               (make-time time-utc 0 (assq-ref post 'timestamp)))))
    (date->string date "~e ~B ~Y ~l:~M ~p")))

(define (post-category-links post)
  (post-categories post))

(define (post-from-key master key)
  (let ((pairs (git-ls-subdirs master key)))
    (and (= (length pairs) 1)
         (post-from-tree key (cdar pairs)))))

(define (all-posts master)
  (map (lambda (pair)
         (post-from-tree (car pair) (cdr pair)))
       (git-ls-subdirs master #f)))

(define (all-published-posts master)
  (dsu-sort
   (filter post-published? (all-posts master))
   post-timestamp
   <))

(define (post-comments sha1)
  (dsu-sort
   (map (lambda (pair)
          (comment-from-tree (car pair) (cdr pair)))
        (git-ls-subdirs sha1 "comments/"))
   comment-timestamp
   <))

(define (reindex-posts index)
  (all-published-posts (assq-ref index 'master)))
