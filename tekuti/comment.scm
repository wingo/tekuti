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
  #:use-module (srfi srfi-1)
  #:export (comment-from-tree build-comment-skeleton))

(use-modules (ice-9 regex))

(use-modules (ice-9 rdelim)
             (ice-9 popen)
             (srfi srfi-1)
             (sxml simple)
             (tekuti url)
             (match-bind)
             (sxml transform))
(debug-enable 'backtrace)

(define *comment-spec*
  `((timestamp ,string->number)))
(define (comment-from-tree encoded-name sha1)
  (acons 'encoded-name encoded-name
         (parse-metadata (string-append sha1 ":" "metadata") *comment-spec*)))

(define (comment-timestamp comment-alist)
  (or (assq-ref x 'timestamp) #f))

(define (build-comment-skeleton comments)
  (fold (lambda (sha1 parent)
          (let* ((ts (comment-timestamp sha1))
                 (env (list "GIT_COMMMITTER=tekuti"
                            ;; this quoting is a hack
                            (format #f "'GIT_COMMITTER_DATE=~a +0100'" ts)
                            (format #f "'GIT_AUTHOR_DATE=~a +0100'" ts))))
            (string-trim-both
             (git* (cons* "commit-tree" sha1 (if parent (list "-p" parent) '()))
                   #:input "comment\n" #:env env))))
        #f
        comments))
