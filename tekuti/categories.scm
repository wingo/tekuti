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

(define-module (tekuti categories)
  #:use-module (tekuti util)
  #:use-module (tekuti post)
  #:use-module (tekuti git)
  #:export (reindex-categories))

(define (compute-categories posts)
  (let ((hash (make-hash-table)))
    (for-each
     (lambda (post-pair)
       (for-each
        (lambda (cat)
          (hash-push! hash cat post-pair))
        (post-categories (cdr post-pair))))
     posts)
    hash))

(define (build-categories-tree master posts)
  (if (null? posts)
      #f
      (let* ((hash (compute-categories posts))
             (tree (make-tree (hash-map->list
                               (lambda (k v) (cons k (make-tree v)))
                               hash)))
             (ts (commit-utc-timestamp master))
             (env (list "GIT_COMMMITTER=tekuti"
                        ;; this quoting is a hack
                        (format #f "'GIT_COMMITTER_DATE=~a +0000'" ts)
                        (format #f "'GIT_AUTHOR_DATE=~a +0000'" ts))))
        (string-trim-both
         (git/input+env "categories\n" env "commit-tree" tree
                        "-p" master))))) ;; FIXME: keep history?

(define (reindex-categories master)
  (build-categories-tree master (all-published-posts master)))
