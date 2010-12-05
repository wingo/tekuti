;; Tekuti
;; Copyright (C) 2008, 2010 Andy Wingo <wingo at pobox dot com>

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
;; Indexing of the persistent data store.
;;
;;; Code:

(define-module (tekuti index)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (tekuti util)
  #:use-module (tekuti git)
  #:use-module (tekuti post)
  #:use-module (tekuti tags)
  #:use-module (tekuti cache)
  #:export (maybe-reindex read-index update-index))

(define index-specs
  `((posts ,reindex-posts ,write ,read)
    (tags ,reindex-tags ,write-hash ,read-hash)
    (cache ,(lambda _ (make-empty-cache)) ,(lambda (x) #f) ,(lambda () '()))))

(define (reindex oldindex master)
  (with-time-debugging
   (fold (lambda (pair index)
           (acons (car pair) ((cadr pair) oldindex index)
                  index))
         (acons 'master master '())
         index-specs)))

(define (assoc-list-ref alist key n default)
  (let ((l (assoc key alist)))
    (if l (list-ref l n) default)))

(define (index->blob key value)
  (with-output-to-blob
   ((assoc-list-ref index-specs key 2 write) value)))
  
(define (blob->index name sha1)
  (with-input-from-blob
   sha1
   ((assoc-list-ref index-specs (string->symbol name) 3 read))))

(define (write-index index oldref)
  (let ((new (git-commit-tree
              (git-mktree
               (let lp ((index index))
                 (cond
                  ((null? index) '())
                  ((eq? (caar index) 'index) (lp (cdr index)))
                  (else (cons (list (caar index)
                                    (index->blob (caar index) (cdar index))
                                    'blob)
                              (lp (cdr index)))))))
              oldref "reindex\n"
              (commit-utc-timestamp (assq-ref index 'master)))))
    (or (false-if-git-error
         (git "update-ref" "refs/heads/index" new (or oldref "")))
        (warn "could not update indexes ref"))
    new))

(define (read-index)
  (let* ((ref (false-if-git-error (git-rev-parse "refs/heads/index")))
         (dents (if ref (git-ls-tree ref #f) '())))
    (acons 'index ref
           (and (and-map (lambda (spec)
                           (assoc (symbol->string (car spec)) dents))
                         index-specs)
                (map (lambda (dent)
                       (cons (string->symbol (car dent))
                             (blob->index (car dent) (cadr dent))))
                     dents)))))

(define (maybe-reindex old-index)
  (let ((master (git-rev-parse "refs/heads/master")))
    (if (equal? (assq-ref old-index 'master) master)
        old-index
        (catch #t
               (lambda ()
                 (let ((new-index (reindex old-index master)))
                   (acons
                    'index (write-index new-index (assq-ref old-index 'index))
                    new-index)))
               (lambda (key . args)
                 (warn "error while reindexing:" key args)
                 old-index)))))

(define (update-index index key update)
  (cond
   ((null? index) (acons key (update '()) '()))
   ((eq? (caar index) key) (acons key (update index) (cdr index)))
   (else (cons (car index) (update-index (cdr index) key update)))))
