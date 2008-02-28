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
;; Indexing of the persistent data store.
;;
;;; Code:

(define-module (tekuti index)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (tekuti util)
  #:use-module (tekuti git)
  #:use-module (tekuti post)
  #:use-module (tekuti tags)
  #:export (maybe-reindex read-index))

(define index-specs
  `((posts ,reindex-posts ,write ,read)
    (tags ,reindex-tags ,write-hash ,read-hash)))

(define (reindex oldindex master)
  (with-backtrace
   (with-time-debugging
    (fold (lambda (pair index)
            (acons (car pair) ((cadr pair) oldindex index)
                   index))
          (acons 'master master '())
          index-specs))))

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
               (pk (map (lambda (pair)
                          (list (car pair)
                                (index->blob (car pair) (cdr pair))
                                'blob))
                        index)))
              oldref "reindex\n"
              (commit-utc-timestamp (assq-ref index 'master)))))
    (or (false-if-git-error
         (git "update-ref" "refs/heads/index" new (or oldref "")))
        (warn "could not update indexes ref"))
    new))

(define (read-index)
  (let* ((ref (false-if-git-error (git-rev-parse "refs/heads/index")))
         (dents (if ref (git-ls-tree ref #f) '())))
    (cons ref
          (and (and-map (lambda (spec)
                          (assoc (symbol->string (car spec)) dents))
                        index-specs)
               (map (lambda (dent)
                      (cons (string->symbol (car dent))
                            (blob->index (car dent) (cadr dent))))
                    dents)))))

(define (maybe-reindex old-index)
  (let ((master (git-rev-parse "refs/heads/master"))
        (old-index-sha1 (and=> old-index car))
        (old-index-data (if old-index (cdr old-index) '())))
    (if (equal? (assq-ref old-index-data 'master) master)
        old-index
        (catch #t
               (lambda ()
                 (let ((new-index (reindex old-index-data master)))
                   (cons (write-index new-index old-index-sha1)
                         new-index)))
               (lambda (key . args)
                 (warn "error while reindexing:" key args)
                 old-index)))))
