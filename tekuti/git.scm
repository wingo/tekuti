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
;; Using git's object database as a persistent store.
;;
;;; Code:

(define-module (tekuti git)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:use-module (tekuti util)
  #:use-module (tekuti config)
  #:use-module (scheme kwargs)
  #:use-module (match-bind)
  #:use-module ((srfi srfi-1) #:select (filter-map partition
                                        delete-duplicates))
  #:use-module (srfi srfi-11) ; let-values
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (&git-condition git-condition? git-condition-argv
            git-condition-output git-condition-status false-if-git-error

            git git* ensure-git-repo git-ls-tree git-ls-subdirs
            git-mktree git-rev-parse git-hash-object git-update-ref
            git-commit-tree

            munge-tree parse-commit commit-utc-timestamp
            
            with-output-to-blob with-input-from-blob))


;;;
;;; git conditions
;;;

(define-condition-type &git-condition &condition git-condition?
  (argv git-condition-argv)
  (output git-condition-output)
  (status git-condition-status))

(define-macro (false-if-git-error . body)
  `(,guard (c ((,git-condition? c) #f))
           ,@body))

;;;
;;; running git
;;; 

(define *debug* #f)
(define (trc . args)
  (if *debug*
      (apply pk args)
      (car (last-pair args))))

(define (run-git env input-file args)
  (define (prepend-env args)
    (if (null? env)
        args
        (cons "/usr/bin/env" (append env args))))
  (define (prepend-git args)
    (cons* *git* "--bare" args))
  (define (redirect-input args)
    (if input-file
        (list "/bin/sh" "-c"
              (string-append (string-join (map shell:quote args) " ")
                             "<" input-file))
        args))
  (let* ((real-args (trc (redirect-input (prepend-env (prepend-git args)))))
         (pipe (apply open-pipe* OPEN_READ real-args))
         (output (read-delimited "" pipe))
         (ret (close-pipe pipe)))
    (case (status:exit-val ret)
      ((0) (if (eof-object? output) "" output))
      (else (trc 'git-error output ret real-args)
            (raise (condition (&git-condition
                               (argv real-args)
                               (output output)
                               (status ret))))))))

(define/kwargs (git* args (input #f) (env '()))
  (if input
      (call-with-temp-file
       input
       (lambda (tempname)
         (trc input)
         (run-git env tempname args)))
      (run-git env #f args)))

(define (git . args)
  (git* args))

;;;
;;; git commands
;;; 

(define (is-dir? path)
  (catch 'system-error
         (lambda () (eq? (stat:type (stat path)) 'directory))
         (lambda args #f)))

(define (ensure-git-repo)
  (let ((d (expanduser *git-dir*)))
    (if (not (is-dir? d))
        (begin
          (mkdir d)
          (chdir d)
          (git "init"))
        (chdir d))))

(define (git-ls-tree treeish path)
  (or (false-if-git-error
       (match-lines (git "ls-tree" treeish (or path "."))
                    "^(.+) (.+) (.+)\t(.+)$" (_ mode type object name)
                    ;; reversed for assoc
                    (list name object (string->symbol type))))
      '()))

(define (git-ls-subdirs treeish path)
  (or (false-if-git-error
       (match-lines (git "ls-tree" treeish (or path "."))
                    "^(.+) tree (.+)\t(.+)$" (_ mode object name)
                    (cons name object)))
      '()))

(define (git-mktree alist)
  (string-trim-both
   (git* '("mktree")
         #:input (string-join
                  (map (lambda (l)
                         (format #f
                                 (if (or (null? (cddr l))
                                         (equal? (caddr l) 'blob))
                                     "100644 blob ~a\t~a"
                                     "040000 tree ~a\t~a")
                                 (cadr l) (car l)))
                       alist)
                  "\n" 'suffix))))

(define (git-rev-parse rev)
  (string-trim-both (git "rev-parse" rev)))

(define (git-hash-object contents)
  (string-trim-both
   (git* '("hash-object" "-w" "--stdin") #:input contents)))

(define (git-update-ref refname proc count)
  (let* ((ref (git-rev-parse refname))
         (commit (proc ref)))
    (cond
     ((zero? count)
      (error "my god, we looped 5 times" commit))
     ((false-if-git-error
       (git "update-ref" refname commit ref))
      commit)
     (else
      (pk "failed to update the ref, trying again..." refname)
      (git-update-ref (git-rev-parse refname) (1- count))))))

(define (git-commit-tree tree parent message timestamp)
  (string-trim-both
   (git* (cons* "commit-tree" tree
                (if parent (list "-p" parent) '()))
         #:input message
         #:env (if timestamp
                   (list "GIT_COMMMITTER=tekuti"
                         (format #f "GIT_COMMITTER_DATE=~a +0100" timestamp)
                         (format #f "GIT_AUTHOR_DATE=~a +0100" timestamp))
                   (list "GIT_COMMMITTER=tekuti")))))

;;; 
;;; utilities
;;; 

(define (munge-tree treeish add remove change)
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
    ; (trc 'munge-tree treeish add remove change)
    (git-mktree
     (append
      (map cdr ladd)
      (filter-map
       (lambda (dent)
         (cond
          ((member (car dent) (map cdr lremove))
           #f)
          ((member (car dent) (map cadr lchange))
           (cdr lchange))
          ((and (eq? (caddr dent) 'tree)
                (member (car dent)
                        (map caar (append dadd dremove dchange))))
           (let ((level-down (lambda (x)
                               (if (equal? (caar x) (car dent))
                                   (cons (cdar x) (cdr x))
                                   #f))))
             (list (car dent)
                   (munge-tree (cadr dent)
                               (filter-map level-down dadd)
                               (filter-map level-down dremove)
                               (filter-map level-down dchange))
                   'tree)))
          (else dent)))
       (append (delete-duplicates
                (filter-map (lambda (x)
                              (and (not (assoc (caar x) dents))
                                   (list (caar x) #f 'tree)))
                            dadd))
               dents))))))
    
(define (parse-commit commit)
  (let ((text (git "cat-file" "commit" commit)))
    (match-bind
     "\n\n(.*)$" text (_ message)
     (acons
      'message message
      (match-lines (substring text 0 (- (string-length text) (string-length _)))
                   "^([^ ]+) (.*)$" (_ k v)
                   (cons (string->symbol k) v))))))

(define (commit-utc-timestamp commit)
  (match-bind
   "^(.*) ([0-9]+) ([+-][0-9]+)" (assq-ref (parse-commit commit) 'committer)
   (_ who ts tz)
   (let ((ts (string->number ts)) (tz (string->number tz)))
     (- ts (* (+ (* (quotient tz 100) 60) (remainder tz 100)) 60)))))

(define (with-output-to-blob* thunk)
  (git-hash-object (with-output-to-string thunk)))

(define-macro (with-output-to-blob . forms)
  `(,with-output-to-blob* (lambda () ,@forms)))

(define (with-input-from-blob* sha1 thunk)
  (with-input-from-string (git "show" sha1) thunk))

(define-macro (with-input-from-blob sha1 . forms)
  `(,with-input-from-blob* ,sha1 (lambda () ,@forms)))
