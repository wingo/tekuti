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
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (&git-condition git-condition? git-condition-argv
            git-condition-output git-condition-status false-if-git-error

            git git* ensure-git-repo git-ls-tree git-ls-subdirs
            git-mktree git-rev-parse git-hash-object git-update-ref
            git-commit-tree git-rev-list git-revert

            munge-tree munge-tree1 parse-commit commit-utc-timestamp
            
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

(define (run env input-file args)
  (define (prepend-env args)
    (if (null? env)
        args
        (cons "/usr/bin/env" (append env args))))
  (define (redirect-input args)
    (if input-file
        (list "/bin/sh" "-c"
              (string-append (string-join (map shell:quote args) " ")
                             "<" input-file))
        args))
  (let* ((real-args (trc (redirect-input (prepend-env args))))
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
         (run env tempname (cons* *git* "--bare" args))))
      (run env #f (cons* *git* "--bare" args))))

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
  (or (and treeish
           (false-if-git-error
            (match-lines (git "ls-tree" treeish (or path "."))
                         "^(.+) (.+) (.+)\t(.+)$" (_ mode type object name)
                         ;; reversed for assoc
                         (list name object (string->symbol type)))))
      '()))

(define (git-ls-subdirs treeish path)
  (or (and treeish
           (false-if-git-error
            (match-lines (git "ls-tree" treeish (or path "."))
                         "^(.+) tree (.+)\t(.+)$" (_ mode object name)
                         (cons name object))))
      '()))

(define (git-mktree alist)
  (if (null? alist)
      #f
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
                      "\n" 'suffix)))))

(define (git-rev-parse rev)
  (string-trim-both (git "rev-parse" rev)))

(define (git-rev-list rev n)
  (let lp ((lines (string-split
                   (git "rev-list" "--pretty=format:%ct %s"
                        "-n" (number->string n) rev) #\newline))
           (ret '()))
    (if (or (null? lines)
            (and (null? (cdr lines)) (string-null? (car lines))))
        (reverse ret)
        (lp (cddr lines)
            (let ((line1 (car lines)) (line2 (cadr lines)))
              (match-bind
               "^commit (.*)$" line1 (_ sha1)
               (match-bind
                "^([0-9]+) (.*)$" line2 (_ ts subject)
                (cons `(,sha1 ,(string->number ts) ,subject) ret)
                (error "bad line2" line2))
               (error "bad line1" line1)))))))

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

;; unused.
(define (patch-blob sha1 patch)
  (call-with-temp-file
   (git "cat-file" "blob" sha1)
   (lambda (orig)
     (run '() patch (list "patch" "-N" "-s" "-u" "-r" "/dev/null" orig))
     (with-output-to-blob
       (display
        (call-with-input-file (orig)
          (read-delimited "" port)))))))

;; could leave stray comments if the post directory changes. but this is
;; probably the best that we can do, given that git does not track
;; directory renames.
(define (git-commit-reverse-operations sha1)
  (with-input-from-string (git "diff-tree" "-R" "-r" sha1)
    (lambda ()
      (read-line) ;; throw away the header
      (let lp ((ops '()))
        (let ((line (read-line)))
          (if (eof-object? line)
              ops
              (match-bind
               "^:([0-9]+) ([0-9]+) ([0-9a-f]+) ([0-9a-f]+) (.)\t(.*)$"
               line (_ mode1 mode2 ob1 ob2 op path)
               (let ((head (let ((d (dirname path)))
                                  (if (string=? d ".") '()
                                      (string-split d #\/))))
                     (tail (basename path)))
                 (lp
                  (case (string-ref op 0)
                    ((#\D) (cons `(delete ,head (,tail))
                                 ops))
                    ((#\A) (cons `(create ,head (,tail ,ob2 blob))
                                 ops))
                    ((#\M) (cons* `(delete ,head (,tail))
                                  `(create ,head (,tail ,ob2 blob))
                                  ops)))))
               (error "crack line" line))))))))

(define (git-revert ref sha1)
  (let ((ops (git-commit-reverse-operations sha1)))
    (git-update-ref ref
                    (lambda (master)
                      (git-commit-tree (munge-tree master ops)
                                       master "revert change" #f))
                    5)))

(define (munge-tree1-local dents command arg)
  (define (command-error why)
    (error "munge-tree1-local error" why command arg))
  (let ((dent (assoc (car arg) dents)))
    (git-mktree
     (case command
       ((create) (if dent
                     (command-error 'file-present)
                     (cons arg dents)))
       ((delete) (if dent
                     (delq dent dents)
                     (command-error 'file-not-present)))
       ((rename) (if dent
                     (acons (cadr arg) (cdr dent) (delq dent dents))
                     (command-error 'file-not-present)))
       (else (command-error 'unrecognized))))))

(define (munge-tree1-recursive dents command ldir rdir arg)
  (define (command-error why)
    (error "munge-tree1-recursive error" why command dir arg))
  (let ((dent (assoc ldir dents)))
    (if (and dent (not (eq? (caddr dent) 'tree)))
        (command-error 'not-a-tree))
    (let ((subtree (and=> dent cadr))
          (other-dents (if dent (delq dent dents) dents)))
      (let ((new (case command
                   ((create)
                    (munge-tree1 subtree command rdir arg))
                   ((delete rename)
                    (if subtree
                        (munge-tree1 subtree command rdir arg)
                        (command-error 'file-not-present)))
                   (else (command-error 'unrecognized)))))
        (git-mktree (if new
                        (cons (list ldir new 'tree) other-dents)
                        other-dents))))))

(define (munge-tree1 treeish command dir arg)
  (let ((dents (git-ls-tree treeish #f)))
    (if (null? dir)
        (munge-tree1-local dents command arg)
        (munge-tree1-recursive dents command (car dir) (cdr dir) arg))))

;; (munge-tree sha1 ((create (key comments) (name sha1 blob))
;;                   (delete (foo bar) (name))
;;                   (rename (baz borky) (from to))))
(define (munge-tree treeish operations)
  (if (null? operations)
      treeish
      (let ((op (car operations)))
        (munge-tree (munge-tree1 treeish (car op) (cadr op) (caddr op))
                    (cdr operations)))))

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
