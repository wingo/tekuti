;; (match-bind) -- binding variables from regular expression matches
;; Copyright (C) 2007, 2009, 2010, 2012 Andy Wingo <wingo at pobox dot com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; @c
;; Utility functions and syntax constructs for dealing with regular
;; expressions in a concise manner.
;;
;;; Code:

(define-module (tekuti match-bind)
  #:use-module (ice-9 regex)
  #:export (match-bind
            s///
            s///g))

(define (regex:count re-string)
  (let lp ((char-class #f) (nparen 0) (in (string->list re-string)))
    (cond
     ((null? in) nparen)
     ((char=? (car in) #\\)
      (lp char-class nparen (cddr in)))
     ((char=? (car in) #\[)
      (lp #t nparen (cdr in)))
     ((char=? (car in) #\])
      (lp #f nparen (cdr in)))
     ((and (char=? (car in) #\() (not char-class))
      (lp #f (1+ nparen) (cddr in)))
     (else
      (lp char-class nparen (cdr in))))))

(define-syntax match-substring
  (syntax-rules ()
    ((_ m i)
     (let* ((pair (vector-ref m (1+ i)))
            (start (car pair))
            (end (cdr pair)))
       (if (or (= start -1) (= end -1))
           #f
           (substring (vector-ref m 0) start end))))))

(define *cache* (make-weak-key-hash-table))
(define (memoize-re regex)
  (cond
   ((hashq-ref *cache* regex))
   (else
    (let ((re (make-regexp regex)))
      (hashq-set! *cache* regex re)
      re))))

(define-syntax match-bind
  (lambda (x)
    "Match a string against a regular expression, binding lexical
variables to the various parts of the match.

@var{vars} is a list of names to which to bind the parts of the match.
The first variable of the list will be bound to the entire match, so the
number of variables needed will be equal to the number of open
parentheses (`(') in the pattern, plus one for the whole match.

@var{consequent} is executed if the given expression @var{str} matches
@var{regex}. If the string does not match, @var{alternate} will be
executed if present. If @var{alternate} is not present, the result of
@code{match-bind} is unspecified.

Here is a short example:
@example
 (define (star-indent line)
   \"Returns the number of spaces until the first
    star (`*') in the input, or #f if the first
    non-space character is not a star.\"
   (match-bind \"^( *)\\*.*$\" line (_ spaces)
               (string-length spaces)
               #f))
@end example
"
    (define (match-bindings m re vars nvars)
      (let lp ((in vars) (i 0) (out '()))
        (syntax-case in ()
          (()
           (if (not (= i nvars))
               (error "bad number of bindings to match-bind" i nvars)
               (reverse out)))
          ((v . v*)
           (lp #'v* (1+ i)
               (with-syntax ((i i) (m m))
                 (cons #'(v (match-substring m i)) out))))
          (v (and (identifier? #'v) (<= i nvars))
             (reverse
              (cons (with-syntax
                        (((init ...)
                          (map (lambda (x)
                                 (with-syntax ((m m) (i (+ i x)))
                                   #'(match-substring m i)))
                               (iota (- nvars i)))))
                      #'(v (list init ...)))
                    out))))))
    (syntax-case x ()
      ((_ regex str vars consequent)
       #'(match-bind regex str vars consequent (if #f #f)))
      ((_ regex str vars consequent alternate)
       (string? (syntax->datum #'regex))
       (let ((m #'m))
         (with-syntax ((m m)
                       (((var val) ...)
                        (match-bindings m (memoize-re (syntax->datum #'regex))
                                        #'vars
                                        ;; 1+ for the match:0 binding
                                        (1+ (regex:count
                                             (syntax->datum #'regex))))))
           #'(let* ((m (regexp-exec (memoize-re regex) str)))
               (if m
                   (let ((var val) ...)
                     consequent)
                   alternate))))))))

(define-macro (make-state-parser states initial)
  `(lambda (port)
     (let lp ((state ',initial)
              (c (read-char port))
              (out '())
              (accum '()))
       (case state
         ((*eof*) (reverse out))
         ,@(map
            (lambda (desc)
              (let ((name (car desc))
                    (cont (cadr desc))
                    (cases (map
                            (lambda (kase)
                              (let ((condition (car kase))
                                    (new-state (cdr kase)))
                                (cond
                                 ((not (symbol? new-state))
                                  (error "invalid new-state in spec" new-state))
                                 ((number? condition)
                                  `((= (length accum) ,condition) ',new-state))
                                 ((list? condition)
                                  `((memv c ',condition) ',new-state))
                                 (else
                                  `(,condition ',new-state)))))
                            (cddr desc))))
                `((,name)
                  (let ((new-state (cond ((eof-object? c) '*eof*) ,@cases))
                        (cont ,cont))
                    (if (eq? state new-state)
                        (lp state (read-char port) out (cons c accum))
                        (lp new-state c
                            (if cont
                                (cons (cont (reverse accum)) out)
                                out)
                            '()))))))
            states)
         (else (error "invalid state" state (reverse out) (reverse accum)))))))

(define (make-item-list subst)
  (map
   (lambda (item)
     (cond ((procedure? item) item)
           ((string? item) (lambda (match) item))
           (else (error "internal error" item))))
   (call-with-input-string
    subst
    (make-state-parser
     ((string
       list->string
       ((#\\) . quote-head)
       ((#\$) . variable-head)
       (else . string))
      (quote-head
       #f
       (0 . quote-head)
       (else . quote))
      (quote
       list->string
       (0 . quote)
       ((#\\) . quote-head)
       ((#\$) . variable-head)
       (else . string))
      (variable-head
       #f
       (0 . variable-head)
       ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . variable)
       (else . error))
      (variable
       (lambda (l)
         (let ((i (string->number (list->string l))))
           (lambda (match)
             (or (match:substring match i) ""))))
       ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) . variable)
       ((#\\) . quote-head)
       ((#\$) . variable-head)
       (else . string)))
     string))))

(define (s/// pat subst)
  "Make a procedure that performs perl-like regular expression
search-and-replace on an input string.

The regular expression pattern @var{pat} is in the standard regular
expression syntax accepted by @code{make-regexp}. The substitution
string is very similar to perl's @code{s///} operator. Backreferences
are indicated with a dollar sign (@samp{$}), and characters can be
escaped with the backslash.

@code{s///} returns a procedure of one argument, the input string to be
matched. If the string matches the pattern, it will be returned with the
first matching segment replaced as per the substitution string.
Otherwise the string will be returned unmodified.

Here are some examples:

@example
 ((s/// \"foo\" \"bar\") \"foo bar baz qux foo\")
    @result{} \"bar bar baz qux foo\"

 ((s/// \"zag\" \"bar\") \"foo bar baz qux foo\")
    @result{} \"foo bar baz qux foo\"

 ((s/// \"(f(o+)) (zag)?\" \"$1 $2 $3\")
  \"foo bar baz qux foo\")
    @result{} \"foo oo bar baz qux foo\"
@end example
"
  (let ((re (make-regexp pat))
        (substers `(,match:prefix
                    ,@(make-item-list subst)
                    ,match:suffix)))
    (lambda (string)
      (let ((match (regexp-exec re string)))
        (if match
            (apply string-append (map (lambda (p) (p match)) substers))
            string)))))

(define (s///g pat subst)
  "Make a procedure that performs perl-like global search-and-replace on
an input string.

The @var{pat} and @var{subst} arguments are as in the non-global
@code{s///}. @xref{match-bind s///,,s///}, for more information.

@code{s///g} differs from @code{s///} in that it does a global search
and replace, not stopping at the first match.
"
  (let ((re (make-regexp pat))
        (items `(pre
                 ,@(make-item-list subst)
                 post)))
    (lambda (string)
      (apply regexp-substitute/global #f re string items))))
