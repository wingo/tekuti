#! /bin/bash
# -*- scheme -*-
exec guile $GUILE_FLAGS -l $0 -e main -- "$@"
!#
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

(define-module (tekuti boot)
  #:use-module (ice-9 format)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 threads)
  #:use-module (tekuti util)
  #:use-module (tekuti git)
  #:use-module (tekuti mod-lisp)
  #:export (boot))

(define *option-grammar* '((gds)
                           (usage)
                           (version (single-char #\v))
                           (help (single-char #\h))))

(define (usage)
  ;; trying out `format'. mixed results.
  (define (repr-option opt)
    (let ((required (and=> (assq 'required (cdr opt)) cadr)))
      (format #f "~:[[~;~]--~a~@*~:[]~;~]"
              (assq 'required? (cdr opt)) (car opt))))
        
  (format #t "usage: tekuti ~{~a~^ ~}~%"
          (map repr-option *option-grammar*)))

(define (version)
  (format #t "tekuti version 0.1" (car (program-arguments))))

;; krap code
(define (parse-options args)
  (let ((opts (getopt-long args '((gds)
                                  (usage)
                                  (version (single-char #\v))
                                  (help (single-char #\h))))))
    (if (or (option-ref opts 'usage #f)
            (option-ref opts 'help #f)
            (not (null? (option-ref (cdr opts) '() '()))))
        (begin
          (usage)
          (exit 0)))
    (if (option-ref opts 'version #f)
        (begin
          (version)
          (exit 0)))
    (if (option-ref opts 'gds #f)
        (let ((run-utility (@ (ice-9 gds-client) run-utility)))
          (make-thread
           (lambda ()
             (with-backtrace run-utility)))))
    opts))

(define (boot args)
  (let ((options (parse-options args)))
    (ensure-git-repo)
    (event-loop)))
