;; Tekuti
;; Copyright (C) 2008, 2010, 2011 Andy Wingo <wingo at pobox dot com>

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

(define-module (tekuti filters)
  #:use-module (sxml simple)
  #:use-module (sxml transform)
  #:use-module (tekuti match-bind)
  #:use-module (tekuti util)
  #:export (wordpress->sxml
            *allowed-tags* bad-user-submitted-xhtml?))

(define blocks '(table thead tfoot caption colgroup tbody tr td th div
                 dl dd dt ul ol li pre select form map area blockquote
                 address math style input p h1 h2 h3 h4 h5 h6))

(define (can-contain-p? tag)
  (memq tag '(div li blockquote)))

(define (inline? tag)
  (not (memq tag blocks)))

(define (wpautop tag body)
  (define (pclose p out)
    (if p (cons (cons 'p (reverse p)) out) out))
  (define (scons x p)
    (if (and (string? x) (string-null? x))
        p
        (cons x (or p '()))))
  (define (pbreak p)
    (if p (cons '(br) p) p))
  
  (let* ((head (if (and (pair? (car body))
                        (eq? (caar body) '@))
                   (list tag (car body))
                   (list tag)))
         (body (if (null? (cdr head)) body (cdr body))))
    (let lp ((p #f) (in body) (out (reverse head)))
      (cond
       ((null? in)
        (reverse (pclose p out)))
       ((string? (car in))
        (match-bind "^([^\n]*)\n(\n*)(.*)$" (car in) (_ head p-or-br? tail)
                    (if (string-null? p-or-br?)
                        (lp (if (string-null? tail)
                                (scons head p)
                                (pbreak (scons head p)))
                            (scons tail (cdr in)) out)
                        (lp #f (scons tail (cdr in))
                            (pclose (scons head p) out)))
                    (lp (scons (car in) p)
                        (cdr in) out)))
       ((inline? (caar in))
        (lp (scons (car in) p) (cdr in) out))
       (else
        (lp #f (cdr in)
            (cons (car in) (pclose p out))))))))

(define (wordpress->sxml text)
  (let ((sxml (cadr (with-input-from-string* (string-append "<div>" text "</div>")
                      xml->sxml))))
    (pre-post-order
     sxml
     `((*default* . ,(lambda (tag . body)
                       (if (can-contain-p? tag)
                           (wpautop tag body)
                           (cons tag body))))
       (*text* . ,(lambda (tag text)
                    text))))))

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
  (define (compile-attribute-rule rule)
    (if (symbol? rule)
        `(,rule . ,ok)
        `(,(car rule) . ,(lambda (tag text)
                           (or ((cdr rule) text)
                               (throw 'bad-attr-value text))
                           (list tag text)))))
  `(,@(map (lambda (spec)
             `(,(car spec)
               ((@ *preorder*
                   . ,(let ((rules `((@ (,@(map compile-attribute-rule
                                                (cdr spec))
                                         (*text*
                                          . ,(lambda (tag text) text))
                                         (*default*
                                          . ,(lambda (tag . body)
                                               (throw 'bad-attr tag))))
                                        . ,ok))))
                        (lambda tree
                          (pre-post-order tree rules)))))
               . ,ok))
           *allowed-tags*)
    (*text* . ,(lambda (tag text)
                 text))
    (*default* . ,(lambda (tag . body)
                    (throw 'bad-tag tag)))))

;; could be better, reflect nesting rules.
(define *valid-xhtml-rules*
  `((div ,(compile-sxslt-rules *allowed-tags*)
         . ,(lambda body body))))

(define (bad-user-submitted-xhtml? x)
  (catch #t
         (lambda ()
           (pre-post-order (wordpress->sxml x) *valid-xhtml-rules*)
           #f)
         (lambda (key . args)
           `(div (p (b "Invalid XHTML"))
                 ,(case key
                    ((parser-error)
                     `(div
                       (p "Valid XHTML is required, although it will "
                          "translate single newlines to <br/> elements, "
                          "and multiple newlines to paragraphs.")
                       (p "Usually if you get here it's because you put in a "
                          "malformed XHTML tag.")
                       (p "Another way to get here is if "
                          "you have an unescaped <, >, or & character. Replace "
                          "them with &lt;, &gt;, or &amp;, respectively.")
                       (p (b "Here is the internal error:"))
                       (pre ,(with-output-to-string
                               (lambda () (write args))))))
                    ((bad-tag)
                     `(p "XHTML tag disallowed: " ,(symbol->string (car args))))
                    ((bad-attr)
                     `(p "XHTML attribute disallowed: " ,(symbol->string (car args))))
                    ((bad-attr-value)
                     `(p "XHTML attribute has bad value: " ,(car args)))
                    (else
                     (pk key args)
                     `(p "Jesus knows why, and so do you")))))))

