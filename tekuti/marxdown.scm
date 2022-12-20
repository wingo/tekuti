;; Tekuti
;; Copyright (C) 2022 Andy Wingo <wingo at pobox dot com>

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
;; "Marxdown" to SXML parser.
;;
;; Marxdown is a dialect of Markdown, designed for simplicity and
;; embeddability.  The X in markdown refers to how it processes embedded
;; XML.
;;
;; In some ways, Marxdown is a more restrictive subset of Markdown:
;;
;;   - Marxdown only supports headings with the ### prefix ("ATX
;;     headings"), and doesn't support so-called "Setext headings" that
;;     follow the heading with --- or similar.  Why bother?
;;
;;   - Marxdown doesn't support indented code blocks, instead only using
;;     fenced code blocks.
;;
;;   - Markxdown doesn't currently support link titles or reference
;;     links.
;;
;;   - Marxdown requires each line of a block quote to have the same
;;     indent and `>` characters, not just the first line.
;;
;;   - Marxdown doesn't support multi-` inline code sequences; just the
;;     single `.
;;
;;   - ...
;;
;; Generally speaking, a valid Marxdown document will also be valid
;; Markdown, as defined by CommonMark.  However there are exceptions.
;; Marxdown is less "sloppy", if you will; unlike Markdown, not any
;; sequence of characters is valid Marxdown.  Parsing Marxdown can raise
;; an error.
;;
;; Notably, Marxdown requires valid nesting.  For example, `*_foo*_` is
;; an error in Marxdown, as the emphasis markers aren't nested properly.
;;
;; Also, when Marxdown parses embedded XML, it hands over control of the
;; character stream to a validating XML parser which can also throw an
;; error.  This XML parser doesn't stop at what CommonMark Markdown
;; would consider block boundaries.  As a consequence, Marxdown cannot
;; be embedded inside XML.  Due to a limitation which may be relaxed in
;; the future, the XML parser doesn't strip off any blockquote prefix.
;;
;; These limitations come with a benefit: tools can easily consume
;; Marxdown and then embed that result in SXML documents of different
;; kinds.
;; 
;;; Code:

(define-module (tekuti marxdown)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 rdelim)
  #:use-module (sxml ssax)
  #:use-module (sxml transform)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:export (marxdown->smarxdown
            smarxdown->shtml))

(define (parse-one-xml-element port)
  ;; -> seed
  (define (fdown elem-gi attributes namespaces expected-content seed)
    '())
  ;; -> seed
  (define (fup elem-gi attributes namespaces parent-seed seed)
    (cons `(,elem-gi
            ,@(match namespaces
                (() '())
                ;; fixme: prohibit?
                (_ `((*NAMESPACES* ,namespaces))))
            ,@(match attributes
                (() '())
                (((attr . value) ...)
                 `((@ . ,(map list attr value)))))
            . ,(reverse seed))
          parent-seed))
  (define (ftext string1 string2 seed)
    (if (string-null? string2)
        (cons string1 seed)
	(cons* string2 string1 seed)))
  (define parse-element (ssax:make-elem-parser fdown fup ftext ()))

  (let ((token (ssax:read-markup-token port)))
    (match (xml-token-kind token)
      ('COMMENT #f)
      ('START
       (let ((elems #f) (entities '()) (namespaces '()) (seed '()))
         (match (parse-element (xml-token-head token) port elems
		               entities namespaces #t seed)
           ((elt) elt))))
      (kind (error "unexpected XML token" token)))))

(define (marxdown->smarxdown port)
  (define (round-up x y)
    (* y (ceiling-quotient x y)))

  (define (advance/tab indent)
    (round-up (1+ indent) 4))

  (define (advance indent)
    (1+ indent))

  (define (unget1 ch)
    (unget-char port ch))
  (define (unget chars)
    (match chars
      (()
       (values))
      ((ch . chars)
       (unget1 ch)
       (unget chars))))

  (define (next) (get-char port))
  (define (peek) (peek-char port))

  (define (next-not-eof ctx)
    (let ((ch (next)))
      (if (eof-object? ch)
          (error "EOF while reading" ctx)
          ch)))
  (define (next-line-and-delim)
    (let lp ((chars '()))
      (define (finish delim)
        (cons (reverse-list->string chars) delim))
      (let ((ch (next)))
        (cond
         ((eof-object? ch) (finish ch))
         ((eqv? ch #\return)
          (if (eqv? (peek) #\newline)
              (finish (next))
              (lp (cons ch chars))))
         ((eqv? ch #\newline)
          (finish ch))
         (else
          (lp (cons ch chars)))))))

  (define (skip-whitespace k)
    (let lp ((indent 0))
      (let ((ch (next)))
        (case ch
          ((#\space)
           (lp (advance indent)))
          ((#\tab)
           (lp (advance/tab indent)))
          (else
           (k ch indent))))))

  (define empty-indent '(0))
  (define (read-indent k)
    (skip-whitespace
     (lambda (ch indent)
       (match ch
         (#\>
          (read-indent
           (lambda (ch indent*)
             (k ch
                (cons indent
                      (match indent*
                        ((head . tail)
                         ;; Account for the #\>.
                         (cons (1+ head) tail))))))))
         (_
          (k ch (list indent)))))))

  (define (advance-indent indent)
    (match indent
      ((indent* ... indent)
       (append indent* (list (advance indent))))))

  (define (advance-indent/tab indent)
    (let* ((col (apply + indent))
           (col* (advance/tab col)))
      (match indent
        ((indent* ... indent)
         (append indent* (+ (- col* col) indent))))))

  (define (compare-indents prev new)
    (match (vector prev new)
      (#((x . prev-tail) (x . new-tail))
       (compare-indents prev-tail new-tail))
      (_
       (match (vector prev new)
         (#(() ()) 'same)
         (#((prev) (new)) (if (< prev new) 'inner 'outer))
         (#((prev . _) (new . _))
          (if (< prev new) 'inner-blockquote 'different-blockquote))
         (#(() _) 'inner-blockquote)
         (#(_ ()) 'outer-blockquote)
         (_ 'different-blockquote)))))

  ;; indent as list of blockquote

  ;; kup ::= (node type info indent) -> _
  ;; knext ::= nodelist -> node

  (define (drop-whitespace-up-to n col kt kf)
    (define col-end (+ col n))
    (let lp ((n n) (chars '()))
      (cond
       ((zero? n) (kt))
       (else
        (match (next)
          (#\space (lp (1- n) (cons #\space chars)))
          (#\tab
           (let ((col (advance/tab (- col-end n))))
             (cond
              ((<= col col-end)
               (lp (- col-end col) (cons #\tab chars)))
              (else (kt)))))
          (#\newline
           ;; Sure.  Trailing whitespace can be any indent.
           (unget1 #\newline)
           (kt))
          (#\return
           (lp n (cons #\return chars)))
          (ch
           (unless (eof-object? ch) (unget1 ch))
           (unget chars)
           (kf)))))))

  (define (drop-whitespace-then-blockquote n col kt kf)
    (define col-end (+ col n))
    (let lp ((n n) (kf kf))
      (let* ((ch (next))
             (kf (lambda () (unless (eof-object? ch) (unget1 ch)) (kf))))
        (cond
         ((zero? n)
          (match ch
            (#\> (kt kf))
            (_ (kf))))
         (else
          (match ch
            (#\space (lp (1- n) kf))
            (#\tab
             (let ((col (advance/tab (- col-end n))))
               (cond
                ((<= col col-end) (lp (- col-end col) kf))
                (else (kf)))))
            (_ (kf))))))))

  (define (consume-indent indent kt kf)
    (match indent
      ((0) (kt))
      (_
       (let lp ((indent indent) (col 0) (kf kf))
         (match indent
           ((n) (drop-whitespace-up-to n col kt kf))
           ((n m . indent)
            (let* ((indent (cons (1- m) indent))
                   (kt (lambda (kf) (lp indent (+ col n 1) kf))))
              (drop-whitespace-then-blockquote n col kt kf))))))))

  (define (read-pre tag indent k)
    (let lp ((body '()))
      (define (finish tail)
        (k (list 'pre tag (string-concatenate-reverse body tail))))
      (consume-indent
       indent
       (lambda ()
         (match (next-line-and-delim)
           ((str . delim)
            (cond
             ((eof-object? delim)
              (finish (if (eof-object? str) "" str)))
             ((string=? str "```")
              (finish ""))
             (else
              (lp (cons* (string delim) str body)))))))
       (lambda ()
         (finish "")))))

  (define (read-link-destination)
    (match (next-not-eof "link destination")
      (#\<
       (let lp ((chars '()))
         (match (next-not-eof "<>-delimited link")
           (#\> (reverse-list->string chars))
           (#\< (error "< inside <>-delimited link"))
           (#\\
            (match (next)
              ((? eof-object?) (lp (cons #\\ chars)))
              (ch (lp (cons ch chars)))))
           (ch (lp (cons ch chars))))))
      (ch
       (unget1 ch)
       (let lp ((chars '()) (k reverse-list->string))
         (match (next-not-eof "link destination")
           (#\) (unget1 #\)) (k chars))
           ((and ch
                 (or #\space #\)
                     (? (lambda (ch)
                          (or (char<? ch (integer->char #x20))
                              (char=? ch (integer->char #x7f)))))))
            (unget1 ch)
            (k chars))
           (#\(
            (lp (cons #\( chars)
                (lambda (chars)
                  (match (next-not-eof "link destination")
                    (#\) (lp (cons #\) chars) k))
                    (ch (error "unexpected char" ch))))))
           (#\\
            (lp (cons (next-not-eof "link-destination") chars) k))
           (ch (lp (cons ch chars) k)))))))

  (define (read-link indent continue)
    (read-text
     #f indent
     (lambda (ch)
       (match ch
         (#\]
          (lambda (text)
            (match (next)
              (#\(
               (let ((dest (read-link-destination)))
                 (match (next)
                   (#\)
                    (continue `(link ,dest . ,text)))
                   (ch
                    (error "unexpected after link dest" ch)))))
              (ch (error "link missing destination URL")))))
         (_ #f)))
     (lambda (elts)
       (error "end-of-block while reading link"))))

  (define (read-emph indent delim continue)
    (define (delim? ch) (eqv? ch delim))
    (match (next-not-eof "emphasis")
      ((? delim?)
       (let ((done? (lambda (ch)
                      (match ch
                        ((? delim?)
                         (match (next-not-eof "emphasis")
                           ((? delim?) continue)
                           (ch (unget1 ch) #f)))
                        (_ #f)))))
         (read-text 'strong indent done?
                    (lambda (elt)
                      (error "end of block while reading strong" elt)))))
      (ch
       (unget1 ch)
       (let ((done? (lambda (ch) (and (delim? ch) continue))))
         (read-text 'emph indent done?
                    (lambda (elt)
                      (error "end of block while reading emph" elt)))))))

  (define (read-code indent continue)
    (let lp ((chars '()))
      (match (next-not-eof "backticks")
        (#\` (continue `(code ,(reverse-list->string chars))))
        (#\return (lp chars))
        (#\newline
         (consume-indent
          indent
          (lambda ()
            (match (next-not-eof "code")
              ((or #\return #\newline)
               (error "end of block while reading code"))
              (ch (unget1 ch) (lp (cons #\space chars)))))
          (lambda () (error "end of block while reading code"))))
        (ch (lp (cons ch chars))))))

  (define (read-text tag indent done? on-block-end)
    (let lp ((elts '()))
      (define (continue elt) (lp (cons elt elts)))
      (define (finish kdone)
        (let lp ((elts elts) (out '()))
          (match elts
            (() (kdone (if tag (cons tag out) out)))
            (((? char? ch) . elts)
             (let lp2 ((elts elts) (chars (list ch)))
               (match elts
                 (((? char? ch) . elts)
                  (lp2 elts (cons ch chars)))
                 (_
                  (lp elts (cons (list->string chars) out))))))
            ((elt . elts)
             (lp elts (cons elt out))))))
      (define (consume-blank-lines-then-finish kdone)
        (let lp ()
          (match (next)
            ((? eof-object?) (finish kdone))
            (#\return (lp))
            (#\newline
             (consume-indent indent lp (lambda () (finish kdone))))
            (ch
             (unget1 ch)
             (finish kdone)))))
      (match (next)
        ((? eof-object?) (finish on-block-end))
        (#\return (lp elts))
        (#\newline
         (consume-indent
          indent
          (lambda ()
            (cond
             ((done? #\newline) => consume-blank-lines-then-finish)
             (else (lp (cons #\newline elts)))))
          (lambda ()
            (finish on-block-end))))
        ((= done? (and kdone (not #f))) (finish kdone))
        ((and (or #\_ #\*) delim) (read-emph indent delim continue))
        (#\` (read-code indent continue))
        (#\[
         (read-link indent continue))
        (#\<
         (unget1 #\<)
         (match (parse-one-xml-element port)
           (#f (lp elts))
           (elt (continue `(inline-xml ,elt)))))
        (#\\ (lp (cons (next-not-eof "backslash") elts)))
        (#\! (match (next)
               ((? eof-object?) (lp (cons #\! elts)))
               (#\[
                (read-link indent
                           (lambda (link)
                             (match link
                               (('link dest . alt)
                                (continue `(image ,dest . ,alt)))))))
               (ch
                (unget1 ch)
                (lp (cons #\! elts)))))
        (ch (lp (cons ch elts))))))

  (define (read-para indent kup knext)
    (define (make-continuation reader)
      (lambda (para)
        (reader indent kup (lambda (nodelist)
                             (knext (cons para nodelist))))))
    (define (done? ch)
      (match ch
        (#\newline
         (let lp ((ch (next)))
           (match ch
             ((? eof-object?)
              (lambda (para)
                (kup (knext (list para)) ch empty-indent)))
             (ch
              (read-block-type ch #t
                               make-continuation
                               (if (eqv? ch #\newline)
                                   (lambda (chars)
                                     (unget chars)
                                     (make-continuation read-para))
                                   (lambda (chars)
                                     (unget chars)
                                     #f)))))))
        (_ #f)))
    (read-text 'para indent done? (make-continuation read-block-list)))

  (define (read-para* chars indent kup knext)
    (unget chars)
    (read-para indent kup knext))

  (define (read-heading level indent continue)
    (let ((continue (lambda (heading)
                      (match heading
                        (('heading . body)
                         (continue `(heading ,level . ,body)))))))
      (read-text 'heading indent (lambda (ch)
                                   (and (eqv? ch #\newline) continue))
                 continue)))

  (define (read-li marker marker-indent marker-size kup knext)
    (define list-tag
      (match marker
        ((? number?) 'enumerate)
        ((? char?) 'itemize)))
    (define (list-tag? tag) (eq? tag list-tag))
    (read-indent
     (lambda (ch indent)
       (match indent
         ((outer . inner)
          (match marker-indent
            ((marker-outer ... marker-inner)
             (let ((body-indent
                    (append marker-outer
                            (list (+ marker-inner marker-size outer))
                            inner)))
               (read-block
                ch body-indent
                (lambda (blocks ch next-indent)
                  (read-indented-block
                   ch marker-indent next-indent kup
                   (lambda (nodelist)
                     (knext
                      (match nodelist
                        ((((? list-tag?) . items) . nodelist)
                         `((,list-tag (item . ,blocks) . ,items) . ,nodelist))
                        (_
                         `((,list-tag (item . ,blocks)) . ,nodelist)))))))
                identity)))))))))

  (define (read-block-type ch in-text? kblock ktext)
    (define (make-continue indent kup knext)
      (lambda (block)
        (read-block-list indent kup
                         (lambda (nodelist)
                           (knext (cons block nodelist))))))
    (match ch
      (#\#
       (let lp ((level 1))
         (match (next-not-eof "heading")
           (#\#
            (lp (1+ level)))
           (#\space
            (kblock
             (lambda (indent kup knext)
               (read-heading level indent (make-continue indent kup knext)))))
           (#\return (lp level))
           (#\newline
            (kblock
             (lambda (indent kup knext)
               ((make-continue indent kup knext) `(heading ,level)))))
           (ch
            (ktext (cons ch (make-list level #\#)))))))
      (#\`
       (match (next)
         ((? eof-object?) (ktext '(#\`)))
         (#\`
          (match (next)
            ((? eof-object?) (ktext '(#\` #\`)))
            (#\`
             (kblock
              (lambda (indent kup knext)
                (match (next-line-and-delim)
                  ((tag . delim)
                   (cond
                    ((eof-object? delim)
                     (error "eof while reading code block"))
                    (else
                     (read-pre (if (string-null? tag) #f tag) indent
                               (make-continue indent kup knext)))))))))
            (ch
             (ktext (list ch #\` #\`)))))
         (ch
          (ktext (list ch #\`)))))
      ((or #\- #\* #\+)
       (match (peek)
         ((or #\space #\tab)
          (kblock (lambda (indent kup knext)
                    (read-li ch indent 1 kup knext))))
         (_
          (ktext (list ch)))))
      ((or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (let lp ((chars (list ch)))
         (let ((ch (next)))
           (match ch
             ((? eof-object?) (ktext chars))
             ((or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
              (lp (cons ch chars)))
             ((or #\. #\))
              ;; fixme: record delimiter
              (match (next)
                ((? eof-object?)
                 (ktext (cons ch chars)))
                ((and ch2 (or #\space #\tab))
                 (unget1 ch2)
                 (if (or (not in-text?) (equal? chars '(#\1)))
                     (kblock
                      (lambda (indent kup knext)
                        (let ((idx (fold (lambda (ch idx)
                                           (+ (* idx 10)
                                              (- (char->integer ch)
                                                 (char->integer #\0))))
                                         0 chars)))
                          (read-li idx indent (1+ (length chars))
                                   kup knext))))
                     (ktext (cons ch chars))))
                (ch2
                 (ktext (cons* ch2 ch chars)))))
             (_
              (ktext (cons ch chars)))))))
      (#\<
       (unget1 #\<)
       (if in-text?
           (ktext '())
           (kblock
            (lambda (indent kup knext)
              (match (parse-one-xml-element port)
                (#f (read-block-list indent kup knext))
                (elt ((make-continue indent kup knext) `(block-xml ,elt))))))))
      (#\return
       (read-block-type (next-not-eof "newline") in-text? kblock ktext))
      (#\newline
       ;; fixme: record loose li
       (kblock read-block-list))
      ((? eof-object?)
       (kblock
        (lambda (indent kup knext)
          (kup (knext '()) ch empty-indent))))
      (_
       (ktext (list ch)))))

  (define (read-block ch indent kup knext)
    (define (have-block read-block)
      (read-block indent kup knext))
    (define (have-text chars)
      (read-para* chars indent kup knext))
    (read-block-type ch #f have-block have-text))

  (define (finish-block-list blocks)
    (match blocks
      ((block) block)
      (_ `(begin . ,blocks))))
  (define (finish-block-quote blocks)
    (match blocks
      ((block) block)
      (_ `(blockquote . ,blocks))))

  (define (read-indented-block ch outer-indent indent kup knext)
    (define (recurse finish recurse-indent)
      (read-indented-block ch recurse-indent indent
                           (lambda (nested ch indent)
                             (read-indented-block ch outer-indent indent
                                                  kup
                                                  (lambda (nodelist)
                                                    (knext (cons nested nodelist)))))
                           finish))
    (match (compare-indents outer-indent indent)
      ('same
       (read-block ch indent kup knext))
      ((or 'outer
           'outer-blockquote
           'different-blockquote)
       (kup (knext '()) ch indent))
      ('inner
       (recurse finish-block-list indent))
      ('inner-blockquote
       (recurse finish-block-quote
                (list-head indent (1+ (length outer-indent)))))))

  (define (parse-error reason)
    (error reason))

  (define (read-block-list outer-indent kup knext)
    (read-indent
     (lambda (ch indent)
       (read-indented-block ch outer-indent indent kup knext))))

  (read-block-list empty-indent
                   (lambda (nodelist ch indent) nodelist)
                   finish-block-list))

(define* (smarxdown->shtml exp #:key
                           (heading-offset 0)
                           (handle-inline-xml identity)
                           (handle-block-xml identity))
  (define (transform-inline exp)
    (match exp
      (('inline-xml xml) (handle-inline-xml xml))
      (('code . body) `(tt . ,body))
      (('emph . body) `(i . ,(map transform-inline body)))
      (('strong . body) `(b . ,(map transform-inline body)))
      (('link dest . body) `(a (@ (href ,dest)) . ,(map transform-inline body)))
      ((? string? str) str)))
  (define (transform-block exp)
    (match exp
      (('block-xml xml) (handle-block-xml xml))
      (('para . body) `(p . ,(map transform-inline body)))
      (('blockquote . body) `(blockquote . ,(map transform-block body)))
      (('itemize ('item . item) ...)
       `(ul . ,(map (lambda (blocks)
                      `(li . ,(map transform-block blocks)))
                    item)))
      (('enumerate ('item . item) ...)
       `(ol . ,(map (lambda (blocks)
                      `(li . ,(map transform-block blocks)))
                    item)))
      (('pre #f . body) `(pre . ,body))
      (('pre info . body) `(pre (@ (class ,(string-append "pre-" info)))
                                . ,body))
      (('heading level . body)
       (let* ((level (+ level heading-offset))
              (tag (cond ((<= level 0) 'h1)
                         ((<= level 6)
                          (string->symbol
                           (string #\h (integer->char
                                        (+ level (char->integer #\0))))))
                         (else 'h6))))
         `(,tag . ,(map transform-inline body))))))
  (match exp
    (('begin exp ...)
     `(div . ,(map transform-block exp)))))
