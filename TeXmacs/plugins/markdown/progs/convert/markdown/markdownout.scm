;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : markdownout.scm
;; DESCRIPTION : markdown-stree to markdown-document or markdown-snippet
;; COPYRIGHT   : (C) 2017 Ana Cañizares García and Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert markdown markdownout)
  (:use (convert tools output) (convert markdown markdown-utils)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (hugo-extensions?)
  (== (get-preference "texmacs->markdown:flavour") "hugo"))

(define (author-by)
  (string-append (md-translate "By") ":"))

(define (prelude)
  "Output Hugo frontmatter"
  (if (hugo-extensions?)
      (with front (md-get 'frontmatter)
        (ahash-set! front "math" "true")
        (when (nnull? (md-get 'doc-authors))
          (ahash-set! front "authors"
                      `(tuple ,@(reverse (md-get 'doc-authors)))))
        (when (nnull? (md-get 'refs))
          (ahash-set! front "refs"
                      `(tuple ,@(list-remove-duplicates (md-get 'refs)))))
        (string-append
         "---"
         (serialize-yaml `(dict ,@(assoc->list (ahash-table->list front))))
         "\n---\n"))
      ""))

(define (postlude-add x)
  (cond ((func? x 'footnote)
         (md-set 'postlude
           (string-concatenate
            `(,(md-get 'postlude)
               "\n[^" ,(number->string (md-get 'footnote-nr)) "]: "
               ,@(md-map serialize-markdown* (cdr x))
               "\n"))))
        ((string? x)
         (md-set 'postlude (string-append (md-get 'postlude) "\n" x)))
        (else 
          (debug-message "convert-error" "postlude-add: bogus input")
          (noop))))

(define (postlude)
  (md-get 'postlude))

(define (indent-increment sn)
  "Increments indentation either by a number of spaces or a fixed string"
  (string-append (md-get 'indent)
    (if (number? sn) (string-concatenate (make-list sn " ")) sn)))

(define (indent-decrement n)
  (if (> (string-length (md-get 'indent)) n)
      (string-drop (md-get 'indent) n)
      ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown to string serializations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (keep x)
  (cons (car x) (md-map serialize-markdown* (cdr x))))

(define (skip x)
  (string-concatenate (md-map serialize-markdown* (cdr x))))

(define (md-markdown x)
  (if (tm-is? x 'markdown)
      (serialize-markdown* (cdr x))
      (begin 
        (debug-message "convert-error" "Invalid markdown tree representation")
        "")))

(define (md-translate x)
  (cond ((null? x) "")
        ((string? x)
         (translate-from-to x "english" (md-get 'language)))
        ((func? x 'localize) (md-translate (cadr x)))
        ((list? x) (serialize-markdown* x))
        (else x)))

(define (md-doc-title x)
  (with title (serialize-markdown* (cdr x))
    (if (hugo-extensions?)
        (serialize-markdown* `(hugo-front "title" ,title))
        (serialize-markdown* `(document ,title "")))))

(define (md-doc-subtitle x)
  (with subtitle (serialize-markdown* (cdr x))
    (if (hugo-extensions?)
        (serialize-markdown* `(hugo-front "subtitle" ,subtitle))
        (serialize-markdown* `(document ,subtitle)))))

(define (md-doc-author x)
  ; TODO? We might want to extract other info
  (with name (select x '(:* author-name))
    (if (nnull? name)
        (if (hugo-extensions?)
            (md-set 'doc-authors (cons (cadar name) (md-get 'doc-authors)))
            (string-append (author-by) (force-string (cdar name))))))
  "")

(define (decode-date date formats)
  ; Without srfi-19 date parsing, return the date string as-is.
  ; Hugo accepts a variety of date string formats.
  (force-string date))

(define (md-doc-date x)
  (with date (decode-date (cadr x) '("~B~d~Y" "~d~B~Y"))
    (if (hugo-extensions?)
        (md-hugo-frontmatter `(hugo-front "date" ,date))
        date)))

(define (md-abstract x)
  (if (hugo-extensions?)
      (with-md-globals 'paragraph-width #f
        (md-hugo-frontmatter
           `(hugo-front "summary" ,(serialize-markdown* (cdr x)))))
      (md-paragraph `(concat (strong "Abstract: ") (em ,(cdr x))))))

(define (must-adjust? t)
  (tm-in? t '(strong em tt strike math concat cite cite-detail
              eqref reference figure hlink hrule)))

(define (md-paragraph x)
  ;; TODO: Hugo shortcode calls that appear inside a (concat ...) will have
  ;; adjust-width applied to them, potentially splitting {{< ... >}} across
  ;; lines. Fix would require safe-split to treat {{< ... >}} as one token.
  (with adjust
      (cut adjust-width
           <> (md-get 'paragraph-width) (md-get 'indent) (md-get 'first-indent))
     (cond ((string? x) (adjust x))
           ((must-adjust? x) (adjust (serialize-markdown* x)))
           (else (serialize-markdown* x)))))

(define (md-document x)
  (string-concatenate
   (list-intersperse (md-map md-paragraph (cdr x))
                     (make-string (md-get 'num-line-breaks) #\newline))))

(define (md-concat x)
  ; HACK: labels in sections will typically look like
  ;    (concat (section "Section one") (label "section-one"))
  ; But this will include a span in the md header, which then e.g. Hugo's 
  ; .TableOfContents will copy to the TOC hence producing two identical ids
  ; in the document. So we split concats in two lines
  (if (and (>= (length x) 3)
           (tuple? (second x))
           (>= (length (second x)) 2)
           (in? (car (second x)) '(h1 h2 h3)))
      (with-md-globals 'num-line-breaks 1
        (serialize-markdown* `(document ,@(cdr x))))
      (string-concatenate (md-map serialize-markdown* (cdr x)))))

(define (md-header n)
  (lambda (x)
    (with-md-globals 'num-line-breaks 0
      (string-concatenate
       `(,@(make-list n "#") " " ,@(md-map serialize-markdown* (cdr x)))))))

(define (md-para x)
  "TeXmacs <paragraph> tag"
  (serialize-markdown* `(concat (strong ,@(cdr x)) " ")))

(define (md-environment x style)
  (let* ((txt (md-translate (second x)))
         (extra (if (and (string? (third x)) (string-null? (third x))) ""
                    `(concat " " ,(third x))))
         (content (cdr (fourth x)))  ; content of inner 'document
         (tag `(strong (concat ,txt ,extra ":"))))
    (serialize-markdown*
     (if (list>1? content)
         `(document (concat ,tag " " (,style ,(car content)))
                    (em (document ,@(cdr content))))
         `(document (concat ,tag " " (,style ,(car content))))))))

(define (md-environment* x style)
  (md-environment (list (first x) (second x) "" (third x)) style))

(define (md-make-environment style)
  (lambda (x) (md-environment x style)))

(define (md-make-environment* style)
  (lambda (x) (md-environment* x style)))

(define (md-dueto x)
  (serialize-markdown*
   `(concat " " (strong (concat "(" ,(cadr x) ")")) " ")))

(define (escape-md-symbols line)
  "Escapes special markdown chars at the beginning of lines"
  ; This is used for equations since they are rendered as `document,
  ; which is not processed by adjust-width, where these symbols are
  ; taken into account when splitting words
  (with result (md-special-line-start? line)
    (if (not result) line
        (string-append (second result) "\\" (third result) (fourth result)))))

(define (md-math x . paragraph-width)
 "Takes a latex stree @x, and returns a valid MathJax-compatible LaTeX string"
 ; Set line length for latex output. Used for display math only (equations):
 ; inline math is rendered as concat which is adjusted later.
 (with save (output-set-line-length
             (if (nnull? paragraph-width) (car paragraph-width) 9999))
   (with ltx (serialize-latex (second x))
     (output-set-line-length save)
     (if (nnull? paragraph-width) ltx (string-replace ltx "\n" " ")))))

(define (md-span content . args)
  (string-append
   "<span " 
   (string-recompose-space (md-map assoc->html-attr args)) 
   ">" 
   (serialize-markdown* content)
   "</span>"))

(define (create-label-link label . extra-attrs)
  (with clean-label (sanitize-selector label)
    (apply md-span "" (append `((id . ,clean-label)) extra-attrs))))

(define (find-latex-label ltx)
  "Find \\label{...} in a LaTeX string and return the label name, or #f"
  (let* ((prefix "\\label{")
         (plen (string-length prefix))
         (pos (string-search-forwards prefix 0 ltx)))
    (if (< pos 0) #f
        (let* ((start (+ pos plen))
               (end (string-search-forwards "}" start ltx)))
          (if (< end 0) #f
              (substring ltx start end))))))

(define (create-equation-link ltx)
  "Returns an empty anchor for every label in the latex line"
  (with label (find-latex-label ltx)
    (if (not label) ""
        (create-label-link label '(class . "tm-eqlabel")))))

(define (md-equation x)
  (let*  ((s1 (md-math x (md-get 'paragraph-width)))
          (s2 (string-replace s1 "\\[" "\\\\["))
          (s3 (string-replace s2 "\\]" "\\\\]"))
          (s4 (string-split s3 #\newline))
          (s5 (map escape-md-symbols s4))
          (anchors (string-concatenate (map create-equation-link s5)))
          (lines (if (string-null? anchors) s5 (cons anchors s5))))
     (with-md-globals 'num-line-breaks 1
       (serialize-markdown* `(document ,@lines)))))

(define (md-numbered-equation x)
  (md-equation x))

(define (md-equation-table x)
  "Renders equation*-table (equation* containing a tabular) as \\\\[ \\\\begin{array}...\\\\end{array} \\\\].
serialize-latex provides \\\\ row separators; we double them to \\\\\\\\ before
applying the \\\\[ and \\\\] escaping."
  (let* ((s1 (md-math x (md-get 'paragraph-width)))
         (s2 (string-replace s1 "\\\\" "\\\\\\\\"))
         (s3 (string-replace s2 "\\[" "\\\\["))
         (s4 (string-replace s3 "\\]" "\\\\]"))
         (s5 (string-split s4 #\newline))
         (s6 (map escape-md-symbols s5))
         (anchors (string-concatenate (map create-equation-link s6)))
         (lines (if (string-null? anchors) s6 (cons anchors s6))))
    (with-md-globals 'num-line-breaks 1
      (serialize-markdown* `(document ,@lines)))))

(define (md-labels x)
  (md-set 'labels (list->ahash-table (cadr x)))
  "")

(define (md-label x)
  (create-label-link (serialize-markdown* (cadr x))))

(define (md-eqref x)
  (let* ((label (serialize-markdown* (cadr x)))
         (err-msg (string-append "undefined label: '" label "'"))
         (label-display (or (ahash-ref (md-get 'labels) label) err-msg)))
    (serialize-markdown*
      `(hlink ,(string-append "(" label-display ")")
              ,(string-append "#" label)))))

(define (md-reference x)
  (let* ((label (serialize-markdown* (cadr x)))
         (err-msg (string-append "undefined label: '" label "'"))
         (label-display (or (ahash-ref (md-get 'labels) label) err-msg)))
    (serialize-markdown*
     `(hlink ,label-display ,(string-append "#" label)))))

(define (md-item x)
  (md-get 'item))

(define (md-subpara x)
  "Renders a continuation paragraph inside a list item with proper indent"
  (let* ((indent (md-get 'indent))
         (content (cadr x)))
    (cond ((string? content)
           (adjust-width content (md-get 'paragraph-width) indent indent))
          ((must-adjust? content)
           (adjust-width (serialize-markdown* content) (md-get 'paragraph-width) indent indent))
          (else
           (string-append indent (serialize-markdown* content))))))

(define (md-tm-description-item x)
  "Renders '(tm-description-item label body)"
  (let* ((title (serialize-markdown* (second x)))
         (body-str (serialize-markdown* (third x))))
    (if (hugo-extensions?)
        (string-append "{{% description-item title="
                       (string-quote title) " %}}\n"
                       body-str "\n"
                       "{{% /description-item %}}")
        (string-append "**" title "** " body-str))))

(define (md-tm-description x)
  "Renders '(tm-description (document item1 item2 ...))"
  (let ((content (serialize-markdown* (second x))))
    (if (hugo-extensions?)
        (string-append "{{% description %}}\n"
                       content "\n"
                       "{{% /description %}}")
        content)))

(define (md-folded x)
  "Hugo extension: collapsible block. Intermediate form: (folded summary body)"
  (if (hugo-extensions?)
      (let ((summary (serialize-markdown* (second x)))
            (body (serialize-markdown* (third x))))
        (string-append "{{% details %}}\n"
                       summary "\n"
                       "<!-- split -->\n"
                       body "\n"
                       "{{% /details %}}"))
      (serialize-markdown* (second x))))

(define (md-tm-env x)
  "Hugo extension: theorem-like environment as {{% env %}} shortcode.
   Intermediate form: (tm-env type number-or-#f title-or-#f style body)"
  (let* ((env-type (second x))
         (number (third x))
         (title (fourth x))
         (style (list-ref x 4))
         (body (list-ref x 5)))
    (if (hugo-extensions?)
        (let* ((title-str (if (and title (string? title)) title
                              (if title (serialize-markdown* title) #f)))
               (params (string-append
                        "type=" (string-quote env-type)
                        (if number
                            (string-append " number=" (string-quote number))
                            "")
                        (if (and title-str (not (string-null? title-str)))
                            (string-append " title=" (string-quote title-str))
                            "")
                        (if (string=? style "plain") " style=\"plain\"" "")))
               (content (serialize-markdown* body)))
          (string-append "{{% env " params " %}}\n"
                         content "\n"
                         "{{% /env %}}"))
        (serialize-markdown* body))))

(define (md-algo-block x)
  "Serializes (algo-block header closing body)."
  (let* ((header (serialize-markdown* (second x)))
         (closing (third x))
         (body (fourth x))
         (closing-str (if closing (serialize-markdown* closing) #f)))
    (if (hugo-extensions?)
        (let ((body-str (serialize-markdown* body)))
          (string-append header "\n"
                         "{{% indent %}}\n"
                         body-str "\n"
                         "{{% /indent %}}"
                         (if closing-str (string-append "\n" closing-str) "")))
        (let ((body-str (with-md-globals 'indent (indent-increment 2)
                          (with-md-globals 'first-indent (md-get 'indent)
                            (serialize-markdown* body)))))
          (string-append header "\n"
                         body-str
                         (if closing-str (string-append "\n" closing-str) ""))))))

(define (md-algo-line x)
  "Serializes (algo-line content)."
  (serialize-markdown* (second x)))

(define (md-algo-indent x)
  "Serializes (algo-indent body). Hugo: {{% indent %}} shortcode. Vanilla: 2-space indent."
  (let ((body (second x)))
    (if (hugo-extensions?)
        (let ((body-str (serialize-markdown* body)))
          (string-append "{{% indent %}}\n"
                         body-str "\n"
                         "{{% /indent %}}"))
        (with-md-globals 'indent (indent-increment 2)
          (with-md-globals 'first-indent (md-get 'indent)
            (serialize-markdown* body))))))

(define (md-tm-algorithm x)
  "Hugo extension: algorithm environment as {{% algorithm %}} shortcode.
   Intermediate form: (tm-algorithm number-or-#f title-or-#f body)"
  (let* ((number (second x))
         (title (third x))
         (body (fourth x))
         (title-str (if (and title (string? title)) title
                        (if title (serialize-markdown* title) #f)))
         (params (string-append
                  (if number (string-append "number=" (string-quote number)) "")
                  (if (and title-str (not (string-null? title-str)))
                      (string-append (if number " " "")
                                     "title=" (string-quote title-str))
                      "")))
         (content (serialize-markdown* body)))
    (if (hugo-extensions?)
        (string-append "{{% algorithm"
                       (if (string-null? params) "" (string-append " " params))
                       " %}}\n"
                       content "\n"
                       "{{% /algorithm %}}")
        (serialize-markdown* body))))

(define (is-item? x)
  (nnull? (select x '(:%0 item))))

(define (is-item-subparagraph? x)
  "#t if @x is a (text) subparagraph of an item. Excludes subitemizes and others."
  (not (or (symbol? x)
           (stree-contains? x '(itemize enumerate quotation item)))))

(define (add-paragraphs-after-items l indent)
  "Adds empty lines in items with multiple paragraphs"
  ; paragraphs inside an itemize but don't begin with an (item) are
  ; considered part of the previous item.
  (with transform
      (lambda (x acc)
        (append acc (if (is-item-subparagraph? x)
                        (list "" `(md-subpara ,x))
                        (list x))))
  (list-fold transform '() l)))

(define (md-list x)
  (let ((c (cond ((== (car x) 'itemize) "* ")
                 ((== (car x) 'enumerate) "1. ")
                 (else "* "))))
    (with-md-globals 'num-line-breaks 1
      (with-md-globals 'item c
        (with-md-globals 'indent (indent-increment (string-length c))
          (with-md-globals 'first-indent (indent-decrement (string-length c))
            (serialize-markdown*
             (add-paragraphs-after-items
              (cadr x)
              (string-concatenate (make-list (string-length c) " "))))))))))

(define (md-quotation x)
  (with-md-globals 'num-line-breaks 1
    (with-md-globals 'indent (indent-increment "> ")
      (with-md-globals 'first-indent (md-get 'indent)
        (serialize-markdown* (cdr x))))))

(define (md-style-text style)
 (cond ((== style 'strong) "**")
       ((== style 'em) "*")
       ((== style 'tt) (md-encoding->tm-encoding "`" (md-get 'file?)))
       ((== style 'strike) "~~")
       ; TODO: Hugo shortcode?
       ((== style 'underline) "")
       (else "")))

(define (md-style-inner st x)
  (let* ((style (md-style-text st))
         (content* (serialize-markdown* x))
         (left (if (string-starts? content* " ") " " ""))
         (right (if (string-ends? content* " ") " " ""))
         (content (string-trim-spaces content*)))
    (cond ((string-null? content) "")
          ((string-punctuation? content) content)
          (else
            (string-concatenate
             (list left style content style right))))))

;;;;;;;;;;;;;;
; TODO: Move this style preprocessing to tmmarkdown where it belongs
; architecturally. Doing so would also enable idempotent styles (em of em = no
; style). Requires a larger refactor of the tmmarkdown → markdownout boundary.

(define md-style-tag-list '(em strong tt strike underline))
(define md-style-drop-tag-list
  '(marginal-note marginal-note* footnote footnote* label item
    equation equation* equation*-table eqnarray eqnarray* math
    folded tm-env tm-algorithm algo-block algo-indent algo-line
    tm-description tm-description-item))
(define md-stylable-tag-list '(document itemize enumerate quotation))
; Tags NOT included here by design:
;   theorem/lemma/etc. never appear (tmmarkdown converts them to std-env/plain-env)
;   std-env/plain-env have structured children ("Name" "number" content) so
;   distributing a style into them would incorrectly wrap the name and number strings

(define-public (add-style-to st x)
  "Recurses into children of @x inserting its tag where necessary."
  (cond ((string? x)
         `(,st ,x))
        ((== st (first x))  ; idempotent: drop repeated styles
         (add-style-to st (cadr x)))
        ((tm-in? x md-stylable-tag-list)
         `(,(first x) ,@(map (cut add-style-to st <>) (cdr x))))
        ((and (tm-in? x md-style-tag-list) 
              (not (tm-in? (second x) md-style-tag-list)))
         `(,(first x) ,@(map (cut add-style-to st <>) (cdr x))))
        ((tm-in? x md-style-drop-tag-list)
         x)
        ((is-item? x)
         `(concat ,@(map (cut add-style-to st <>) (cdr x))))
        (else
          `(,st ,x))))

(define (md-style x)
  (let* ((st (car x))
         (content (cadr x)))
    (cond ((md-get 'disable-styles)
           (serialize-markdown* content))
          ((tm-in? content md-style-drop-tag-list)
           (serialize-markdown* content))
          ((tm-in? content md-stylable-tag-list)
           (with styled (add-style-to st content)
             (serialize-markdown* styled)))
          (else
            (md-style-inner st content)))))

(define (md-cite x)
  "Custom hugo {{<cite>}} shortcode"
  (if (not (hugo-extensions?)) ""
      (with citations 
          (list-filter (cdr x) (lambda (x) (and (string? x) (not (string-null? x)))))
        (md-set 'refs (append (md-get 'refs) citations))
        (md-hugo-shortcode* 
         (cons 'cite (map (lambda (s) `(#f . ,s)) citations))))))

(define (md-cite-detail x)
  (if (not (hugo-extensions?)) ""
      (with detail (serialize-markdown* (cddr x))
        (string-append (md-cite `(cite ,(cadr x))) " (" detail ")"))))

(define (md-hlink x)
  (with payload (cdr x)
    (string-append "[" (serialize-markdown* (first payload)) "]"
                   "(" (force-string (second payload)) ")")))    

(define (md-image x)
  (let* ((payload (cdr x))
         (src (first payload))
         (alt (if (list-2? payload) (second payload) "")))
    (string-append "![" alt "](" (force-string src) ")")))

(define (md-figure type . extra-args)
  (lambda (x)
    (let* ((args (assoc-extend (cdr x) extra-args))
           (name (assoc-default args 'name ""))
           (caption (assoc-default args 'caption ""))
           (body (assoc-default args 'body '())))
      (if (hugo-extensions?)
          (begin
            (set! args (assoc-remove-many args '(body name caption)))
            (set! args (assoc-append? args 'class (md-get 'html-class)))
            (md-hugo-shortcode* `(,type ,@args)
                                `(document ,body (concat ,name ,caption))))
          (with content (if (assoc 'src args)
                            `(image ,(assoc-ref args 'src) ,body)
                            body)
            (serialize-markdown*
             `(document ,body (concat ,name ,caption))))))))

(define (md-footnote x)
  ; Input: (footnote (document [stuff here]))
  (md-set 'footnote-nr (+ 1 (md-get 'footnote-nr)))
  (with-md-globals 'num-line-breaks 0
    (with-md-globals 'indent ""
      (with-md-globals 'paragraph-width #f
        (postlude-add x)
        (string-append "[^" (number->string (md-get 'footnote-nr)) "]")))))

(define (md-todo x)
  (md-span (serialize-markdown* (cdr x)) `(class . "todo")))

(define (md-block x)
  (with-md-globals 'num-line-breaks 1
    (with-md-globals 'paragraph-width #f
      (let ((syntax (second x))
            (backquotes (md-encoding->tm-encoding "```" (md-get 'file?))))
        (string-concatenate
         `(,backquotes ,syntax
           "\n"
           ,@(md-map serialize-markdown* (cddr x))
           "\n"
           ,backquotes))))))

(define (md-hugo-frontmatter x)
  (if (odd? (length (cdr x)))
      (debug-message "convert-error"
                     "ERROR: frontmatter tag must have even number of entries")
      (when (hugo-extensions?)
        (with set-pair! (lambda (kv)
                          (ahash-set! (md-get 'frontmatter) (car kv) (cdr kv)))
          (map set-pair! (list->assoc (cdr x))))))
  "")

(define (md-hugo-shortcode x)
  "Processes '(hugo-short shortcode-name (args)) where args is a list of tuples (name val). For unnamed arguments, use (#f val)"
  (md-hugo-shortcode* (cdr x)))

(define (md-hugo-shortcode* x . inner)
  "Inner processing of shortcodes"
  (if (not (md-get 'disable-shortcodes))
      (let ((shortcode (symbol->string (car x)))
            (args (cdr x))
            (content (if (null? inner) ""
                         (string-append (serialize-markdown* (car inner))
                                        "{{</" (symbol->string (car x)) ">}}"))))
        (tm-string-trim-both
         (string-append
          (string-recompose-space
          `("{{<" ,shortcode ,@(map assoc->html-attr args) ">}}"))
          content)))
      ""))

(define (md-toc x)
  (if (hugo-extensions?)
      "{{< toc >}}"
      "Table of contents not implemented for raw Markdown"))

(define (md-bibliography x)
  (if (hugo-extensions?) 
      (md-hugo-shortcode* '(references))
      (md-style '(strong "Bibliography not implemented for raw Markdown"))))

(define (md-sidenote-sub x numbered?)
  (if (hugo-extensions?)
      (let ((numbered (if numbered? '((numbered . "numbered")) '()))
            (args (cdr x)))
        (md-hugo-shortcode*
         (append `(sidenote (halign . ,(md-marginal-style (first args)))
                            (valign . ,(md-marginal-style (second args))))
                 numbered)
         (third args)))
      (serialize-markdown* `(footnote ,(third (cdr x))))))

(define (md-sidenote x)
  (md-sidenote-sub x #t))

(define (md-sidenote* x)
  (md-sidenote-sub x #f))

(define (md-explain-macro x)
  (let ((inner (with-md-globals 'disable-styles #t 
                 (md-map serialize-markdown* (cdr x)))))
    (md-style
      `(tt ,(string-append "<" (string-recompose inner "|") ">")))))

(define (md-tmdoc-copyright x)
  (with args (cdr x)
    (serialize-markdown*
     `(concat "---\n" "(C) " ,(first args)
              " by " ,(string-recompose-comma (cdr args))))))

(define (md-hrule x) "---")

(define (md-session x)
  "Serialize (tm-session lang items...) as a REPL-style fenced code block."
  (let* ((lang (second x))
         (items (cddr x))
         (backquotes (md-encoding->tm-encoding "```" (md-get 'file?))))
    (with-md-globals 'num-line-breaks 1
      (with-md-globals 'paragraph-width #f
        (let* ((lines
                (apply append
                       (map (lambda (item)
                              (cond
                                ((func? item 'session-in)
                                 (list (string-append (second item) (third item))))
                                ((func? item 'session-io)
                                 (let* ((code (string-append (second item) (third item)))
                                        (out (cadddr item)))
                                   (if (string-nnull? out)
                                       (list code out)
                                       (list code))))
                                (else '())))
                            items)))
               (content (string-recompose lines "\n")))
          (string-concatenate
           `(,backquotes ,lang "\n" ,content "\n" ,backquotes)))))))

(define (md-cwith-list tformat)
  (if (func? tformat 'tformat)
      (filter (cut func? <> 'cwith 6) (cDr tformat))
      '()))

(define (md-detect-colsep tformat)
  "Return 1-based column index with cell-rborder 1ln, or #f"
  (let loop ((specs (md-cwith-list tformat)))
    (cond ((null? specs) #f)
          ((let* ((s (car specs))
                  (c1 (list-ref s 3)) (c2 (list-ref s 4))
                  (prop (list-ref s 5)) (val (list-ref s 6)))
             (and (== prop "cell-rborder") (== val "1ln") (== c1 c2)))
           (string->number (list-ref (car specs) 3)))
          (else (loop (cdr specs))))))

(define (md-detect-header tformat)
  "Return #t if row 1 has cell-bborder 1ln"
  (let loop ((specs (md-cwith-list tformat)))
    (cond ((null? specs) #f)
          ((let* ((s (car specs))
                  (r1 (list-ref s 1)) (r2 (list-ref s 2))
                  (prop (list-ref s 5)) (val (list-ref s 6)))
             (and (== prop "cell-bborder") (== val "1ln") (== r1 "1") (== r2 "1")))
           #t)
          (else (loop (cdr specs))))))

(define (md-cell->string c)
  (if (func? c 'cell)
      (string-trim-spaces
       (string-concatenate (md-map serialize-markdown* (cdr c))))
      ""))

(define (md-row->gfm row)
  (if (func? row 'row)
      (string-append "| "
                     (string-recompose (map md-cell->string (cdr row)) " | ")
                     " |")
      ""))

(define (md-tabular-hugo x)
  (let* ((tformat (cdr x))
         (table   (if (func? tformat 'tformat) (cAr tformat) tformat))
         (rows    (if (func? table 'table) (cdr table) '()))
         (ncols   (if (null? rows) 0 (length (cdr (car rows)))))
         (sep     (apply string-append (cons "|" (make-list ncols "---|"))))
         (colsep  (md-detect-colsep tformat))
         (open    (string-append "{{% tmtable"
                                 (if colsep
                                     (string-append " colsep=\""
                                                    (number->string colsep) "\"")
                                     "")
                                 " %}}\n")))
    (if (null? rows) ""
        (string-append
         open
         (md-row->gfm (car rows)) "\n"
         sep "\n"
         (string-recompose (map md-row->gfm (cdr rows)) "\n")
         "\n{{% /tmtable %}}"))))

(define (md-tabular x)
  (if (hugo-extensions?)
      (md-tabular-hugo x)
      (if (== "html" (get-preference "texmacs->markdown:table-format"))
          (let ((opts '(("texmacs->html:css" . "on")
                        ("texmacs->html:mathjax" . "on")
                        ("texmacs->html:mathml" . "off")
                        ("texmacs->html:images" . "on"))))
            (serialize-html (texmacs->html (maybe-rewrap-html-class (cdr x)) opts)))
          (serialize-markdown* `(document "Tables not implemented for raw markdown")))))

(define (md-html-class x)
  (with-md-globals 'html-class (second x)
    (serialize-markdown* (third x))))

(define-macro (maybe-rewrap-html-class . body)
  "Wraps in 'html-class tag if we saw one earlier and discarded it"
  `(with cl (md-get 'html-class)
     (if (string-nnull? cl) 
         (list 'html-class cl ,@body)
         ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEPRECATED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (paper-author-add x)
  "PaperWhy extension DEPRECATED"
  (if (hugo-extensions?)
      (md-hugo-frontmatter `(hugo-front "paper-authors" (tuple ,@(cdr x))))
      ""))

(define (md-hugo-tags x)
  "hugo-tags DEPRECATED, use `(hugo-front tags `(tuple tag1 tag2 ...)) "
  (if (hugo-extensions?)
      (md-hugo-frontmatter `(hugo-front "tags" (tuple (cdr x))))
      ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dispatch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (serialize-markdown* x)
;   (display* "Serialize: " x "\n")
  (cond ((string? x) x)
        ((char? x) (char->string x))
        ((symbol? x) "")
        ((and (nnull? x) (symbol? (car x)))
         (with fun (or (ahash-ref serialize-hash (car x)) skip)
           (fun x)))
        (else
          (string-concatenate (md-map serialize-markdown* x)))))

(define serialize-hash (make-ahash-table))
(for-each (lambda (l) (apply (cut ahash-set! serialize-hash <> <>) l))
          (list
      (list 'abstract md-abstract)
      (list 'bibliography md-bibliography)  ; TfL extension
      (list 'big-figure
        (md-figure 'tmfigure '(marginal-caption . #t) '(class . "big-figure")))
      (list 'big-table (md-figure 'tmfigure ))
      (list 'block md-block)
      (list 'cite-detail md-cite-detail)
      (list 'cite md-cite)
      (list 'concat md-concat)
      (list 'doc-author md-doc-author)
      (list 'doc-date md-doc-date)
      (list 'doc-subtitle md-doc-subtitle)
      (list 'doc-title md-doc-title)
      (list 'document md-document)
      (list 'tm-description md-tm-description)
      (list 'tm-description-item md-tm-description-item)
      (list 'dueto md-dueto)
      (list 'em md-style)
      (list 'enumerate md-list)
      (list 'eqnarray* md-equation)
      (list 'eqnarray md-numbered-equation)
      (list 'eqref md-eqref)
      (list 'equation* md-equation)
      (list 'equation*-table md-equation-table)
      (list 'equation md-numbered-equation)
      (list 'explain-macro md-explain-macro)
      (list 'footnote md-footnote)
      (list 'h1 (md-header 1))
      (list 'h2 (md-header 2))
      (list 'h3 (md-header 3))
      (list 'hlink md-hlink)
      (list 'hrule md-hrule)
      (list 'html-class md-html-class)
      (list 'hugo-front md-hugo-frontmatter)  ; Hugo extension
      (list 'hugo-short md-hugo-shortcode)  ; Hugo extension
      (list 'identity skip)
      (list 'image md-image)
      (list 'itemize md-list)
      (list 'item md-item)
      (list 'md-subpara md-subpara)
      (list 'label md-label)
      (list 'labels md-labels)
      (list 'localize md-translate)
      (list 'marginal-figure (md-figure 'sidefigure))
      (list 'marginal-note md-sidenote) ; TfL extension
      (list 'marginal-note* md-sidenote*) ; TfL extension
      (list 'markdown md-markdown)
      (list 'math md-math)
      (list 'paper-author-name paper-author-add)  ; Paperwhy extension
      (list 'para md-para)
      (list 'plain-env (md-make-environment 'identity))
      (list 'plain-env* (md-make-environment* 'identity))
      (list 'quotation md-quotation)
      (list 'reference md-reference)
      (list 'small-figure (md-figure 'tmfigure '(class . "small-figure")))
      (list 'small-table (md-figure 'tmfigure))
      (list 'std-env (md-make-environment 'em))
      (list 'std-env* (md-make-environment* 'em))
      (list 'strike md-style)
      (list 'strong md-style)
      (list 'table-of-contents md-toc) ; Hugo extension
      (list 'tabular md-tabular)
      (list 'tags md-hugo-tags)  ; Hugo extension (DEPRECATED)
      (list 'tmdoc-copyright md-tmdoc-copyright)
      (list 'todo md-todo)
      (list 'tt md-style)
      (list 'underline md-style)
      (list 'wide-figure (md-figure 'tmfigure '(class . "wide-figure")))
      (list 'algo-block md-algo-block)
      (list 'algo-indent md-algo-indent)
      (list 'algo-line md-algo-line)
      (list 'folded md-folded)
      (list 'tm-algorithm md-tm-algorithm)
      (list 'tm-env md-tm-env)
      (list 'tm-session md-session)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (globals-defaults)
  (with frontmatter (make-ahash-table)
    `((file? . #t)
      ; Use a pre-set language (e.g. extracted from TM stree in headless mode)
      ; falling back to get-document-language for interactive use.
      (language . ,(or (md-get 'language) (get-document-language)))
      (num-line-breaks . 2)
      (paragraph-width . ,(get-preference "texmacs->markdown:paragraph-width"))
      (first-indent . "")
      (disable-shortcodes . #f)
      (disable-styles . #f)
      (html-class . "")
      (indent . "")
      (item . "* ")
      (postlude . "\n")
      (footnote-nr . 0)
      (labels . ())
      (doc-authors . ())
      (refs . ())
      (frontmatter . ,frontmatter))))

(define (md-init-globals!)
  ; Initialize md-globals in-place via md-set so the mutation is visible
  ; across module boundaries (with-global's set! only affects local binding).
  (for-each (lambda (kv) (md-set (car kv) (cdr kv))) (globals-defaults)))

(tm-define (serialize-markdown x)
  (md-init-globals!)
  (md-string (serialize-markdown* x)))

(define (strip-leading-newlines s)
  (let loop ((i 0))
    (cond ((>= i (string-length s)) "")
          ((char=? (string-ref s i) #\newline) (loop (+ i 1)))
          (else (substring s i)))))

(tm-define (serialize-markdown-document x)
  (md-init-globals!)
  (with body (serialize-markdown* x)
    ; Strip leading newlines from the body: in Hugo mode, doc-title/author/date
    ; serialize to "" (they go into YAML frontmatter) but still produce "\n\n"
    ; separators in md-document, creating unwanted blank lines after the --- block.
    (md-string (string-append (prelude) (strip-leading-newlines body) (postlude)))))
