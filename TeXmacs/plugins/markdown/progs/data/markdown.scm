;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : markdown.scm
;; DESCRIPTION : Markdown format setup
;; COPYRIGHT   : (C) 2017 Ana Cañizares García and Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (data markdown))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ignore var val) (noop))
(define-preferences
  ("texmacs->markdown:flavour" "vanilla" ignore)
  ("texmacs->markdown:paragraph-width" 79 ignore)
  ("texmacs->markdown:show-menu" "off" ignore)
  ("texmacs->markdown:numbered-sections" "on" ignore)
  ("texmacs->markdown:auto-export" "off" ignore)
  ("texmacs->markdown:table-format" "html" ignore))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown format definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format markdown
  (:name "Markdown")
  (:suffix "md"))

(lazy-define (convert markdown markdownout) serialize-markdown)
(lazy-define (convert markdown markdownout) serialize-markdown-document)
(lazy-define (convert markdown tmmarkdown) texmacs->markdown)

(converter markdown-stree markdown-document
  (:function serialize-markdown-document))

(converter markdown-stree markdown-snippet
  (:function serialize-markdown))

(converter texmacs-stree markdown-stree
  (:function texmacs->markdown))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lazy-menu (convert markdown markdown-menus) markdown-menu tools-menu)

(delayed (:idle 1000)
  (lazy-define-force tools-menu)
  (menu-bind tools-menu
    (former)
    ("Markdown plugin" (toggle-preference "texmacs->markdown:show-menu"))))
