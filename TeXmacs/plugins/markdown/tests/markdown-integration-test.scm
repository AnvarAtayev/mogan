;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : markdown-integration-test.scm
;; DESCRIPTION : Integration tests for the markdown converter
;;               Converts .tm files and compares against expected .md files
;; COPYRIGHT   : (C) 2024 Anvar Atayev
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (markdown tests markdown-integration-test)
  (:use (convert markdown markdown-utils)
        (convert markdown tmmarkdown)
        (convert markdown markdownout)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tests-root
  (url->system
   (url-resolve "$TEXMACS_PATH/plugins/markdown/tests" "r")))

(define (test-path subdir name)
  (string-append tests-root "/" subdir "/" name))

(define (tm-stree-language stree)
  "Extract language from a TM stree's initial section, defaulting to 'english'.
   In headless mode get-document-language reads the system locale rather than
   the document, so we extract the language directly from the stree."
  (let ((assocs (select stree '(initial collection associate))))
    (let loop ((l assocs))
      (cond ((null? l) "english")
            ((and (list? (car l))
                  (== (car (car l)) 'associate)
                  (== (cadr (car l)) "language"))
             (caddr (car l)))
            (else (loop (cdr l)))))))

(define (convert-tm-to-md tm-path)
  "Loads a .tm file and returns the markdown string via the direct pipeline"
  (let* ((tm-string (string-load (system->url tm-path)))
         (tm-tree   (parse-texmacs tm-string))
         (tm-stree  (tree->stree tm-tree))
         (lang      (tm-stree-language tm-stree))
         (md-stree  (texmacs->markdown tm-stree)))
    ; Pre-set language from document stree before md-init-globals! runs,
    ; so globals-defaults picks it up instead of the system locale.
    (md-set 'language lang)
    (serialize-markdown-document md-stree)))

(define (read-expected md-path)
  "Reads expected .md, stripping the final newline that serialize adds"
  (with s (string-load (system->url md-path))
    (if (and (string-nnull? s) (string-ends? s "\n"))
        (string-drop-right s 1)
        s)))

(define (normalize actual)
  "Strip final newline that serialize-markdown-document appends"
  (if (and (string-nnull? actual) (string-ends? actual "\n"))
      (string-drop-right actual 1)
      actual))

(define (run-one-test subdir name)
  "Returns #t if conversion matches expected, or the actual string on mismatch"
  (let* ((actual   (normalize (convert-tm-to-md (test-path subdir (string-append name ".tm")))))
         (expected (read-expected (test-path subdir (string-append name ".md")))))
    (or (== actual expected) actual)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vanilla tests (paragraph-width 79, flavour vanilla)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-markdown-vanilla)
  (set-preference "texmacs->markdown:flavour" "vanilla")
  (set-preference "texmacs->markdown:paragraph-width" 79)
  (regression-test-group
   "markdown vanilla" "vanilla"
   :none :none
   (test "code.tm"             (run-one-test "vanilla" "code")             #t)
   (test "eqnarray.tm"         (run-one-test "vanilla" "eqnarray")         #t)
   (test "itemize.tm"          (run-one-test "vanilla" "itemize")          #t)
   (test "itemize-styled.tm"   (run-one-test "vanilla" "itemize-styled")   #t)
   (test "quotations.tm"       (run-one-test "vanilla" "quotations")       #t)
   (test "sections.tm"         (run-one-test "vanilla" "sections")         #t)
   (test "simple-math.tm"      (run-one-test "vanilla" "simple-math")      #t)
   (test "std-environments.tm" (run-one-test "vanilla" "std-environments") #t)
   (test "styles.tm"           (run-one-test "vanilla" "styles")           #t)
   (test "titles.tm"           (run-one-test "vanilla" "titles")           #t)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test suite entry point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (regtest-markdown-integration)
  ; Ensure texmacs->latex is available for math conversion tests
  (module-load '(convert latex tmtex))
  (let ((n (+ (regtest-markdown-vanilla))))
    (display* "Total: " (object->string n) " integration tests.\n")
    (display "Integration test suite of markdown: ok\n")))
