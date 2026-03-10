;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : markdownout-test.scm
;; DESCRIPTION : Tests for markdownout, focusing on add-style-to
;; COPYRIGHT   : (C) 2024 Anvar Atayev
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert markdown markdownout-test)
  (:use (convert markdown markdownout)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests for add-style-to
;;
;; add-style-to recurses into block content to push an inline style marker
;; down to text leaves, rather than wrapping a block node in a style tag.
;; The idempotent branch (clause 2) prevents double-wrapping when a child
;; is already wrapped in the same style.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-add-style-to)
  (regression-test-group
   "markdownout add-style-to" "add-style-to"
   :none :none

   ;; Base case: string gets wrapped in style tag
   (test "string gets style tag"
     (add-style-to 'em "foo")
     '(em "foo"))

   ;; Idempotent branch: same style is dropped, not doubled
   (test "em of em string is idempotent"
     (add-style-to 'em '(em "foo"))
     '(em "foo"))

   (test "strong of strong string is idempotent"
     (add-style-to 'strong '(strong "bar"))
     '(strong "bar"))

   (test "tt of tt string is idempotent"
     (add-style-to 'tt '(tt "code"))
     '(tt "code"))

   ;; Triple nesting: clause 2 fires twice, collapses to single
   (test "triple em collapses to single em"
     (add-style-to 'em '(em (em "foo")))
     '(em "foo"))

   ;; Distribution into document: idempotent branch fires on already-styled child
   ;; This is the primary real-world trigger of the idempotent branch.
   (test "em distributes into document, em child stays single"
     (add-style-to 'em '(document (em "a") "b"))
     '(document (em "a") (em "b")))

   (test "strong distributes into document, strong child stays single"
     (add-style-to 'strong '(document "a" (strong "b")))
     '(document (strong "a") (strong "b")))

   ;; quotation is a stylable block: distribute into its document child
   (test "em distributes into quotation"
     (add-style-to 'em '(quotation (document "a" "b")))
     '(quotation (document (em "a") (em "b"))))

   ;; std-env is NOT stylable: wrap the whole node (name/number would break otherwise)
   (test "em wraps std-env whole"
     (add-style-to 'em '(std-env "Theorem" "1" (document "foo")))
     '(em (std-env "Theorem" "1" (document "foo"))))

   ;; Drop list: math and equation content is returned unchanged
   (test "math content drops style"
     (add-style-to 'em '(math "x"))
     '(math "x"))

   (test "equation content drops style"
     (add-style-to 'em '(equation "x = y"))
     '(equation "x = y"))

   ;; Different styles distribute into each other (no cancellation)
   (test "em distributes inside strong"
     (add-style-to 'em '(strong "foo"))
     '(strong (em "foo")))

   (test "strong distributes inside em"
     (add-style-to 'strong '(em "foo"))
     '(em (strong "foo")))
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test suite entry point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (regtest-markdownout)
  (let ((n (+ (regtest-add-style-to))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of markdownout: ok\n")))
