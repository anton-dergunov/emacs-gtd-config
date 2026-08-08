;;; test-ps-blank-lines-engine.el --- ERT tests for ps-blank-lines-engine -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path "lisp")
(require 'ps-blank-lines-engine)

;;; Test helpers

(defconst ps/blank-lines-engine-test--fixtures
  ;; (NAME ANCESTOR WORKING EXPECTED)
  ;; Every one is resolved from memory alone, so they run with strategy `zero'
  ;; and never depend on a fitted rule.
  '(("pure strip"
     "* A\n\n** 1\nbody\n\n** 2\n"
     "* A\n** 1\nbody\n** 2\n"
     "* A\n\n** 1\nbody\n\n** 2\n")

    ("strip plus a content edit in the same commit"
     "* A\n\n** 1\nbody\n\n** 2\n"
     "* A\n** 1\nbody edited\n** 2\n** 3 new\n"
     "* A\n\n** 1\nbody edited\n\n** 2\n** 3 new\n")

    ("TODO to DONE keeps both of the block's edges"
     "* A\nprose\n\n** TODO t1\nb1\n\n** t2\n"
     "* A\nprose\n** DONE t1\nb1\n** t2\n"
     "* A\nprose\n\n** DONE t1\nb1\n\n** t2\n")

    ("lead stays with a bodyless parent when children are reordered"
     "* A\n\n** 1\n** 2\n** 3\n"
     "* A\n** 2\n** 1\n** 3\n"
     "* A\n\n** 2\n** 1\n** 3\n")

    ("a closing separator is not carried off by the deeper node"
     "** 1\n*** x\n*** y\n\n** 2\n"
     "** 1\n*** x\n*** y\n** 2\n"
     "** 1\n*** x\n*** y\n\n** 2\n")

    ;; The moved node's own body, including its internal blank line, is carried
    ;; through untouched; only the two seams are resolved.  Memory alone cannot
    ;; supply the level-1 seam before `* B' — see
    ;; `ps/blank-lines-engine--a-moved-subtree-gets-its-level-1-seam-from-the-rule'.
    ("a moved subtree keeps its interior; its seams are recomputed"
     "* A\nb\n\n** 1\nx1\n\nx2\n\n** 2\ny\n\n* B\nb\n"
     "* A\nb\n** 2\ny\n** 1\nx1\n\nx2\n* B\nb\n"
     "* A\nb\n\n** 2\ny\n** 1\nx1\n\nx2\n* B\nb\n")

    ("a promoted subtree still matches"
     "* A\nb\n\n** 1\nx\n\n* B\n"
     "* A\nb\n* 1\nx\n* B\n"
     "* A\nb\n\n* 1\nx\n\n* B\n")

    ("sibling reorder under a parent with prose uses the half-edge"
     "* A\nprose\n\n** 1\nx\n** 2\ny\n"
     "* A\nprose\n** 2\ny\n** 1\nx\n"
     "* A\nprose\n\n** 2\ny\n** 1\nx\n")

    ("blank lines Beorg added are removed again"
     "* A\n** 1\n** 2\n"
     "* A\n\n** 1\n\n** 2\n"
     "* A\n** 1\n** 2\n")

    ("body lines reordered inside a node are left byte-identical"
     "* A\nb1\nb2\n\n* B\n"
     "* A\nb2\nb1\n* B\n"
     "* A\nb2\nb1\n\n* B\n")

    ("a body damaged only in blank lines is swapped wholesale"
     "* A\np1\n\np2\n\n* B\n"
     "* A\np1\np2\n* B\n"
     "* A\np1\n\np2\n\n* B\n")

    ("a block deleted on mobile leaves a resolved seam"
     "* A\nbody\n\n* B\nb\n\n* C\n"
     "* A\nbody\n* C\n"
     "* A\nbody\n\n* C\n")

    ("preamble paragraph breaks are restored"
     "pre1\n\npre2\n\n* A\n"
     "pre1\npre2\n* A\n"
     "pre1\n\npre2\n\n* A\n")

    ("a gap run of length 2 is preserved, not normalised"
     "* A\nbody\n\n\n* B\n"
     "* A\nbody\n* B\n"
     "* A\nbody\n\n\n* B\n")

    ("bof and eof gaps are restored"
     "\n* A\nb\n\n"
     "* A\nb\n"
     "\n* A\nb\n\n")

    ("a file with no trailing newline keeps having none"
     "* A\n\n** 1"
     "* A\n** 1"
     "* A\n\n** 1")

    ("a CRLF file stays CRLF"
     "* A\r\n\r\n** 1\r\n"
     "* A\r\n** 1\r\n"
     "* A\r\n\r\n** 1\r\n")

    ("a blank line inside a src block is never touched"
     "* A\n#+begin_src sh\nc\n\nd\n#+end_src\n\n* B\n"
     "* A\n#+begin_src sh\nc\n\nd\n#+end_src\n* B\n"
     "* A\n#+begin_src sh\nc\n\nd\n#+end_src\n\n* B\n"))
  "Ancestor/working/expected triples driving both the per-case tests and
`ps/blank-lines-engine--every-fixture-preserves-content'.  One source, so a
fixture can never be checked for its output without also being checked for
the safety invariant.")

(defun ps/blank-lines-engine-test--propose (ancestor working &optional rule strategy)
  "Propose WORKING with ANCESTOR's blank lines, defaulting to memory only."
  (ps/blank-lines-propose working ancestor
                          :rule rule :strategy (or strategy 'zero)))

(defun ps/blank-lines-engine-test--text (ancestor working &optional rule strategy)
  "Return the proposed text for ANCESTOR and WORKING."
  (let ((result (ps/blank-lines-engine-test--propose ancestor working rule strategy)))
    (should (plist-get result :ok))
    (plist-get result :text)))

(defun ps/blank-lines-engine-test--fixture (name)
  "Return the fixture named NAME."
  (or (assoc name ps/blank-lines-engine-test--fixtures)
      (error "No such fixture: %s" name)))

(defun ps/blank-lines-engine-test--check (name)
  "Assert that the fixture named NAME produces its expected text."
  (pcase-let ((`(,_ ,ancestor ,working ,expected)
               (ps/blank-lines-engine-test--fixture name)))
    (should (equal (ps/blank-lines-engine-test--text ancestor working) expected))))

(defun ps/blank-lines-engine-test--sources (ancestor working &optional rule strategy)
  "Return an alist of (SLOT . SOURCE) for every change, in order."
  (mapcar (lambda (c) (cons (ps/blank-lines-change-slot c)
                            (ps/blank-lines-change-source c)))
          (plist-get (ps/blank-lines-engine-test--propose
                      ancestor working rule strategy)
                     :changes)))

(defun ps/blank-lines-engine-test--rule (&rest texts)
  "Return a rule fitted over TEXTS."
  (let ((rule (ps/blank-lines-rule-empty)))
    (dolist (text texts)
      (ps/blank-lines-rule-observe rule (ps/blank-lines-parse text)))
    rule))

;;; --------------------------------------------------------------------------
;;; The safety invariant
;;; --------------------------------------------------------------------------

(ert-deftest ps/blank-lines-engine--every-fixture-preserves-content ()
  "No fixture may change a single non-blank line."
  (pcase-dolist (`(,name ,ancestor ,working ,expected)
                 ps/blank-lines-engine-test--fixtures)
    (let ((result (ps/blank-lines-engine-test--propose ancestor working)))
      (should (plist-get result :ok))
      (should (ps/blank-lines-strip-equal-p (plist-get result :text) working))
      ;; The expected text must itself be content-identical to the input, or
      ;; the fixture is asserting something this feature must never do.
      (should (equal name (car (ps/blank-lines-engine-test--fixture name))))
      (should (ps/blank-lines-strip-equal-p expected working)))))

(ert-deftest ps/blank-lines-engine--refuses-an-unparsable-input ()
  "Mixed line endings on either side abort rather than guess."
  (should (eq (plist-get (ps/blank-lines-propose "* A\r\n* B\n" "* A\n") :error)
              'parse-working))
  (should (eq (plist-get (ps/blank-lines-propose "* A\n" "* A\r\n* B\n") :error)
              'parse-ancestor)))

;;; --------------------------------------------------------------------------
;;; Fixtures
;;; --------------------------------------------------------------------------

(ert-deftest ps/blank-lines-engine--fixture-pure-strip ()
  (ps/blank-lines-engine-test--check "pure strip"))

(ert-deftest ps/blank-lines-engine--fixture-strip-with-content-edit ()
  (ps/blank-lines-engine-test--check "strip plus a content edit in the same commit"))

(ert-deftest ps/blank-lines-engine--fixture-state-toggle-keeps-both-edges ()
  (ps/blank-lines-engine-test--check "TODO to DONE keeps both of the block's edges"))

(ert-deftest ps/blank-lines-engine--fixture-lead-stays-with-bodyless-parent ()
  "The design's worked case: `* A' keeps its blank when its children move."
  (ps/blank-lines-engine-test--check
   "lead stays with a bodyless parent when children are reordered"))

(ert-deftest ps/blank-lines-engine--fixture-closing-separator-not-carried ()
  "The design's other worked case: an outdent separator stays put."
  (ps/blank-lines-engine-test--check
   "a closing separator is not carried off by the deeper node"))

(ert-deftest ps/blank-lines-engine--fixture-subtree-moved ()
  (ps/blank-lines-engine-test--check
   "a moved subtree keeps its interior; its seams are recomputed"))

(ert-deftest ps/blank-lines-engine--a-moved-subtree-gets-its-level-1-seam-from-the-rule ()
  "The seam a move creates is exactly where the rule earns its keep.

Memory has nothing for the new edge, and the level-1 cell is 100% in the
corpus, so `learned' restores the blank that `zero' leaves out."
  (let ((ancestor "* A\nb\n\n** 1\nx1\n\nx2\n\n** 2\ny\n\n* B\nb\n")
        (working "* A\nb\n** 2\ny\n** 1\nx1\n\nx2\n* B\nb\n")
        (rule (ps/blank-lines-engine-test--rule
               "* C\nc\n\n* D\nd\n\n* E\ne\n\n* F\nf\n\n* G\ng\n\n* H\nh\n")))
    (should (equal (ps/blank-lines-engine-test--text ancestor working rule 'learned)
                   "* A\nb\n\n** 2\ny\n** 1\nx1\n\nx2\n\n* B\nb\n"))))

(ert-deftest ps/blank-lines-engine--half-edge-requires-the-same-follower-level ()
  "A predecessor's old trailing gap is evidence only for the same level.

Without this the rung deletes real blank lines: on the live corpus every
proposed removal came from adopting the gap that preceded a `***' for a
node that is now a `*'."
  ;; Ancestor: `Top' is followed by a level-2 with no blank.  In the working
  ;; tree the same predecessor is followed by a level-1, which must not
  ;; inherit that 0 — with no rule it falls through to `zero' rather than
  ;; confidently proposing a removal.
  (let ((ancestor "* Top\nbody\n** deep\nd\n")
        (working "* Top\nbody\n\n* Other\no\n"))
    (should (equal (ps/blank-lines-engine-test--text ancestor working)
                   "* Top\nbody\n\n* Other\no\n"))
    (should-not (assq 'sep (ps/blank-lines-engine-test--sources ancestor working)))))

(ert-deftest ps/blank-lines-engine--fixture-subtree-promoted ()
  (ps/blank-lines-engine-test--check "a promoted subtree still matches"))

(ert-deftest ps/blank-lines-engine--fixture-sibling-reorder-half-edge ()
  (ps/blank-lines-engine-test--check
   "sibling reorder under a parent with prose uses the half-edge"))

(ert-deftest ps/blank-lines-engine--fixture-beorg-overinsertion-removed ()
  (ps/blank-lines-engine-test--check "blank lines Beorg added are removed again")
  (let ((result (ps/blank-lines-engine-test--propose
                 "* A\n** 1\n** 2\n" "* A\n\n** 1\n\n** 2\n")))
    (should (equal (plist-get result :removed) 2))
    (should (equal (plist-get result :restored) 0))))

(ert-deftest ps/blank-lines-engine--fixture-body-reorder-byte-identical ()
  (ps/blank-lines-engine-test--check
   "body lines reordered inside a node are left byte-identical"))

(ert-deftest ps/blank-lines-engine--fixture-body-swap-restores-blanks ()
  (ps/blank-lines-engine-test--check
   "a body damaged only in blank lines is swapped wholesale"))

(ert-deftest ps/blank-lines-engine--fixture-block-deleted-seam ()
  (ps/blank-lines-engine-test--check
   "a block deleted on mobile leaves a resolved seam"))

(ert-deftest ps/blank-lines-engine--fixture-preamble-restored ()
  (ps/blank-lines-engine-test--check "preamble paragraph breaks are restored"))

(ert-deftest ps/blank-lines-engine--fixture-run-length-2-preserved ()
  (ps/blank-lines-engine-test--check
   "a gap run of length 2 is preserved, not normalised"))

(ert-deftest ps/blank-lines-engine--fixture-bof-eof-gaps ()
  (ps/blank-lines-engine-test--check "bof and eof gaps are restored"))

(ert-deftest ps/blank-lines-engine--fixture-no-trailing-newline ()
  (ps/blank-lines-engine-test--check
   "a file with no trailing newline keeps having none"))

(ert-deftest ps/blank-lines-engine--fixture-crlf ()
  (ps/blank-lines-engine-test--check "a CRLF file stays CRLF"))

(ert-deftest ps/blank-lines-engine--fixture-src-block-blank-untouched ()
  "The hazard a flat model would have: an in-block blank must survive."
  (ps/blank-lines-engine-test--check
   "a blank line inside a src block is never touched"))

;;; --------------------------------------------------------------------------
;;; The whole-body swap
;;; --------------------------------------------------------------------------

(ert-deftest ps/blank-lines-engine--body-swap-refuses-on-any-content-difference ()
  "The swap fires only when the non-blank lines are identical."
  ;; Same content, different blanks: swapped.
  (should (equal (ps/blank-lines-engine-test--text
                  "* A\np1\n\np2\n" "* A\np1\np2\n")
                 "* A\np1\n\np2\n"))
  ;; Different content: the working tree's body is kept, untouched.
  (should (equal (ps/blank-lines-engine-test--text
                  "* A\np1\n\np2\n" "* A\np1\nCHANGED\n")
                 "* A\np1\nCHANGED\n"))
  ;; Reordered content is a content difference, not a blank-line difference.
  (should (equal (ps/blank-lines-engine-test--text
                  "* A\np1\n\np2\n" "* A\np2\np1\n")
                 "* A\np2\np1\n")))

;;; --------------------------------------------------------------------------
;;; The resolution ladder
;;; --------------------------------------------------------------------------

(ert-deftest ps/blank-lines-engine--ladder-uses-the-exact-edge-first ()
  "Two nodes still adjacent take their gap verbatim."
  (should (equal (assq 'sep (ps/blank-lines-engine-test--sources
                             "* A\nb\n\n* B\n" "* A\nb\n* B\n"))
                 '(sep . exact-edge))))

(ert-deftest ps/blank-lines-engine--ladder-falls-back-to-the-half-edge ()
  "When the follower changed, the predecessor's own trailing gap is adopted."
  (should (equal (assq 'sep (ps/blank-lines-engine-test--sources
                             "* A\nb\n\n* B\nx\n" "* A\nb\n* C\ny\n"))
                 '(sep . half-edge))))

(ert-deftest ps/blank-lines-engine--no-evidence-leaves-the-gap-alone ()
  "The last rung keeps the current value; it must never delete a blank line.

A gap the ancestor knows nothing about is not thereby known to be unwanted."
  (let ((ancestor "* A\n") (working "* Z\nx\n\n* Y\n"))
    (should (equal (ps/blank-lines-engine-test--text ancestor working) working))
    ;; Nothing to report, because nothing changed.
    (should-not (assq 'sep (ps/blank-lines-engine-test--sources ancestor working)))))

(ert-deftest ps/blank-lines-engine--lead-is-never-rule-driven ()
  "A matched node's lead is copied verbatim; an unmatched node's is left alone."
  (should (equal (assq 'lead (ps/blank-lines-engine-test--sources
                              "* A\n\nbody\n" "* A\nbody\n"))
                 '(lead . verbatim)))
  ;; A node with no counterpart keeps whatever lead it arrived with.
  (should (equal (ps/blank-lines-engine-test--text
                  "* A\n" "* Zebra\n\nnew body\n")
                 "* Zebra\n\nnew body\n")))

;;; --------------------------------------------------------------------------
;;; The fitted rule
;;; --------------------------------------------------------------------------

(ert-deftest ps/blank-lines-engine--rule-cell-classification ()
  "Each of the measured cells is reachable from its own shape."
  (should (eq (ps/blank-lines-rule-cell 1 2 t) 'l1))
  (should (eq (ps/blank-lines-rule-cell 3 2 t) 'l3+))
  (should (eq (ps/blank-lines-rule-cell 2 nil nil) 'l2-after-preamble))
  (should (eq (ps/blank-lines-rule-cell 2 1 t) 'l2-parent-prose))
  (should (eq (ps/blank-lines-rule-cell 2 1 nil) 'l2-parent-bare))
  (should (eq (ps/blank-lines-rule-cell 2 3 t) 'l2-from-deeper-prose))
  (should (eq (ps/blank-lines-rule-cell 2 3 nil) 'l2-from-deeper-bare))
  (should (eq (ps/blank-lines-rule-cell 2 2 t) 'l2-sibling-prose))
  (should (eq (ps/blank-lines-rule-cell 2 2 nil) 'l2-sibling-bare)))

(ert-deftest ps/blank-lines-engine--rule-is-fitted-from-observations ()
  "Fitting counts boundary gaps and predicts each cell's most common value."
  (let ((rule (ps/blank-lines-engine-test--rule
               "* A\nb\n\n* B\nb\n\n* C\nb\n\n* D\nb\n\n* E\nb\n\n* F\nb\n")))
    (should (equal (ps/blank-lines-rule-report rule) '((l1 1 5 5))))
    (should (equal (car (ps/blank-lines-rule-predict rule 'l1)) 1))))

(ert-deftest ps/blank-lines-engine--rule-needs-enough-samples ()
  "A thinly observed cell is not trusted, so the ladder falls through to 0."
  (let ((rule (ps/blank-lines-engine-test--rule "* A\nb\n\n* B\nb\n")))
    (should-not (ps/blank-lines-rule-predict rule 'l1))
    (should-not (ps/blank-lines-rule-predict rule 'l2-sibling-prose))))

(ert-deftest ps/blank-lines-engine--rule-fills-a-new-seam ()
  "A level-1 heading typed on mobile gets the blank the corpus always has."
  (let ((rule (ps/blank-lines-engine-test--rule
               "* A\nb\n\n* B\nb\n\n* C\nb\n\n* D\nb\n\n* E\nb\n\n* F\nb\n")))
    (should (equal (ps/blank-lines-engine-test--text
                    "* A\nb\n\n* B\nb\n" "* A\nb\n\n* B\nb\n* NEW\nn\n"
                    rule 'learned)
                   "* A\nb\n\n* B\nb\n\n* NEW\nn\n"))
    ;; With `zero' the same seam is left flat — the cost of never guessing.
    (should (equal (ps/blank-lines-engine-test--text
                    "* A\nb\n\n* B\nb\n" "* A\nb\n\n* B\nb\n* NEW\nn\n"
                    rule 'zero)
                   "* A\nb\n\n* B\nb\n* NEW\nn\n"))))

;;; --------------------------------------------------------------------------
;;; Matching
;;; --------------------------------------------------------------------------

(ert-deftest ps/blank-lines-engine--bodyless-siblings-are-not-scored-identical ()
  "Two empty bodies are no evidence, so they must not score 1.0.
Otherwise a file of bare `** TODO' siblings would match arbitrarily."
  (let ((a (car (ps/blank-lines-node-walk (ps/blank-lines-parse "* one\n"))))
        (b (car (ps/blank-lines-node-walk (ps/blank-lines-parse "* two\n")))))
    (should-not (ps/blank-lines--body-similarity
                 (ps/blank-lines-node-body a) (ps/blank-lines-node-body b)))
    (should (< (ps/blank-lines-similarity a b) 1.0))))

(ert-deftest ps/blank-lines-engine--similarity-survives-a-state-toggle ()
  "A TODO toggle changes the heading line but not the node's identity."
  (let ((a (car (ps/blank-lines-node-walk
                 (ps/blank-lines-parse "* TODO Write it :work:\nbody\n"))))
        (b (car (ps/blank-lines-node-walk
                 (ps/blank-lines-parse "* DONE Write it :work:\nbody\n")))))
    (should (equal (ps/blank-lines-similarity a b) 1.0))))

(ert-deftest ps/blank-lines-engine--unrelated-nodes-stay-below-the-floor ()
  "Nothing in common must not match."
  (let ((a (car (ps/blank-lines-node-walk
                 (ps/blank-lines-parse "* Buy milk\nfrom the shop\n"))))
        (b (car (ps/blank-lines-node-walk
                 (ps/blank-lines-parse "* Rewrite the parser\nin elisp\n")))))
    (should (< (ps/blank-lines-similarity a b) ps/blank-lines-match-floor))))

;;; --------------------------------------------------------------------------
;;; Ancestor scoring
;;; --------------------------------------------------------------------------

(ert-deftest ps/blank-lines-engine--score-counts-recoverable-blank-lines ()
  "Scoring ignores the rule, so candidates compete on what they remember."
  (should (equal (ps/blank-lines-score-recoverable
                  "* A\n** 1\nbody\n** 2\n" "* A\n\n** 1\nbody\n\n** 2\n")
                 2))
  (should (equal (ps/blank-lines-score-recoverable
                  "* A\n** 1\nbody\n** 2\n" "* A\n** 1\nbody\n** 2\n")
                 0)))

(provide 'test-ps-blank-lines-engine)
;;; test-ps-blank-lines-engine.el ends here
