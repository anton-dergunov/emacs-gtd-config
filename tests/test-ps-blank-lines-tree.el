;;; test-ps-blank-lines-tree.el --- ERT tests for ps-blank-lines-tree -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'org-element)
(add-to-list 'load-path "lisp")
(require 'ps-blank-lines-tree)

;;; Test helpers

(defconst ps/blank-lines-tree-test--round-trip-texts
  '(""
    "\n"
    "\n\n\n"
    "a"
    "a\n"
    "a\n\n"
    "\n\n* A\n"
    "* A\n"
    "* A\n\n"
    "pre1\n\npre2\n\n* A\n\nbody A1\n\nbody A2\n\n\n** B\ntext\n\n*** C\n\n"
    "* A\n\n** 1\n** 2\n** 3\n"
    "** 1\n*** x\n*** y\n\n** 2\n"
    "* A\n#+begin_src sh\ncode\n\nmore\n#+end_src\n\n* B\n"
    "* TODO [#A] Task :tag:\n body\n"
    "* A\r\n\r\n** 1\r\n")
  "Texts that must survive `render' after `parse' byte for byte.
Whitespace-only lines are deliberately absent; see
`ps/blank-lines-tree--whitespace-only-blank-lines-are-normalized'.")

(defun ps/blank-lines-tree-test--nodes (text)
  "Parse TEXT and return its nodes in document order."
  (ps/blank-lines-node-walk (ps/blank-lines-parse text)))

(defun ps/blank-lines-tree-test--gaps (text)
  "Return (RAW-VALUE SEP LEAD) for every node of TEXT, in document order."
  (mapcar (lambda (n)
            (list (ps/blank-lines-node-raw-value n)
                  (ps/blank-lines-node-sep n)
                  (ps/blank-lines-node-lead n)))
          (ps/blank-lines-tree-test--nodes text)))

(defun ps/blank-lines-tree-test--pre-blanks (text)
  "Return org-element's `:pre-blank' for every headline of TEXT."
  (with-temp-buffer
    (insert text)
    (let ((org-inhibit-startup t) (org-element-use-cache nil))
      (delay-mode-hooks (org-mode)))
    (let ((org-element-use-cache nil))
      (org-element-map (org-element-parse-buffer 'headline) 'headline
        (lambda (h) (org-element-property :pre-blank h))))))

;;; --------------------------------------------------------------------------
;;; Round trip
;;; --------------------------------------------------------------------------

(ert-deftest ps/blank-lines-tree--render-inverts-parse ()
  "`render' must reproduce the source text exactly, for every shape."
  (dolist (text ps/blank-lines-tree-test--round-trip-texts)
    (let ((file (ps/blank-lines-parse text)))
      (should file)
      (should (equal (ps/blank-lines-render file) text)))))

(ert-deftest ps/blank-lines-tree--parse-rejects-mixed-line-endings ()
  "Mixed CRLF and bare LF cannot round-trip, so parsing refuses."
  (should-not (ps/blank-lines-parse "* A\r\n* B\n"))
  (should (ps/blank-lines-parse "* A\r\n* B\r\n"))
  (should (ps/blank-lines-parse "* A\n* B\n")))

(ert-deftest ps/blank-lines-tree--crlf-is-preserved ()
  "A CRLF file renders back with CRLF terminators."
  (let ((file (ps/blank-lines-parse "* A\r\n\r\n** 1\r\n")))
    (should (equal (ps/blank-lines-file-eol file) "\r\n"))
    (should (equal (ps/blank-lines-render file) "* A\r\n\r\n** 1\r\n"))))

;;; --------------------------------------------------------------------------
;;; Blank lines and the safety invariant
;;; --------------------------------------------------------------------------

(ert-deftest ps/blank-lines-tree--whitespace-only-lines-are-blank ()
  "A whitespace-only line counts as blank, matching org-element."
  (should (ps/blank-lines-blank-p ""))
  (should (ps/blank-lines-blank-p "   "))
  (should (ps/blank-lines-blank-p "\t"))
  (should-not (ps/blank-lines-blank-p " x ")))

(ert-deftest ps/blank-lines-tree--whitespace-only-blank-lines-are-normalized ()
  "A whitespace-only line renders back as a truly empty line.

Gaps are counts, so the original spaces are not carried.  This is the one
place rendering is not byte-exact; it is rare (2 lines in 8683 across the
real corpus), it cannot change content, and
`ps/blank-lines-count-whitespace-only' lets callers report it rather than
letting it appear unexplained in a review diff."
  (let ((file (ps/blank-lines-parse "* A\n  \nbody\n")))
    (should (equal (ps/blank-lines-render file) "* A\n\nbody\n"))
    (should (ps/blank-lines-strip-equal-p (ps/blank-lines-render file) "* A\n  \nbody\n")))
  (should (equal (ps/blank-lines-count-whitespace-only "* A\n  \nbody\n\t\n") 2))
  (should (equal (ps/blank-lines-count-whitespace-only "* A\n\nbody\n") 0)))

(ert-deftest ps/blank-lines-tree--strip-drops-only-blank-lines ()
  "`strip' returns the non-blank lines, in order, and nothing else."
  (should (equal (ps/blank-lines-strip "a\n\n  \nb\n") '("a" "b")))
  (should (equal (ps/blank-lines-strip "") '()))
  (should (equal (ps/blank-lines-strip "a\r\nb\r\n") '("a" "b"))))

(ert-deftest ps/blank-lines-tree--strip-equal-p-ignores-blank-runs ()
  "Texts differing only in blank lines compare equal; content changes do not."
  (should (ps/blank-lines-strip-equal-p "* A\n\nbody\n" "* A\nbody\n\n\n"))
  (should-not (ps/blank-lines-strip-equal-p "* A\nbody\n" "* A\nBODY\n"))
  (should-not (ps/blank-lines-strip-equal-p "* A\nb\nc\n" "* A\nc\nb\n")))

;;; --------------------------------------------------------------------------
;;; Gap ownership — the design's two worked cases
;;; --------------------------------------------------------------------------

(ert-deftest ps/blank-lines-tree--lead-belongs-to-a-bodyless-parent ()
  "The blank after `* A' is A's lead, so its children can be reordered freely.
The first child therefore has no sep of its own."
  (should (equal (ps/blank-lines-tree-test--gaps "* A\n\n** 1\n** 2\n** 3\n")
                 '(("A" nil 1) ("1" nil 0) ("2" 0 0) ("3" 0 0)))))

(ert-deftest ps/blank-lines-tree--closing-separator-is-not-carried-by-the-deeper-node ()
  "A blank before an outdent is the next node's sep, not the previous lead.
Otherwise it would wander off whenever `*** y' moved."
  (should (equal (ps/blank-lines-tree-test--gaps "** 1\n*** x\n*** y\n\n** 2\n")
                 '(("1" nil 0) ("x" nil 0) ("y" 0 0) ("2" 1 0)))))

(ert-deftest ps/blank-lines-tree--sep-is-nil-exactly-for-first-content-in-parent ()
  "sep is nil when the preceding line is the parent's own heading line."
  ;; Parent with prose: the child is no longer the parent's first content.
  (should (equal (ps/blank-lines-tree-test--gaps "* A\nprose\n\n** 1\n** 2\n")
                 '(("A" nil 0) ("1" 1 0) ("2" 0 0))))
  ;; Parent without prose: the child is first content, so the gap is A's lead.
  (should (equal (ps/blank-lines-tree-test--gaps "* A\n\n** 1\n")
                 '(("A" nil 1) ("1" nil 0)))))

(ert-deftest ps/blank-lines-tree--lead-matches-org-element-pre-blank ()
  "Our lead must agree with org-element's `:pre-blank' on every headline."
  (dolist (text ps/blank-lines-tree-test--round-trip-texts)
    (unless (string-match-p "\r" text)
      (let ((file (ps/blank-lines-parse text)))
        (should (equal (mapcar #'ps/blank-lines-node-lead
                               (ps/blank-lines-node-walk file))
                       (ps/blank-lines-tree-test--pre-blanks text)))))))

;;; --------------------------------------------------------------------------
;;; Bodies are opaque
;;; --------------------------------------------------------------------------

(ert-deftest ps/blank-lines-tree--blank-inside-a-src-block-lives-in-the-body ()
  "A blank line inside `#+begin_src' is body content, never a gap.
This is what makes the flat-model hazard unreachable: nothing outside the
body can propose changing it."
  (let* ((text "* A\n#+begin_src sh\ncode\n\nmore\n#+end_src\n\n* B\n")
         (nodes (ps/blank-lines-tree-test--nodes text)))
    (should (equal (ps/blank-lines-node-body (car nodes))
                   '("#+begin_src sh" "code" "" "more" "#+end_src")))
    (should (equal (ps/blank-lines-node-lead (car nodes)) 0))
    (should (equal (ps/blank-lines-node-sep (cadr nodes)) 1))))

(ert-deftest ps/blank-lines-tree--body-excludes-trailing-blanks ()
  "Trailing blanks belong to the next node's sep, not to the body.
org-element's section `:contents-end' includes them, so they must be sliced."
  (let ((nodes (ps/blank-lines-tree-test--nodes "* A\nbody\n\n\n* B\n")))
    (should (equal (ps/blank-lines-node-body (car nodes)) '("body")))
    (should (equal (ps/blank-lines-node-sep (cadr nodes)) 2))))

(ert-deftest ps/blank-lines-tree--body-keeps-internal-blanks ()
  "Blank lines between two body lines stay inside the body, verbatim."
  (let ((nodes (ps/blank-lines-tree-test--nodes "* A\np1\n\np2\n\n* B\n")))
    (should (equal (ps/blank-lines-node-body (car nodes)) '("p1" "" "p2")))
    (should (equal (ps/blank-lines-node-sep (cadr nodes)) 1))))

;;; --------------------------------------------------------------------------
;;; File-level slots
;;; --------------------------------------------------------------------------

(ert-deftest ps/blank-lines-tree--preamble-is-the-headless-root-body ()
  "Preamble prose and its paragraph breaks are carried verbatim."
  (let ((file (ps/blank-lines-parse "pre1\n\npre2\n\n* A\n")))
    (should (equal (ps/blank-lines-file-preamble file) '("pre1" "" "pre2")))
    (should (equal (ps/blank-lines-file-bof file) 0))
    (should (equal (ps/blank-lines-node-sep (car (ps/blank-lines-file-nodes file))) 1))))

(ert-deftest ps/blank-lines-tree--bof-absorbs-a-blank-only-preamble ()
  "With no preamble prose, leading blanks are the file's bof, not a sep."
  (let ((file (ps/blank-lines-parse "\n\n* A\n")))
    (should (equal (ps/blank-lines-file-bof file) 2))
    (should-not (ps/blank-lines-file-preamble file))
    (should-not (ps/blank-lines-node-sep (car (ps/blank-lines-file-nodes file))))))

(ert-deftest ps/blank-lines-tree--eof-holds-trailing-blanks ()
  "Trailing blanks are the file's eof, whether or not there are headings."
  (should (equal (ps/blank-lines-file-eof (ps/blank-lines-parse "* A\nbody\n\n\n")) 2))
  (should (equal (ps/blank-lines-file-eof (ps/blank-lines-parse "* A\n\n\n")) 2))
  (should (equal (ps/blank-lines-file-eof (ps/blank-lines-parse "prose\n\n\n")) 2)))

;;; --------------------------------------------------------------------------
;;; Tree shape
;;; --------------------------------------------------------------------------

(ert-deftest ps/blank-lines-tree--nesting-sets-path-and-sibling-index ()
  "Nodes nest by heading level, recording their outline path and position."
  (let* ((file (ps/blank-lines-parse "* A\n** 1\n*** x\n** 2\n* B\n"))
         (nodes (ps/blank-lines-node-walk file)))
    (should (equal (mapcar #'ps/blank-lines-node-raw-value nodes)
                   '("A" "1" "x" "2" "B")))
    (should (equal (mapcar #'ps/blank-lines-node-path nodes)
                   '(nil ("A") ("A" "1") ("A") nil)))
    (should (equal (mapcar #'ps/blank-lines-node-sibling-index nodes)
                   '(0 0 0 1 1)))
    (should (equal (length (ps/blank-lines-file-nodes file)) 2))))

(ert-deftest ps/blank-lines-tree--heading-fields-are-normalized ()
  "Titles drop the keyword, priority and tags; the heading line stays verbatim."
  (let ((node (car (ps/blank-lines-tree-test--nodes "* TODO [#A] Write it :work:home:\n"))))
    (should (equal (ps/blank-lines-node-raw-value node) "Write it"))
    (should (equal (ps/blank-lines-node-todo node) "TODO"))
    (should (equal (ps/blank-lines-node-tags node) '("work" "home")))
    (should (equal (ps/blank-lines-node-heading-line node)
                   "* TODO [#A] Write it :work:home:"))))

(provide 'test-ps-blank-lines-tree)
;;; test-ps-blank-lines-tree.el ends here
