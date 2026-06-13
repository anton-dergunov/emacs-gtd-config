;;; test-ps-org-src-comment.el --- ERT tests for ps-org-src-comment -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path "lisp")
(require 'ps-org-src-comment)

;;; Helpers

(defmacro ps/org-src-comment-test--with-buffer (text &rest body)
  "Insert TEXT into a temporary `org-mode' buffer and run BODY in it."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,text)
     ,@body))

(defun ps/org-src-comment-test--toggle-lines (start-line end-line)
  "Select lines START-LINE..END-LINE (1-indexed) and toggle their comments."
  (goto-char (point-min))
  (forward-line (1- start-line))
  (let ((beg (point)))
    (goto-char (point-min))
    (forward-line (1- end-line))
    (end-of-line)
    (set-mark beg)
    (activate-mark)
    (ps/org-src-comment-dwim)))

(defun ps/org-src-comment-test--buffer-string ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun ps/org-src-comment-test--toggle-region (beg end)
  "Activate a region from BEG to END (buffer positions) and toggle comments."
  (set-mark beg)
  (goto-char end)
  (activate-mark)
  (ps/org-src-comment-dwim))

;;; -------------------------------------------------------
;;; comment-string
;;; -------------------------------------------------------

(ert-deftest ps/org-src-comment--string-for-emacs-lisp ()
  "The comment prefix for an emacs-lisp src block is \";; \"."
  (ps/org-src-comment-test--with-buffer
      "#+begin_src emacs-lisp\n  (setq x 1)\n#+end_src\n"
    (goto-char (point-min))
    (search-forward "(setq x 1)")
    (should (equal (ps/org-src-comment-string) ";; "))))

;;; -------------------------------------------------------
;;; single line: comment, then uncomment
;;; -------------------------------------------------------

(ert-deftest ps/org-src-comment--single-line-roundtrip ()
  "Toggling a single uncommented line comments it, and toggling again
restores the original line exactly."
  (ps/org-src-comment-test--with-buffer
      "#+begin_src emacs-lisp\n  (setq x 1)\n  (setq y 2)\n#+end_src\n"
    (let ((original (ps/org-src-comment-test--buffer-string)))
      (goto-char (point-min))
      (search-forward "(setq x 1)")
      (beginning-of-line)
      (ps/org-src-comment-dwim)
      (should (equal (ps/org-src-comment-test--buffer-string)
                     "#+begin_src emacs-lisp\n  ;; (setq x 1)\n  (setq y 2)\n#+end_src\n"))
      (beginning-of-line)
      (ps/org-src-comment-dwim)
      (should (equal (ps/org-src-comment-test--buffer-string) original)))))

;;; -------------------------------------------------------
;;; multi-line, already-commented block: uncomment removes one level
;;; -------------------------------------------------------

(ert-deftest ps/org-src-comment--uncomment-fully-commented-block ()
  "A fully `;; '-commented, aligned block is uncommented in place, restoring
the code's relative indentation."
  (ps/org-src-comment-test--with-buffer
      (concat "#+begin_src emacs-lisp\n"
              "  (setq x\n"
              "        '((\"All\" . t)\n"
              "          ;; (\"Work\" . (:include (\"a\"\n"
              "          ;;                       \"b\")\n"
              "          ;;            :exclude nil))\n"
              "          ))\n"
              "#+end_src\n")
    (goto-char (point-min))
    (search-forward ";; (\"Work\"")
    (let ((start-line (line-number-at-pos)))
      (search-forward ":exclude nil))")
      (let ((end-line (line-number-at-pos)))
        (ps/org-src-comment-test--toggle-lines start-line end-line)
        (should (equal (ps/org-src-comment-test--buffer-string)
                       (concat "#+begin_src emacs-lisp\n"
                               "  (setq x\n"
                               "        '((\"All\" . t)\n"
                               "          (\"Work\" . (:include (\"a\"\n"
                               "                                \"b\")\n"
                               "                     :exclude nil))\n"
                               "          ))\n"
                               "#+end_src\n")))))))

;;; -------------------------------------------------------
;;; round trip: comment -> uncomment -> comment is byte-identical and
;;; remains aligned (regression for the misaligned re-comment bug)
;;; -------------------------------------------------------

(ert-deftest ps/org-src-comment--multiline-roundtrip-stays-aligned ()
  "Toggling an aligned commented block twice returns to the exact original
text, with the comment markers still aligned in a single column (not
scattered at each line's own indentation)."
  (let ((original (concat "#+begin_src emacs-lisp\n"
                           "  (setq ps/file-tree-file-sets\n"
                           "        '((\"All\" . (:include nil :exclude nil))\n"
                           "          ;; ;; Examples:\n"
                           "          ;; (\"Work\" . (:include (\"/Areas/Work\\\\.org\\\\'\"\n"
                           "          ;;                       \"/Current/\")\n"
                           "          ;;            :exclude nil))\n"
                           "          ;; (\"No Sensitive\" . (:include nil\n"
                           "          ;;                     :exclude (\"/Areas/Financial\\\\.org\\\\'\"\n"
                           "          ;;                               \"/Areas/Interviews\\\\.org\\\\'\")))\n"
                           "          ))\n"
                           "#+end_src\n")))
    (ps/org-src-comment-test--with-buffer original
      (goto-char (point-min))
      (search-forward ";; ;; Examples")
      (let ((start-line (line-number-at-pos)))
        (search-forward "Interviews")
        (let ((end-line (line-number-at-pos)))
          ;; toggle 1: uncomment
          (ps/org-src-comment-test--toggle-lines start-line end-line)
          (let ((uncommented (ps/org-src-comment-test--buffer-string)))
            (should (equal uncommented
                           (concat "#+begin_src emacs-lisp\n"
                                   "  (setq ps/file-tree-file-sets\n"
                                   "        '((\"All\" . (:include nil :exclude nil))\n"
                                   "          ;; Examples:\n"
                                   "          (\"Work\" . (:include (\"/Areas/Work\\\\.org\\\\'\"\n"
                                   "                                \"/Current/\")\n"
                                   "                     :exclude nil))\n"
                                   "          (\"No Sensitive\" . (:include nil\n"
                                   "                              :exclude (\"/Areas/Financial\\\\.org\\\\'\"\n"
                                   "                                        \"/Areas/Interviews\\\\.org\\\\'\")))\n"
                                   "          ))\n"
                                   "#+end_src\n")))
            ;; toggle 2: re-comment -- must reproduce the original byte-for-byte
            (ps/org-src-comment-test--toggle-lines start-line end-line)
            (should (equal (ps/org-src-comment-test--buffer-string) original))
            ;; toggle 3: uncomment again -- identical to toggle 1's result
            (ps/org-src-comment-test--toggle-lines start-line end-line)
            (should (equal (ps/org-src-comment-test--buffer-string) uncommented))
            ;; toggle 4: re-comment again -- still identical to the original
            (ps/org-src-comment-test--toggle-lines start-line end-line)
            (should (equal (ps/org-src-comment-test--buffer-string) original))))))))

;;; -------------------------------------------------------
;;; mixed region: comments everything, aligned to the minimum indentation
;;; -------------------------------------------------------

(ert-deftest ps/org-src-comment--mixed-region-comments-at-min-indent ()
  "A region with some commented and some uncommented lines is fully
commented, with the new prefixes aligned to the region's minimum
indentation (not each line's own indentation)."
  (ps/org-src-comment-test--with-buffer
      (concat "#+begin_src emacs-lisp\n"
              "  (setq x\n"
              "        '((\"All\" . t)\n"
              "          ;; (\"Work\" . t)\n"
              "          ;; (\"Other\" . t)\n"
              "          ))\n"
              "#+end_src\n")
    (goto-char (point-min))
    (search-forward "'((\"All\"")
    (let ((start-line (line-number-at-pos)))
      (search-forward "(\"Other\" . t)")
      (let ((end-line (line-number-at-pos)))
        (ps/org-src-comment-test--toggle-lines start-line end-line)
        (should (equal (ps/org-src-comment-test--buffer-string)
                       (concat "#+begin_src emacs-lisp\n"
                               "  (setq x\n"
                               "        ;; '((\"All\" . t)\n"
                               "        ;;   ;; (\"Work\" . t)\n"
                               "        ;;   ;; (\"Other\" . t)\n"
                               "          ))\n"
                               "#+end_src\n")))))))

;;; -------------------------------------------------------
;;; blank lines inside the region are left untouched
;;; -------------------------------------------------------

(ert-deftest ps/org-src-comment--blank-line-untouched ()
  "A blank line inside the toggled region is neither commented nor
considered when computing alignment."
  (ps/org-src-comment-test--with-buffer
      "#+begin_src emacs-lisp\n  (setq x 1)\n\n  (setq y 2)\n#+end_src\n"
    (goto-char (point-min))
    (search-forward "(setq x 1)")
    (let ((start-line (line-number-at-pos)))
      (search-forward "(setq y 2)")
      (let ((end-line (line-number-at-pos)))
        (ps/org-src-comment-test--toggle-lines start-line end-line)
        (should (equal (ps/org-src-comment-test--buffer-string)
                       "#+begin_src emacs-lisp\n  ;; (setq x 1)\n\n  ;; (setq y 2)\n#+end_src\n"))))))

;;; -------------------------------------------------------
;;; selection extends one line past the commented block (C-SPC + down-arrow)
;;; -------------------------------------------------------

(ert-deftest ps/org-src-comment--selection-extends-past-commented-block ()
  "A selection that starts at the first commented line and ends at the
start of the following uncommented line is still treated as fully commented
and is uncommented, leaving the trailing line untouched."
  (ps/org-src-comment-test--with-buffer
      (concat "#+begin_src emacs-lisp\n"
              "  (setq x\n"
              "        '((\"All\" . t)\n"
              "          ;; (\"Work\" . t)\n"
              "          ;; (\"Other\" . t)\n"
              "          ))\n"
              "#+end_src\n")
    (goto-char (point-min))
    (search-forward ";; (\"Work\"")
    (beginning-of-line)
    (let ((beg (point)))
      (search-forward "(\"Other\" . t)")
      (forward-line 1)
      (beginning-of-line)
      (ps/org-src-comment-test--toggle-region beg (point)))
    (should (equal (ps/org-src-comment-test--buffer-string)
                    (concat "#+begin_src emacs-lisp\n"
                            "  (setq x\n"
                            "        '((\"All\" . t)\n"
                            "          (\"Work\" . t)\n"
                            "          (\"Other\" . t)\n"
                            "          ))\n"
                            "#+end_src\n")))))

;;; -------------------------------------------------------
;;; mid-line-to-mid-line selection: snapped to whole lines, round trips
;;; -------------------------------------------------------

(ert-deftest ps/org-src-comment--midline-to-midline-roundtrip ()
  "A selection that starts in the middle of one line and ends in the middle
of the next is snapped to whole lines: commenting puts the prefix at the
start of each line (not mid-line), and toggling again from the same
mid-line positions restores the original text exactly."
  (let ((original (concat "#+begin_src emacs-lisp\n"
                           "  (setq ps/file-tree-file-sets\n"
                           "        '((\"All\" . (:include nil :exclude nil))\n"
                           "          ))\n"
                           "#+end_src\n")))
    (ps/org-src-comment-test--with-buffer original
      (goto-char (point-min))
      (search-forward "(setq ")
      (let ((beg (point)))
        (search-forward "'(")
        (let ((end (point)))
          (ps/org-src-comment-test--toggle-region beg end)
          (should (equal (ps/org-src-comment-test--buffer-string)
                         (concat "#+begin_src emacs-lisp\n"
                                 "  ;; (setq ps/file-tree-file-sets\n"
                                 "  ;;       '((\"All\" . (:include nil :exclude nil))\n"
                                 "          ))\n"
                                 "#+end_src\n")))))
      ;; Toggle again from the same mid-line positions -- restores the original.
      (goto-char (point-min))
      (search-forward "(setq ")
      (let ((beg (point)))
        (search-forward "'(")
        (let ((end (point)))
          (ps/org-src-comment-test--toggle-region beg end)
          (should (equal (ps/org-src-comment-test--buffer-string) original)))))))

;;; -------------------------------------------------------
;;; outside a src block: falls back to `comment-line'
;;; -------------------------------------------------------

(ert-deftest ps/org-src-comment--outside-src-block-falls-back ()
  "Outside a src block, the command does not error and leaves the heading
text's first character a comment marker via the normal `comment-line'."
  (ps/org-src-comment-test--with-buffer
      "* Heading\nSome text.\n"
    (goto-char (point-min))
    (forward-line 1)
    (should-not (org-in-src-block-p))
    (ps/org-src-comment-dwim)
    (should (equal (ps/org-src-comment-test--buffer-string)
                   "* Heading\n# Some text.\n"))))

;;; test-ps-org-src-comment.el ends here
