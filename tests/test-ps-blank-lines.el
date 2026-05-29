;;; test-ps-blank-lines.el --- ERT tests for ps-blank-lines -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path "lisp")
(require 'ps-blank-lines)

;; Declared special so the driver test's `let' binding is dynamic.
(defvar org-agenda-files)

(defmacro ps/blank-lines-test--with-content (content &rest body)
  "Run BODY in a temp buffer containing CONTENT (point left at start)."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     ,@body))

(ert-deftest ps/blank-lines--inserts-and-counts ()
  "Blank lines are inserted before headers lacking one; count is returned.
A header at point-min also gets a leading blank (preserved legacy behavior)."
  (ps/blank-lines-test--with-content "* A\ntext\n* B\n"
    (let ((n (ps/blank-lines--reinsert-in-buffer)))
      (should (= n 2))
      (should (equal (buffer-string) "\n* A\ntext\n\n* B\n")))))

(ert-deftest ps/blank-lines--idempotent ()
  "A second pass over already-spaced content makes no changes."
  (ps/blank-lines-test--with-content "* A\ntext\n* B\n"
    (ps/blank-lines--reinsert-in-buffer)
    (let ((before (buffer-string))
          (n (ps/blank-lines--reinsert-in-buffer)))
      (should (= n 0))
      (should (equal (buffer-string) before)))))

(ert-deftest ps/blank-lines--respects-existing-blank ()
  "A header already preceded by a blank line is left untouched."
  (ps/blank-lines-test--with-content "\n* A\nbody\n\n* B\n"
    (let ((n (ps/blank-lines--reinsert-in-buffer)))
      (should (= n 0))
      (should (equal (buffer-string) "\n* A\nbody\n\n* B\n")))))

(ert-deftest ps/blank-lines--no-headers ()
  "Content with no level-1 headers is unchanged with zero count."
  (ps/blank-lines-test--with-content "just some text\nmore text\n"
    (let ((n (ps/blank-lines--reinsert-in-buffer)))
      (should (= n 0))
      (should (equal (buffer-string) "just some text\nmore text\n")))))

(ert-deftest ps/blank-lines--only-level-1 ()
  "Deeper headings (**, ***) are not affected, only level-1 (* )."
  (ps/blank-lines-test--with-content "\n* A\n** sub\n*** subsub\n"
    (let ((n (ps/blank-lines--reinsert-in-buffer)))
      (should (= n 0))
      (should (equal (buffer-string) "\n* A\n** sub\n*** subsub\n")))))

(ert-deftest ps/blank-lines--reinsert-is-interactive ()
  "The agenda-wide driver command is interactive."
  (should (commandp 'ps/blank-lines-reinsert)))

(ert-deftest ps/blank-lines--reinsert-driver-rewrites-files ()
  "The driver rewrites all agenda files and reports per-file + total counts."
  (let ((dir (make-temp-file "ps-blank-driver-" t)))
    (unwind-protect
        (let ((fa (expand-file-name "a.org" dir))
              (fb (expand-file-name "b.org" dir)))
          (with-temp-file fa (insert "* A\nx\n* B\n"))
          (with-temp-file fb (insert "* C\ny\n"))
          (let ((org-agenda-files (list fa fb)))
            (ps/blank-lines-reinsert))
          (should (equal (with-temp-buffer (insert-file-contents fa) (buffer-string))
                         "\n* A\nx\n\n* B\n"))
          (should (equal (with-temp-buffer (insert-file-contents fb) (buffer-string))
                         "\n* C\ny\n"))
          (let ((report (get-buffer "*Org File Changes*")))
            (should report)
            (with-current-buffer report
              (let ((s (buffer-string)))
                (should (string-match-p "a\\.org" s))
                (should (string-match-p "b\\.org" s))
                (should (string-match-p "Total lines changed: 3" s))))))
      (when (get-buffer "*Org File Changes*") (kill-buffer "*Org File Changes*"))
      (delete-directory dir t))))