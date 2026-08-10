;;; test-ps-prose-width.el --- ERT tests for ps-prose-width -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path "lisp")
(require 'ps-prose-width)

;; `(defvar my-org-base-directory)' with no value, as in ps-org-files.el, only
;; marks the symbol special within that file -- repeat it here so the `let'
;; bindings below are dynamic rather than lexical.
(defvar my-org-base-directory)
(defvar ps/org-files-root)

(defmacro ps/prose-width-test--with-file (name &rest body)
  "Create a temp dir containing a file called NAME, visit it, run BODY.
`ps/org-files-root' is bound to the temp dir so `ps/org-files-in-scope-p'
(called with no args by `ps/prose-width--enabled-p') resolves against it."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "ps-prose-width-" t))
          (ps/org-files-root dir)
          (buf (progn
                 (with-temp-file (expand-file-name ,name dir) (insert ""))
                 (find-file-noselect (expand-file-name ,name dir)))))
     (unwind-protect
         (with-current-buffer buf ,@body)
       (kill-buffer buf)
       (delete-directory dir t))))

(ert-deftest ps/prose-width-test-disabled-when-columns-zero ()
  "Zero columns disables margins even for an otherwise in-scope file."
  (ps/prose-width-test--with-file "Inbox.org"
    (let ((ps/prose-width-columns 0))
      (should-not (ps/prose-width--enabled-p)))))

(ert-deftest ps/prose-width-test-enabled-for-in-scope-file ()
  "A positive width enables margins for a plain in-scope .org file."
  (ps/prose-width-test--with-file "Inbox.org"
    (let ((ps/prose-width-columns 120))
      (should (ps/prose-width--enabled-p)))))

(ert-deftest ps/prose-width-test-disabled-for-excluded-file ()
  "workspace.org is excluded regardless of `ps/prose-width-columns'."
  (ps/prose-width-test--with-file "workspace.org"
    (let ((ps/prose-width-columns 120))
      (should-not (ps/prose-width--enabled-p)))))

;;; test-ps-prose-width.el ends here
