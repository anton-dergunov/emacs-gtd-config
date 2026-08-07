;;; test-ps-file-tree-icons.el --- ERT tests for ps-file-tree-icons -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path "lisp")
(require 'ps-file-tree-icons)

;; Point at the repo's shipped icon assets.
(let ((root (locate-dominating-file default-directory "icons")))
  (setq ps/material-icons-codepoints-file
        (expand-file-name "icons/material-symbols.codepoints" root))
  (setq ps/file-tree-icon-fallback-dir (expand-file-name "icons" root)))

;;; -------------------------------------------------------
;;; glyph icon strings
;;; -------------------------------------------------------

(ert-deftest ps/file-tree-icons--glyph-is-image-with-spacer ()
  "A glyph icon is an image-display string followed by the standard spacer."
  ;; Stub create-image to avoid SVG dependency in batch Emacs.
  (cl-letf (((symbol-function 'create-image)
             (lambda (data &optional type data-p &rest props)
               (apply #'list 'image :type type (if data-p :data :file) data props))))
    (let* ((ps/material-icons--table nil)
           (icon (ps/file-tree-icons--glyph "folder")))
      (should (eq (car (get-text-property 0 'display icon)) 'image))
      (should (> (length icon) 1))
      (should (equal (get-text-property 1 'display icon)
                      (list 'space :width ps/file-tree-name-spacing))))))

(ert-deftest ps/file-tree-icons--glyph-uses-file-tree-ascent ()
  "The glyph image takes its `:ascent' from `ps/file-tree-icon-ascent'."
  ;; Stub create-image to avoid SVG dependency in batch Emacs.
  (cl-letf (((symbol-function 'create-image)
             (lambda (data &optional type data-p &rest props)
               (apply #'list 'image :type type (if data-p :data :file) data props))))
    (let* ((ps/material-icons--table nil)
           (ps/file-tree-icon-ascent 80)
           (image (get-text-property 0 'display (ps/file-tree-icons--glyph "folder"))))
      (should (equal (image-property image :ascent) 80)))))

(ert-deftest ps/file-tree-icons--glyph-unknown-is-nil ()
  "An unknown icon name yields nil (no icon registered)."
  (let ((ps/material-icons--table nil))
    (should (null (ps/file-tree-icons--glyph "no_such_icon_xyz")))))

;;; -------------------------------------------------------
;;; fallback SVGs
;;; -------------------------------------------------------

(ert-deftest ps/file-tree-icons--fallback-svg-found ()
  "A shipped fallback SVG (named after its glyph) renders to an image string."
  ;; Stub create-image to avoid SVG dependency in batch Emacs.
  (cl-letf (((symbol-function 'create-image)
             (lambda (data &optional type data-p &rest props)
               (apply #'list 'image :type type (if data-p :data :file) data props))))
    (let ((icon (ps/file-tree-icons--fallback-svg ps/file-tree-icons--file)))
      (should icon)
      (should (eq (car (get-text-property 0 'display icon)) 'image))
      (should (equal (image-property (get-text-property 0 'display icon) :file)
                      (expand-file-name "draft.svg" ps/file-tree-icon-fallback-dir))))))

(ert-deftest ps/file-tree-icons--fallback-svg-missing-is-nil ()
  "A missing fallback SVG yields nil."
  (should (null (ps/file-tree-icons--fallback-svg "NoSuchIcon"))))

;;; -------------------------------------------------------
;;; icon-key collection (pure)
;;; -------------------------------------------------------

(defmacro ps/file-tree-icons-test--with-tree (entries &rest body)
  "Create a temp dir containing ENTRIES, bind `dir', run BODY, then clean up.
Each entry is a relative name; names ending in \"/\" become directories, the
rest become empty files (their parent directories are created as needed)."
  (declare (indent 1))
  `(let ((dir (make-temp-file "ps-file-tree-icons-" t)))
     (unwind-protect
         (progn
           (dolist (name ,entries)
             (if (string-suffix-p "/" name)
                 (make-directory (expand-file-name name dir) t)
               (let ((file (expand-file-name name dir)))
                 (make-directory (file-name-directory file) t)
                 (with-temp-file file (insert "")))))
           ,@body)
       (delete-directory dir t))))

(defun ps/file-tree-icons-test--icon (keys key)
  "Return the glyph name KEYS assigns to KEY -- the last one wins."
  (cdr (assoc key (reverse keys))))

(ert-deftest ps/file-tree-icons--collect-registers-generic-dir-icons ()
  "Nested directories get our folder glyphs, not treemacs's inherited icons.
Regression: only `root-open'/`root-closed' used to be registered, so any
directory below a project root fell through to treemacs's own dir icon."
  (ps/file-tree-icons-test--with-tree '("ML/older/Test.org")
    (let ((ps/material-icons-folder-map nil)
          (ps/material-icons-folder-contents-map nil)
          (ps/material-icons-category-map nil))
      (let ((keys (ps/file-tree-icons--collect dir)))
        (should (equal (ps/file-tree-icons-test--icon keys 'dir-closed)
                       ps/file-tree-icons--folder-closed))
        (should (equal (ps/file-tree-icons-test--icon keys 'dir-open)
                       ps/file-tree-icons--folder-open))
        (should (equal (ps/file-tree-icons-test--icon keys 'root-closed)
                       ps/file-tree-icons--folder-closed))))))

(ert-deftest ps/file-tree-icons--collect-maps-directories-at-any-depth ()
  "`ps/material-icons-folder-map' icons the directory node itself, nested too."
  (ps/file-tree-icons-test--with-tree '("ML/older/Test.org" "Current/Week.org")
    (let ((ps/material-icons-folder-map '(("older" . "history")
                                          ("Current" . "calendar_month")))
          (ps/material-icons-folder-contents-map nil)
          (ps/material-icons-category-map nil))
      (let ((keys (ps/file-tree-icons--collect dir)))
        (should (equal (ps/file-tree-icons-test--icon keys "older-closed") "history"))
        (should (equal (ps/file-tree-icons-test--icon keys "older-open") "history"))
        (should (equal (ps/file-tree-icons-test--icon keys "current-closed")
                       "calendar_month"))
        ;; An unmapped directory keeps the generic glyph.
        (should-not (ps/file-tree-icons-test--icon keys "ml-closed"))))))

(ert-deftest ps/file-tree-icons--collect-matches-relative-paths ()
  "A folder-map key may be a base-relative path, targeting just one directory."
  (ps/file-tree-icons-test--with-tree '("ML/older/A.org" "Work/older/B.org")
    (let ((ps/material-icons-folder-map '(("ML/older" . "history")))
          (ps/material-icons-folder-contents-map nil)
          (ps/material-icons-category-map nil))
      ;; Both directories are named "older", so the shared key is set once --
      ;; what matters is that the path key resolves at all.
      (should (equal (ps/file-tree-icons-test--icon
                      (ps/file-tree-icons--collect dir) "older-closed")
                     "history")))))

(ert-deftest ps/file-tree-icons--collect-icons-root-from-folder-map ()
  "A folder-map entry for the Org directory itself icons the project root."
  (ps/file-tree-icons-test--with-tree '("Inbox.org")
    (let* ((name (file-name-nondirectory (directory-file-name dir)))
           (ps/material-icons-folder-map (list (cons name "book")))
           (ps/material-icons-folder-contents-map nil)
           (ps/material-icons-category-map nil)
           (keys (ps/file-tree-icons--collect dir)))
      (should (equal (ps/file-tree-icons-test--icon keys 'root-closed) "book"))
      (should (equal (ps/file-tree-icons-test--icon keys 'root-open) "book")))))

(ert-deftest ps/file-tree-icons--collect-files-at-any-depth ()
  "Every .org file gets an icon key, however deeply nested."
  (ps/file-tree-icons-test--with-tree '("Inbox.org" "ML/Deep.org" "ML/older/Test.org")
    (let ((ps/material-icons-folder-map nil)
          (ps/material-icons-folder-contents-map nil)
          (ps/material-icons-category-map '(("Deep" . "neurology"))))
      (let ((keys (ps/file-tree-icons--collect dir)))
        (should (equal (ps/file-tree-icons-test--icon keys "deep.org") "neurology"))
        ;; Unmapped files, nested or not, fall back to the generic file glyph.
        (should (equal (ps/file-tree-icons-test--icon keys "inbox.org")
                       ps/file-tree-icons--file))
        (should (equal (ps/file-tree-icons-test--icon keys "test.org")
                       ps/file-tree-icons--file))))))

(ert-deftest ps/file-tree-icons--collect-registers-the-org-extension ()
  "The \"org\" extension key carries the generic file glyph.
Regression: only per-filename keys were registered, so a file created after
the walk -- or one already deleted -- fell through to treemacs's own icon."
  (ps/file-tree-icons-test--with-tree '("Inbox.org")
    (let ((ps/material-icons-folder-map nil)
          (ps/material-icons-folder-contents-map nil)
          (ps/material-icons-category-map '(("Inbox" . "inbox"))))
      (let ((keys (ps/file-tree-icons--collect dir)))
        (should (equal (ps/file-tree-icons-test--icon keys "org")
                       ps/file-tree-icons--file))
        ;; A mapped file still wins over the extension key.
        (should (equal (ps/file-tree-icons-test--icon keys "inbox.org") "inbox"))))))

(ert-deftest ps/file-tree-icons--collect-folder-contents-beats-category ()
  "`folder-contents-map' icons every file under a folder, overriding categories."
  (ps/file-tree-icons-test--with-tree '("Vision/2026.org" "Vision/old/2024.org"
                                        "ML/Deep.org")
    (let ((ps/material-icons-folder-map nil)
          (ps/material-icons-folder-contents-map '(("Vision" . "mountain_flag")))
          (ps/material-icons-category-map '(("2026" . "calculate")
                                            ("Deep" . "neurology"))))
      (let ((keys (ps/file-tree-icons--collect dir)))
        (should (equal (ps/file-tree-icons-test--icon keys "2026.org") "mountain_flag"))
        ;; Recursive: a file in a nested subfolder is covered too.
        (should (equal (ps/file-tree-icons-test--icon keys "2024.org") "mountain_flag"))
        ;; Files outside the folder are untouched.
        (should (equal (ps/file-tree-icons-test--icon keys "deep.org") "neurology"))))))

(ert-deftest ps/file-tree-icons--collect-skips-ignored-names ()
  "Ignored files and directories contribute no icon keys."
  (ps/file-tree-icons-test--with-tree '("init.org" ".git/config.org" "Inbox.org")
    (let ((ps/file-tree-ignored-files '("\\`init\\.org\\'" "\\`\\."))
          (ps/material-icons-folder-map nil)
          (ps/material-icons-folder-contents-map nil)
          (ps/material-icons-category-map nil))
      (let ((keys (ps/file-tree-icons--collect dir)))
        (should (ps/file-tree-icons-test--icon keys "inbox.org"))
        (should-not (ps/file-tree-icons-test--icon keys "init.org"))
        (should-not (ps/file-tree-icons-test--icon keys "config.org"))))))

;;; test-ps-file-tree-icons.el ends here
