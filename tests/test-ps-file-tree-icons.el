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
  (let* ((ps/material-icons--table nil)
         (icon (ps/file-tree-icons--glyph "folder")))
    (should (eq (car (get-text-property 0 'display icon)) 'image))
    (should (> (length icon) 1))
    (should (equal (get-text-property 1 'display icon)
                    (list 'space :width ps/file-tree-name-spacing)))))

(ert-deftest ps/file-tree-icons--glyph-uses-file-tree-ascent ()
  "The glyph image takes its `:ascent' from `ps/file-tree-icon-ascent'."
  (let* ((ps/material-icons--table nil)
         (ps/file-tree-icon-ascent 80)
         (image (get-text-property 0 'display (ps/file-tree-icons--glyph "folder"))))
    (should (equal (image-property image :ascent) 80))))

(ert-deftest ps/file-tree-icons--glyph-unknown-is-nil ()
  "An unknown icon name yields nil (no icon registered)."
  (let ((ps/material-icons--table nil))
    (should (null (ps/file-tree-icons--glyph "no_such_icon_xyz")))))

;;; -------------------------------------------------------
;;; fallback SVGs
;;; -------------------------------------------------------

(ert-deftest ps/file-tree-icons--fallback-svg-found ()
  "A shipped fallback SVG (named after its glyph) renders to an image string."
  (let ((icon (ps/file-tree-icons--fallback-svg ps/file-tree-icons--file)))
    (should icon)
    (should (eq (car (get-text-property 0 'display icon)) 'image))
    (should (equal (image-property (get-text-property 0 'display icon) :file)
                    (expand-file-name "draft.svg" ps/file-tree-icon-fallback-dir)))))

(ert-deftest ps/file-tree-icons--fallback-svg-missing-is-nil ()
  "A missing fallback SVG yields nil."
  (should (null (ps/file-tree-icons--fallback-svg "NoSuchIcon"))))

;;; test-ps-file-tree-icons.el ends here
