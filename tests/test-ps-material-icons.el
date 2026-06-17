;;; test-ps-material-icons.el --- ERT tests for ps-material-icons -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path "lisp")
(require 'ps-material-icons)

;; Use the real codepoints file shipped in the repo.
(setq ps/material-icons-codepoints-file
      (expand-file-name "icons/material-symbols.codepoints"
                        (locate-dominating-file default-directory "icons")))

(defmacro ps/material-icons-test--fresh-table (&rest body)
  "Run BODY with the codepoints table reset, so it is reparsed."
  `(let ((ps/material-icons--table nil)) ,@body))

;;; -------------------------------------------------------
;;; codepoint resolution
;;; -------------------------------------------------------

(ert-deftest ps/material-icons--codepoint-resolves-known-name ()
  "A known snake_case name resolves to its codepoint."
  (ps/material-icons-test--fresh-table
   (should (equal (ps/material-icons-codepoint "edit_square") #xf88d))
   (should (equal (ps/material-icons-codepoint "folder") #xe2c7))))

(ert-deftest ps/material-icons--codepoint-normalizes-name ()
  "Title Case with spaces resolves the same as snake_case."
  (ps/material-icons-test--fresh-table
   (should (equal (ps/material-icons-codepoint "Edit Square")
                  (ps/material-icons-codepoint "edit_square")))
   (should (equal (ps/material-icons-codepoint "  CALENDAR-MONTH ")
                  (ps/material-icons-codepoint "calendar_month")))))

(ert-deftest ps/material-icons--codepoint-unknown-is-nil ()
  "An unknown or empty name yields nil."
  (ps/material-icons-test--fresh-table
   (should (null (ps/material-icons-codepoint "no_such_icon_xyz")))
   (should (null (ps/material-icons-codepoint "")))
   (should (null (ps/material-icons-codepoint nil)))))

;;; -------------------------------------------------------
;;; svg rendering
;;; -------------------------------------------------------

(ert-deftest ps/material-icons--svg-embeds-codepoint-entity ()
  "The SVG embeds the resolved codepoint as a lowercase hex entity."
  (ps/material-icons-test--fresh-table
   (should (string-match-p "&#xf88d;" (ps/material-icons-svg "edit_square")))))

(ert-deftest ps/material-icons--svg-uses-font-and-color ()
  "The SVG uses the configured font family and the given/the default color."
  (ps/material-icons-test--fresh-table
   (let ((ps/material-icons-font-family "Material Symbols Outlined")
         (ps/material-icons-color "#5f6368"))
     (let ((svg (ps/material-icons-svg "work")))
       (should (string-match-p "font-family=\"Material Symbols Outlined\"" svg))
       (should (string-match-p "fill=\"#5f6368\"" svg)))
     (should (string-match-p "fill=\"#123456\""
                             (ps/material-icons-svg "work" "#123456"))))))

(ert-deftest ps/material-icons--svg-unknown-is-nil ()
  "An unknown name produces no SVG."
  (ps/material-icons-test--fresh-table
   (should (null (ps/material-icons-svg "no_such_icon_xyz")))))

;;; -------------------------------------------------------
;;; image
;;; -------------------------------------------------------

(ert-deftest ps/material-icons--image-honors-ascent-and-height ()
  "An explicit ascent and height are passed to the image."
  ;; Stub create-image to avoid SVG dependency in batch Emacs.
  (cl-letf (((symbol-function 'create-image)
             (lambda (data &optional type data-p &rest props)
               (apply #'list 'image :type type (if data-p :data :file) data props))))
    (ps/material-icons-test--fresh-table
     (let ((img (ps/material-icons-image "folder" 90 17)))
       (should (eq (car img) 'image))
       (should (equal (image-property img :ascent) 90))
       (should (equal (image-property img :height) 17)))
     ;; default ascent is center
     (should (equal (image-property (ps/material-icons-image "folder" nil 17)
                                    :ascent)
                    'center))
     ;; unknown name -> nil
     (should (null (ps/material-icons-image "no_such_icon_xyz"))))))

;;; -------------------------------------------------------
;;; pixel height
;;; -------------------------------------------------------

(ert-deftest ps/material-icons--pixel-height-integer-passthrough ()
  "An integer height is returned verbatim."
  (let ((ps/material-icons-height 23))
    (should (equal (ps/material-icons--pixel-height) 23))))

(ert-deftest ps/material-icons--pixel-height-auto-batch-fallback ()
  "In batch (no graphical display) `auto' falls back to 20."
  (let ((ps/material-icons-height 'auto))
    (should (equal (ps/material-icons--pixel-height) 20))))

;;; -------------------------------------------------------
;;; add (merge)
;;; -------------------------------------------------------

(ert-deftest ps/material-icons--add-merges-and-overrides ()
  "`add' inserts new entries and overrides existing ones by key."
  (let ((ps/material-icons-category-map '(("Blog" . "old") ("Work" . "work"))))
    (ps/material-icons-add '(("Blog" . "edit_square") ("News" . "newsmode")))
    (should (equal (cdr (assoc "Blog" ps/material-icons-category-map)) "edit_square"))
    (should (equal (cdr (assoc "News" ps/material-icons-category-map)) "newsmode"))
    (should (equal (cdr (assoc "Work" ps/material-icons-category-map)) "work"))
    ;; no duplicate Blog entry
    (should (= 1 (cl-count "Blog" ps/material-icons-category-map
                           :key #'car :test #'equal)))))

;;; test-ps-material-icons.el ends here
