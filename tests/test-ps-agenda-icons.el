;;; test-ps-agenda-icons.el --- ERT tests for ps-agenda-icons -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path "lisp")
(require 'ps-agenda-icons)

;; Point at the repo's shipped codepoints list.
(setq ps/material-icons-codepoints-file
      (expand-file-name "icons/material-symbols.codepoints"
                        (locate-dominating-file default-directory "icons")))

(defmacro ps/agenda-icons-test--with-map (map &rest body)
  "Run BODY with `ps/material-icons-category-map' bound to MAP and a fresh table."
  (declare (indent 1))
  `(let ((ps/material-icons-category-map ,map)
         (ps/material-icons--table nil))
     ,@body))

;;; -------------------------------------------------------
;;; build-alist
;;; -------------------------------------------------------

(ert-deftest ps/agenda-icons--build-entry-shape ()
  "Each entry is (CATEGORY SVG svg t :ascent center :height H) with SVG data."
  (ps/agenda-icons-test--with-map '(("Blog" . "edit_square"))
    (let ((entry (car (ps/agenda-icons--build-alist))))
      (should (equal (nth 0 entry) "Blog"))
      (should (string-match-p "&#xf88d;" (nth 1 entry)))
      (should (eq (nth 2 entry) 'svg))
      (should (eq (nth 3 entry) t))
      (should (eq (nth 4 entry) :ascent))
      (should (eq (nth 5 entry) 'center))
      (should (eq (nth 6 entry) :height))
      (should (integerp (nth 7 entry))))))

(ert-deftest ps/agenda-icons--build-preserves-order-and-count ()
  "Every resolvable map entry yields one alist entry, in order."
  (ps/agenda-icons-test--with-map '(("Blog" . "edit_square")
                                     ("Work" . "badge"))
    (let ((alist (ps/agenda-icons--build-alist)))
      (should (= (length alist) 2))
      (should (equal (mapcar #'car alist) '("Blog" "Work"))))))

(ert-deftest ps/agenda-icons--build-skips-unresolved ()
  "Entries whose icon name does not resolve are skipped."
  (ps/agenda-icons-test--with-map '(("Blog" . "edit_square")
                                     ("Bogus" . "no_such_icon_xyz"))
    (let ((alist (ps/agenda-icons--build-alist)))
      (should (= (length alist) 1))
      (should (equal (caar alist) "Blog")))))

(ert-deftest ps/agenda-icons--build-empty-map ()
  "An empty category map yields nil."
  (ps/agenda-icons-test--with-map nil
    (should (null (ps/agenda-icons--build-alist)))))

;;; -------------------------------------------------------
;;; apply
;;; -------------------------------------------------------

;; `customize-set-value' writes via `set-default', so assert against
;; `default-value'. Save/restore the global so the shared process stays clean.

;; `ps/material-icons-available-p' needs a font backend (absent in batch), so
;; stub it to exercise `apply' deterministically.

(ert-deftest ps/agenda-icons--apply-sets-variable ()
  "apply sets org-agenda-category-icon-alist (default value) to the built alist."
  (ps/agenda-icons-test--with-map '(("Blog" . "edit_square") ("Work" . "badge"))
    (let ((bound (boundp 'org-agenda-category-icon-alist))
          (saved (and (boundp 'org-agenda-category-icon-alist)
                      (default-value 'org-agenda-category-icon-alist))))
      (unwind-protect
          (cl-letf (((symbol-function 'ps/material-icons-available-p)
                     (lambda () t)))
            (ps/agenda-icons-apply)
            (should (= (length (default-value 'org-agenda-category-icon-alist)) 2)))
        (if bound
            (set-default 'org-agenda-category-icon-alist saved)
          (makunbound 'org-agenda-category-icon-alist))))))

(ert-deftest ps/agenda-icons--apply-empty-map-noop ()
  "apply does not touch the variable when the map is empty."
  (ps/agenda-icons-test--with-map nil
    (let ((bound (boundp 'org-agenda-category-icon-alist))
          (saved (and (boundp 'org-agenda-category-icon-alist)
                      (default-value 'org-agenda-category-icon-alist))))
      (unwind-protect
          (cl-letf (((symbol-function 'ps/material-icons-available-p)
                     (lambda () t)))
            (set-default 'org-agenda-category-icon-alist 'sentinel)
            (ps/agenda-icons-apply)
            (should (eq (default-value 'org-agenda-category-icon-alist) 'sentinel)))
        (if bound
            (set-default 'org-agenda-category-icon-alist saved)
          (makunbound 'org-agenda-category-icon-alist))))))

;;; test-ps-agenda-icons.el ends here
