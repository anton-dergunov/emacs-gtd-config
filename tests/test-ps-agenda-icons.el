;;; test-ps-agenda-icons.el --- ERT tests for ps-agenda-icons -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path "lisp")
(require 'ps-agenda-icons)

(defmacro ps/agenda-icons-test--with-icon-dir (files &rest body)
  "Create a temp dir containing FILES (a list of names), bind `dir', run BODY.
Each file is created empty.  Cleans up afterward."
  (declare (indent 1))
  `(let ((dir (make-temp-file "ps-agenda-icons-" t)))
     (unwind-protect
         (progn
           (dolist (name ,files)
             (with-temp-file (expand-file-name name dir) (insert "")))
           ,@body)
       (delete-directory dir t))))

;;; -------------------------------------------------------
;;; build-alist
;;; -------------------------------------------------------

(ert-deftest ps/agenda-icons--build-counts-svgs-only ()
  "Only .svg files produce entries; other files are ignored."
  (ps/agenda-icons-test--with-icon-dir '("a.svg" "b.svg" "c.txt" "notes.org")
    (should (= (length (ps/agenda-icons--build-alist dir)) 2))))

(ert-deftest ps/agenda-icons--build-categories-are-basenames ()
  "Categories are the SVG file base names (no extension, no directory)."
  (ps/agenda-icons-test--with-icon-dir '("Work.svg" "Home.svg")
    (let ((cats (mapcar #'car (ps/agenda-icons--build-alist dir))))
      (should (member "Work" cats))
      (should (member "Home" cats)))))

(ert-deftest ps/agenda-icons--build-entry-shape ()
  "Each entry has shape (CATEGORY FILE nil nil :ascent center) with an abs path."
  (ps/agenda-icons-test--with-icon-dir '("Work.svg")
    (let ((entry (car (ps/agenda-icons--build-alist dir))))
      (should (equal (nth 0 entry) "Work"))
      (should (string-suffix-p "Work.svg" (nth 1 entry)))
      (should (file-name-absolute-p (nth 1 entry)))
      (should (null (nth 2 entry)))
      (should (null (nth 3 entry)))
      (should (eq (nth 4 entry) :ascent))
      (should (eq (nth 5 entry) 'center)))))

(ert-deftest ps/agenda-icons--build-empty-dir ()
  "A directory with no SVGs yields nil."
  (ps/agenda-icons-test--with-icon-dir '("readme.txt")
    (should (null (ps/agenda-icons--build-alist dir)))))

(ert-deftest ps/agenda-icons--build-missing-dir ()
  "A non-existent directory yields nil."
  (should (null (ps/agenda-icons--build-alist "/no/such/dir/at/all"))))

;;; -------------------------------------------------------
;;; apply
;;; -------------------------------------------------------

;; `customize-set-value' writes the variable's default via `set-default', so
;; assert against `default-value' (a lexical `let' would not observe it). Save
;; and restore the global so the shared test process is not polluted.

(ert-deftest ps/agenda-icons--apply-sets-variable ()
  "apply sets org-agenda-category-icon-alist (default value) to the built alist."
  (ps/agenda-icons-test--with-icon-dir '("a.svg" "b.svg")
    (let ((bound (boundp 'org-agenda-category-icon-alist))
          (saved (and (boundp 'org-agenda-category-icon-alist)
                      (default-value 'org-agenda-category-icon-alist))))
      (unwind-protect
          (progn
            (ps/agenda-icons-apply dir)
            (should (= (length (default-value 'org-agenda-category-icon-alist)) 2))
            (should (equal (default-value 'org-agenda-category-icon-alist)
                           (ps/agenda-icons--build-alist dir))))
        (if bound
            (set-default 'org-agenda-category-icon-alist saved)
          (makunbound 'org-agenda-category-icon-alist))))))

(ert-deftest ps/agenda-icons--apply-missing-dir-noop ()
  "apply does not touch the variable when the directory is missing."
  (let ((bound (boundp 'org-agenda-category-icon-alist))
        (saved (and (boundp 'org-agenda-category-icon-alist)
                    (default-value 'org-agenda-category-icon-alist))))
    (unwind-protect
        (progn
          (set-default 'org-agenda-category-icon-alist 'sentinel)
          (ps/agenda-icons-apply "/no/such/dir/at/all")
          (should (eq (default-value 'org-agenda-category-icon-alist) 'sentinel)))
      (if bound
          (set-default 'org-agenda-category-icon-alist saved)
        (makunbound 'org-agenda-category-icon-alist)))))

;;; test-ps-agenda-icons.el ends here
