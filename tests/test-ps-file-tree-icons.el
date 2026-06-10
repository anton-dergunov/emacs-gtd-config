;;; test-ps-file-tree-icons.el --- ERT tests for ps-file-tree-icons -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path "lisp")
(require 'ps-file-tree-icons)

(defmacro ps/file-tree-icons-test--with-icon-dir (files &rest body)
  "Create a temp dir containing FILES (a list of names), bind `dir', run BODY.
Each file is created empty.  Cleans up afterward."
  (declare (indent 1))
  `(let ((dir (make-temp-file "ps-file-tree-icons-" t)))
     (unwind-protect
         (progn
           (dolist (name ,files)
             (with-temp-file (expand-file-name name dir) (insert "")))
           ,@body)
       (delete-directory dir t))))

;;; -------------------------------------------------------
;;; build-alist
;;; -------------------------------------------------------

(ert-deftest ps/file-tree-icons--build-counts-svgs-only ()
  "Only .svg files produce entries; other files are ignored."
  (ps/file-tree-icons-test--with-icon-dir '("a.svg" "b.svg" "c.txt" "notes.org")
    (should (= (length (ps/file-tree-icons--build-alist dir)) 2))))

(ert-deftest ps/file-tree-icons--build-categories-are-basenames ()
  "Categories are the SVG file base names (no extension, no directory)."
  (ps/file-tree-icons-test--with-icon-dir '("Work.svg" "Home.svg")
    (let ((cats (mapcar #'car (ps/file-tree-icons--build-alist dir))))
      (should (member "Work" cats))
      (should (member "Home" cats)))))

(ert-deftest ps/file-tree-icons--build-entry-is-abs-path ()
  "Each entry maps CATEGORY to an absolute path of the SVG file."
  (ps/file-tree-icons-test--with-icon-dir '("Work.svg")
    (let ((entry (car (ps/file-tree-icons--build-alist dir))))
      (should (equal (car entry) "Work"))
      (should (string-suffix-p "Work.svg" (cdr entry)))
      (should (file-name-absolute-p (cdr entry))))))

(ert-deftest ps/file-tree-icons--build-empty-dir ()
  "A directory with no SVGs yields nil."
  (ps/file-tree-icons-test--with-icon-dir '("readme.txt")
    (should (null (ps/file-tree-icons--build-alist dir)))))

(ert-deftest ps/file-tree-icons--build-missing-dir ()
  "A non-existent directory yields nil."
  (should (null (ps/file-tree-icons--build-alist "/no/such/dir/at/all"))))

;;; -------------------------------------------------------
;;; merge
;;; -------------------------------------------------------

(ert-deftest ps/file-tree-icons--merge-skips-missing-dir ()
  "merge still finds icons in later dirs when an earlier one is missing."
  (ps/file-tree-icons-test--with-icon-dir '("Work.svg")
    (let ((merged (ps/file-tree-icons--merge (list "/no/such/dir/at/all" dir))))
      (should (= (length merged) 1))
      (should (equal (car (car merged)) "Work")))))

(ert-deftest ps/file-tree-icons--merge-later-dir-overrides-earlier ()
  "When the same category exists in multiple dirs, the last dir wins."
  (ps/file-tree-icons-test--with-icon-dir '("Work.svg")
    (let ((stock-dir dir))
      (ps/file-tree-icons-test--with-icon-dir '("Work.svg")
        (let* ((custom-dir dir)
               (merged (ps/file-tree-icons--merge (list stock-dir custom-dir))))
          (should (= (length merged) 1))
          (should (string-prefix-p custom-dir (cdr (assoc "Work" merged)))))))))

;;; test-ps-file-tree-icons.el ends here
