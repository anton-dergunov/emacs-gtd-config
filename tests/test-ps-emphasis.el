;;; test-ps-emphasis.el --- ERT tests for ps-emphasis -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path "lisp")
(require 'ps-emphasis)

(defun ps/emphasis-test--faces (s pos)
  "Return the `face' property of S at POS as a list."
  (let ((face (get-text-property pos 'face s)))
    (cond ((null face) nil)
          ((listp face) face)
          (t (list face)))))

;;; -------------------------------------------------------
;;; marker removal
;;; -------------------------------------------------------

(ert-deftest ps/emphasis-render-drops-the-markers ()
  "Markers vanish from the returned text, so widths match what is drawn."
  (should (equal (substring-no-properties
                  (ps/emphasis-render "Read *Deep Work* today"))
                 "Read Deep Work today"))
  (should (equal (substring-no-properties
                  (ps/emphasis-render "Read /Deep Work/ today"))
                 "Read Deep Work today"))
  (should (equal (substring-no-properties
                  (ps/emphasis-render "Read =Deep Work= today"))
                 "Read Deep Work today")))

(ert-deftest ps/emphasis-render-handles-several-spans ()
  "Two emphasised spans on one line are both unwrapped."
  (should (equal (substring-no-properties
                  (ps/emphasis-render "*Buy* milk and /bread/"))
                 "Buy milk and bread")))

(ert-deftest ps/emphasis-render-leaves-plain-text-alone ()
  "Text without emphasis -- and lone or mid-word markers -- is unchanged."
  (dolist (s '("Plain title" "" "2 * 3 = 6" "some_file_name.org"))
    (should (equal (substring-no-properties (ps/emphasis-render s)) s)))
  (should (null (ps/emphasis-render nil))))

;;; -------------------------------------------------------
;;; faces
;;; -------------------------------------------------------

(ert-deftest ps/emphasis-render-applies-the-org-face ()
  "The wrapped text carries the face `org-emphasis-alist' names for its marker."
  (let ((s (ps/emphasis-render "Read *Deep Work* today")))
    (should (memq 'bold (ps/emphasis-test--faces s 5)))
    ;; ...and only the wrapped text does.
    (should-not (ps/emphasis-test--faces s 0)))
  (should (memq 'italic
                (ps/emphasis-test--faces (ps/emphasis-render "a /b/ c") 2)))
  (should (memq 'org-verbatim
                (ps/emphasis-test--faces (ps/emphasis-render "a =b= c") 2))))

(ert-deftest ps/emphasis-render-keeps-existing-properties ()
  "Emphasis faces are added on top of whatever the string already carried."
  (let* ((input (propertize "Read *Deep Work* today" 'help-echo "raw"))
         (s (ps/emphasis-render input)))
    (should (equal (get-text-property 0 'help-echo s) "raw"))
    (should (equal (get-text-property 5 'help-echo s) "raw"))
    (should (memq 'bold (ps/emphasis-test--faces s 5)))))

;;; test-ps-emphasis.el ends here
