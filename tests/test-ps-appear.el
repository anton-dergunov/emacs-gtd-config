;;; test-ps-appear.el --- ERT tests for ps-appear -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'cl-lib)
(add-to-list 'load-path "lisp")
(require 'ps-appear)

;; org-appear is not installed in the batch test env.  A valueless `defvar'
;; marks a variable special only for the file it appears in, so the three
;; internals the tests bind have to be re-declared here as well -- otherwise
;; `let' binds them lexically and `ps/appear--reassert-reveal' never sees the
;; values.  The one org-appear function it calls is stubbed per test.
(defvar org-appear-mode)
(defvar org-appear--prev-elem)
(defvar org-appear--elem-toggled)

(defmacro ps/appear-test--with-buffer (&rest body)
  "Run BODY in a fontified Org buffer holding one bold word.
Not `with-temp-buffer': font-lock refuses buffers whose name starts with a
space.  Point is left after the opening `*', and `ps/appear-test--marker'
names the position of that `*'."
  (declare (indent 0))
  `(let ((buf (generate-new-buffer "ps-appear-test.org")))
     (unwind-protect
         (with-current-buffer buf
           (org-mode)
           (setq-local org-hide-emphasis-markers t)
           (insert "Some *Whisper* text and more words after it.\n")
           (font-lock-mode 1)
           (font-lock-ensure)
           (goto-char (point-min))
           (search-forward "*Whis")
           (let ((ps/appear-test--marker (match-beginning 0)))
             (ignore ps/appear-test--marker)
             ,@body))
       (with-current-buffer buf (set-buffer-modified-p nil))
       (kill-buffer buf))))

(defun ps/appear-test--refontify-line ()
  "Refontify the current line the way a mid-edit redisplay does."
  (font-lock-fontify-region (line-beginning-position) (line-end-position)))

(defun ps/appear-test--reveal (pos)
  "Remove the hidden-marker property at POS, as org-appear's reveal does."
  (with-silent-modifications
    (remove-text-properties pos (1+ pos) '(invisible nil))))

(defun ps/appear-test--hidden-p (pos)
  "Non-nil when the emphasis marker at POS is hidden."
  (get-text-property pos 'invisible))

(ert-deftest ps/appear-markers-start-out-hidden ()
  "Baseline: Org fontification hides the emphasis markers."
  (ps/appear-test--with-buffer
    (should (ps/appear-test--hidden-p ps/appear-test--marker))))

(ert-deftest ps/appear-reveal-survives-refontification ()
  "A revealed marker stays revealed when the line is refontified.
This is the regression: without the advice, Org's fontification re-hides the
markers on the redisplay that follows every keystroke typed inside the
element, so the raw syntax collapses while it is being edited."
  (ps/appear-test--with-buffer
    (ps/appear-setup)
    (unwind-protect
        (cl-letf (((symbol-function 'org-appear--show-invisible)
                   (lambda (elem) (ps/appear-test--reveal elem))))
          (let ((org-appear-mode t)
                (org-appear--elem-toggled t)
                (org-appear--prev-elem ps/appear-test--marker))
            (ps/appear-test--reveal ps/appear-test--marker)
            (should-not (ps/appear-test--hidden-p ps/appear-test--marker))
            (ps/appear-test--refontify-line)
            (should-not (ps/appear-test--hidden-p ps/appear-test--marker))))
      (advice-remove 'font-lock-default-fontify-region
                     #'ps/appear--reassert-reveal))))

(ert-deftest ps/appear-reveal-is-lost-without-setup ()
  "Without the advice the reveal is lost, so the test above has teeth."
  (ps/appear-test--with-buffer
    (ps/appear-test--reveal ps/appear-test--marker)
    (should-not (ps/appear-test--hidden-p ps/appear-test--marker))
    (ps/appear-test--refontify-line)
    (should (ps/appear-test--hidden-p ps/appear-test--marker))))

(defun ps/appear-test--reassert-calls (mode toggled elem)
  "Return the number of reveals `ps/appear--reassert-reveal' performs.
MODE, TOGGLED and ELEM stand in for org-appear's three tracked variables."
  (let ((calls 0))
    (cl-letf (((symbol-function 'org-appear--show-invisible)
               (lambda (_elem) (cl-incf calls))))
      (let ((org-appear-mode mode)
            (org-appear--elem-toggled toggled)
            (org-appear--prev-elem elem))
        (ps/appear--reassert-reveal)))
    calls))

(ert-deftest ps/appear-reassert-respects-guards ()
  "The reveal is re-asserted only while org-appear has an element toggled.
The `elem-toggled' guard is what lets org-appear hide on the way out:
`org-appear--post-cmd' clears it before it hides."
  (should (= 1 (ps/appear-test--reassert-calls t t 'elem)))
  (should (= 0 (ps/appear-test--reassert-calls nil t 'elem)))
  (should (= 0 (ps/appear-test--reassert-calls t nil 'elem)))
  (should (= 0 (ps/appear-test--reassert-calls t t nil))))

(ert-deftest ps/appear-setup-is-idempotent ()
  "Calling `ps/appear-setup' twice installs the advice once."
  (unwind-protect
      (progn
        (ps/appear-setup)
        (ps/appear-setup)
        (let ((count 0))
          (advice-mapc (lambda (fn _props)
                         (when (eq fn #'ps/appear--reassert-reveal)
                           (cl-incf count)))
                       'font-lock-default-fontify-region)
          (should (= count 1))))
    (advice-remove 'font-lock-default-fontify-region
                   #'ps/appear--reassert-reveal)))

(provide 'test-ps-appear)
;;; test-ps-appear.el ends here
