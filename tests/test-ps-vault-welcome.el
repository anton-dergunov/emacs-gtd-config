;;; test-ps-vault-welcome.el --- ERT tests for ps-vault-welcome -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path "lisp")
(require 'ps-vault-welcome)

(defvar my-org-base-directory)

(defmacro ps/vault-welcome-test--with-registry (registry &rest body)
  "Render REGISTRY into a temp buffer, bind `text' to the result, run BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (ps/vault-welcome--render ,registry)
     (let ((text (buffer-string)))
       (ignore text)
       ,@body)))

;;; -------------------------------------------------------
;;; Intro text
;;; -------------------------------------------------------

(ert-deftest ps/vault-welcome-test-explains-what-a-vault-is ()
  "A first run says what a vault is -- the reader has no other source for it."
  (let ((text (ps/vault-welcome--intro (ps/vault--empty-registry))))
    (should (string-match-p "No vault is open" text))
    (should (string-match-p "folder of Org files" text))))

(ert-deftest ps/vault-welcome-test-names-missing-folders ()
  "A vault whose folder has gone is named, so the list does not look empty."
  (let* ((registry (ps/vault--registry-set-current
                    (ps/vault--empty-registry) "/nonexistent/ps-welcome/"))
         (text (ps/vault-welcome--intro registry)))
    (should (string-match-p "not there" text))
    (should (string-match-p "ps-welcome" text))))

(ert-deftest ps/vault-welcome-test-no-missing-note-when-all-are-present ()
  "A healthy vault list does not get the missing-folder warning."
  (let ((dir (file-name-as-directory (make-temp-file "ps-welcome-" t))))
    (unwind-protect
        (let ((registry (ps/vault--registry-set-current
                         (ps/vault--empty-registry) dir)))
          (should-not (string-match-p "not there"
                                      (ps/vault-welcome--intro registry))))
      (delete-directory dir t))))

(ert-deftest ps/vault-welcome-test-missing-lists-only-the-missing ()
  "Only the vaults that are actually gone are reported as gone."
  (let ((dir (file-name-as-directory (make-temp-file "ps-welcome-" t))))
    (unwind-protect
        (let ((registry (ps/vault--registry-add
                         (ps/vault--registry-add (ps/vault--empty-registry) dir)
                         "/nonexistent/ps-welcome/")))
          (should (equal (ps/vault-welcome--missing registry)
                         '("/nonexistent/ps-welcome/"))))
      (delete-directory dir t))))

;;; -------------------------------------------------------
;;; Rendering
;;; -------------------------------------------------------

(ert-deftest ps/vault-welcome-test-offers-both-ways-in ()
  "Both the create and open actions are on screen with their keys."
  (ps/vault-welcome-test--with-registry (ps/vault--empty-registry)
    (should (string-match-p "Create a new vault" text))
    (should (string-match-p "Open an existing folder" text))
    (should (string-match-p "(n)" text))
    (should (string-match-p "(o)" text))))

(ert-deftest ps/vault-welcome-test-says-how-to-switch-later ()
  "The screen points at the three ways to switch, so it teaches the feature."
  (ps/vault-welcome-test--with-registry (ps/vault--empty-registry)
    (should (string-match-p "file tree" text))
    (should (string-match-p "C-c p V" text))))

(ert-deftest ps/vault-welcome-test-lists-known-vaults ()
  "Vaults already in the list are offered as buttons to go back to."
  (let ((dir (file-name-as-directory (make-temp-file "ps-welcome-" t))))
    (unwind-protect
        (ps/vault-welcome-test--with-registry
            (ps/vault--registry-set-current (ps/vault--empty-registry) dir)
          (should (string-match-p "Vaults you have used" text))
          (should (string-match-p (regexp-quote dir) text)))
      (delete-directory dir t))))

(ert-deftest ps/vault-welcome-test-omits-the-list-when-empty ()
  "A genuine first run has no vault list to show."
  (ps/vault-welcome-test--with-registry (ps/vault--empty-registry)
    (should-not (string-match-p "Vaults you have used" text))))

(ert-deftest ps/vault-welcome-test-buttons-carry-their-own-path ()
  "Each row's target lives on the button, not at point.
`push-button' does not move point on a mouse click, so a screen that read
point would act on whichever row the keyboard last visited."
  (let ((dir (file-name-as-directory (make-temp-file "ps-welcome-" t))))
    (unwind-protect
        (with-temp-buffer
          (ps/vault-welcome--render
           (ps/vault--registry-set-current (ps/vault--empty-registry) dir))
          (goto-char (point-min))
          (let ((button (next-button (point-min))))
            (should button)
            (should (equal (button-get button 'ps-vault-path) dir))))
      (delete-directory dir t))))

(ert-deftest ps/vault-welcome-test-render-is-idempotent ()
  "Redrawing replaces the screen rather than appending a second copy."
  (with-temp-buffer
    (ps/vault-welcome--render (ps/vault--empty-registry))
    (let ((once (buffer-string)))
      (ps/vault-welcome--render (ps/vault--empty-registry))
      (should (equal (buffer-string) once)))))

;;; -------------------------------------------------------
;;; Showing it
;;; -------------------------------------------------------

(ert-deftest ps/vault-welcome-test-shown-only-without-a-vault ()
  "The screen appears when startup found no vault, and not otherwise."
  (let ((ps/vault--needs-welcome nil))
    (ps/vault-welcome-maybe-show)
    (should-not (get-buffer ps/vault-welcome-buffer)))
  (let ((ps/vault--needs-welcome t)
        (home (file-name-as-directory (make-temp-file "ps-welcome-home-" t))))
    (unwind-protect
        (let ((user-emacs-directory home))
          (ps/vault-welcome-maybe-show)
          (should (get-buffer ps/vault-welcome-buffer)))
      (when (get-buffer ps/vault-welcome-buffer)
        (kill-buffer ps/vault-welcome-buffer))
      (delete-directory home t))))

(provide 'test-ps-vault-welcome)
;;; test-ps-vault-welcome.el ends here
