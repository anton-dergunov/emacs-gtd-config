;;; test-ps-heading-stars.el --- ERT tests for ps-heading-stars -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(add-to-list 'load-path "lisp")
(require 'ps-heading-stars)

(defmacro ps/heading-stars-test--in-org (content &rest body)
  "Run BODY in a temp `org-mode' buffer with CONTENT, reveal enabled + fontified.
Emphasis markers are hidden (as in the live config) so a mis-parsed heading
star actually gets the `invisible' property our matcher must strip."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((org-mode-hook nil)
           (org-hide-emphasis-markers t)
           (org-fontify-emphasized-text t))
       (org-mode)
       (insert ,content)
       (ps/heading-stars-enable)
       (font-lock-flush)
       (font-lock-ensure)
       (goto-char (point-min))
       ,@body)))

(defun ps/heading-stars-test--any-star-invisible-p ()
  "Return non-nil if any `*' in the buffer has `invisible' = t."
  (save-excursion
    (goto-char (point-min))
    (catch 'hit
      (while (search-forward "*" nil t)
        (when (eq (get-text-property (match-beginning 0) 'invisible) t)
          (throw 'hit t)))
      nil)))

;;; -------------------------------------------------------
;;; Half-typed heading on its own line
;;; -------------------------------------------------------

(ert-deftest ps/heading-stars--three-stars-all-visible ()
  "\"***\" with no trailing space keeps all three literal stars visible."
  (ps/heading-stars-test--in-org "***"
    (should-not (ps/heading-stars-test--any-star-invisible-p))))

(ert-deftest ps/heading-stars--four-stars-all-visible ()
  "\"****\" with no trailing space keeps all four literal stars visible."
  (ps/heading-stars-test--in-org "****"
    (should-not (ps/heading-stars-test--any-star-invisible-p))))

;;; -------------------------------------------------------
;;; Stray "**" must not eat a neighbouring headline's star
;;; -------------------------------------------------------

(ert-deftest ps/heading-stars--stray-stars-spare-next-headline ()
  "A bare \"**\" line above a real headline leaves every star visible."
  (ps/heading-stars-test--in-org "**\n** Foo\n"
    (should-not (ps/heading-stars-test--any-star-invisible-p))))

;;; -------------------------------------------------------
;;; Real headlines are unaffected
;;; -------------------------------------------------------

(ert-deftest ps/heading-stars--real-headline-visible ()
  "A completed headline keeps its stars visible."
  (ps/heading-stars-test--in-org "*** Done heading\n"
    (should-not (ps/heading-stars-test--any-star-invisible-p))))

;;; -------------------------------------------------------
;;; Negative guard: do not over-reveal real inline bold
;;; -------------------------------------------------------

(ert-deftest ps/heading-stars--inline-bold-stays-hidden ()
  "A body line beginning with inline bold keeps its opening marker hidden.
The leading `*' of \"*word*\" is a genuine emphasis marker, not a heading star,
so the matcher must leave it `invisible'."
  (ps/heading-stars-test--in-org "*word* and more\n"
    ;; Sanity: emphasis fontification ran and hid the opening marker.
    (should (eq (get-text-property (point-min) 'invisible) t))))

;;; test-ps-heading-stars.el ends here
