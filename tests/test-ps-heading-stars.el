;;; test-ps-heading-stars.el --- ERT tests for ps-heading-stars -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(add-to-list 'load-path "lisp")
(require 'ps-heading-stars)

(defmacro ps/heading-stars-test--in-org (content &rest body)
  "Run BODY in a temp `org-mode' buffer with CONTENT, the guard active + fontified.
Emphasis markers are hidden (as in the live config) so a mis-parsed heading
star would actually get bold/`invisible'.  The global advice is removed again
afterwards so it cannot leak into other test files."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((org-mode-hook nil)
           (org-hide-emphasis-markers t)
           (org-fontify-emphasized-text t))
       (org-mode)
       (insert ,content)
       (unwind-protect
           (progn
             (ps/heading-stars-enable)
             (font-lock-flush)
             (font-lock-ensure)
             (goto-char (point-min))
             ,@body)
         (ps/heading-stars-disable)))))

(defun ps/heading-stars-test--star-bold-p (pos)
  "Return non-nil if the `face' at POS includes the `bold' face."
  (memq 'bold (ensure-list (get-text-property pos 'face))))

(defun ps/heading-stars-test--bad-star ()
  "Return the position of the first star that is bold, invisible, or emphasized.
Nil means every `*' in the buffer renders as a plain heading star."
  (save-excursion
    (goto-char (point-min))
    (catch 'hit
      (while (search-forward "*" nil t)
        (let ((pos (match-beginning 0)))
          (when (or (eq (get-text-property pos 'invisible) t)
                    (get-text-property pos 'org-emphasis)
                    (ps/heading-stars-test--star-bold-p pos))
            (throw 'hit pos))))
      nil)))

;;; -------------------------------------------------------
;;; A pure run of leading stars is never emphasized
;;; -------------------------------------------------------

(ert-deftest ps/heading-stars--three-stars-plain ()
  "\"***\" stays plain (no bold, no hidden, no emphasis)."
  (ps/heading-stars-test--in-org "***"
    (should-not (ps/heading-stars-test--bad-star))))

(ert-deftest ps/heading-stars--four-stars-plain ()
  "\"****\" stays plain."
  (ps/heading-stars-test--in-org "****"
    (should-not (ps/heading-stars-test--bad-star))))

(ert-deftest ps/heading-stars--many-stars-not-bold ()
  "A long pure star run has no bold middle stars."
  (ps/heading-stars-test--in-org "**********\n"
    (should-not (ps/heading-stars-test--bad-star))))

;;; -------------------------------------------------------
;;; A stray "**" must not bleed onto the next headline
;;; -------------------------------------------------------

(ert-deftest ps/heading-stars--stray-stars-spare-next-headline ()
  "A bare \"**\" line above a headline leaves the headline's stars pristine."
  (ps/heading-stars-test--in-org "**\n** TODO foo\n"
    (should-not (ps/heading-stars-test--bad-star))))

;;; -------------------------------------------------------
;;; Real headlines are unaffected
;;; -------------------------------------------------------

(ert-deftest ps/heading-stars--real-headline-plain ()
  "A completed headline's stars are not bold/hidden/emphasized."
  (ps/heading-stars-test--in-org "*** Foo\n"
    (should-not (ps/heading-stars-test--bad-star))))

;;; -------------------------------------------------------
;;; Negative guard: do not disturb real inline bold
;;; -------------------------------------------------------

(ert-deftest ps/heading-stars--inline-bold-untouched ()
  "A body line beginning with inline bold keeps its hidden, emphasized marker.
The leading `*' of \"*word*\" is a genuine emphasis marker, not a heading star."
  (ps/heading-stars-test--in-org "*bold* and more\n"
    (should (eq (get-text-property (point-min) 'invisible) t))
    (should (get-text-property (point-min) 'org-emphasis))))

;;; test-ps-heading-stars.el ends here
