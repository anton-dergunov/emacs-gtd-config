;;; test-ps-links.el --- ERT tests for ps-links -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(add-to-list 'load-path "lisp")
(require 'ps-links)

(ert-deftest ps/obsidian-links-all-composed ()
  "All obsidian: prefixes in a buffer receive font-lock composition."
  (with-temp-buffer
    (org-mode)
    (insert "[[obsidian:One]]\n[[obsidian:Two]]\n[[obsidian:Three]]")
    (ps/org-enable-obsidian-links)
    (font-lock-ensure)
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward "obsidian:" nil t)
        (when (get-text-property (match-beginning 0) 'composition)
          (setq count (1+ count))))
      (should (= count 3)))))

;;; -------------------------------------------------------
;;; Async URL link insertion
;;; -------------------------------------------------------

(ert-deftest ps/links--async-api-defined ()
  "The async URL command and its helpers are defined; the command is interactive."
  (should (fboundp 'ps/org-insert-link-async))
  (should (commandp 'ps/org-insert-link-async))
  (should (fboundp 'ps/org--system-clipboard-url))
  (should (fboundp 'ps/org--replace-marked-region-with)))

(ert-deftest ps/links--replace-marked-region-basic ()
  "Replacing a marked region swaps the placeholder text for the final text."
  (with-temp-buffer
    (insert "before PLACEHOLDER after")
    (goto-char (point-min))
    (search-forward "PLACEHOLDER")
    (let ((start (set-marker (make-marker) (match-beginning 0)))
          (end   (set-marker (make-marker) (match-end 0))))
      (ps/org--replace-marked-region-with start end "[[url][Title]]")
      (should (equal (buffer-string) "before [[url][Title]] after")))))

(ert-deftest ps/links--replace-marked-region-nullifies-markers ()
  "After replacement the markers are cleared, preventing a double callback."
  (with-temp-buffer
    (insert "x PLACEHOLDER y")
    (goto-char (point-min))
    (search-forward "PLACEHOLDER")
    (let ((start (set-marker (make-marker) (match-beginning 0)))
          (end   (set-marker (make-marker) (match-end 0))))
      (ps/org--replace-marked-region-with start end "Z")
      (should (null (marker-position start)))
      (should (null (marker-position end)))
      ;; A second call is a no-op now that the markers are nil.
      (ps/org--replace-marked-region-with start end "SHOULD-NOT-APPEAR")
      (should-not (string-match-p "SHOULD-NOT-APPEAR" (buffer-string))))))

(ert-deftest ps/links--replace-marked-region-noop-on-unset ()
  "With unset markers, replacement does nothing and leaves the buffer intact."
  (with-temp-buffer
    (insert "untouched")
    (let ((start (make-marker))
          (end   (make-marker)))
      (ps/org--replace-marked-region-with start end "X")
      (should (equal (buffer-string) "untouched")))))
