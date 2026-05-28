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
