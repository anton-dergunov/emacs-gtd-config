(require 'ert)
(require 'org)

(add-to-list 'load-path "lisp")

(require 'org-links)

(ert-deftest my/obsidian-links-all-composed ()
  (with-temp-buffer
    (org-mode)

    (insert
     "[[obsidian:One]]\n[[obsidian:Two]]\n[[obsidian:Three]]")

    (my/org-enable-obsidian-links)

    (font-lock-ensure)

    (goto-char (point-min))

    (let ((count 0))
      (while (re-search-forward "obsidian:" nil t)
        (when (get-text-property
               (match-beginning 0)
               'composition)
          (setq count (1+ count))))

      (should (= count 3)))))
