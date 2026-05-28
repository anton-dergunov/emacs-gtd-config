;;; ps-links.el --- Org link display helpers -*- lexical-binding: t; -*-

(require 'subr-x)

(defcustom ps/obsidian-link-icon "✎ "
  "Display icon for obsidian: link prefixes."
  :type 'string)

(defun ps/org-compose-obsidian-prefix (limit)
  "Compose all visible obsidian: prefixes up to LIMIT into the display icon."
  (while (re-search-forward "\\(\\bobsidian:\\)" limit t)
    (let ((start (match-beginning 1))
          (end   (match-end 1)))
      (remove-text-properties start end '(composition nil))
      (compose-region start end ps/obsidian-link-icon)))
  nil)

(defun ps/org-enable-obsidian-links ()
  "Register the obsidian: link composition rule in the current buffer."
  (font-lock-add-keywords
   nil
   '((ps/org-compose-obsidian-prefix 0 t))))

(provide 'ps-links)
;;; ps-links.el ends here
