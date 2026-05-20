;;; org-links.el --- Org link helpers -*- lexical-binding: t; -*-

(require 'subr-x)

(defcustom my/obsidian-link-icon "✎ "
  "Display icon for obsidian links."
  :type 'string)

(defun my/org-compose-obsidian-prefix (limit)
  "Compose all visible obsidian: prefixes up to LIMIT."
  (while (re-search-forward "\\(\\bobsidian:\\)" limit t)
    (let ((start (match-beginning 1))
          (end (match-end 1)))
      (remove-text-properties start end '(composition nil))
      (compose-region start end my/obsidian-link-icon)))
  nil)

(defun my/org-enable-obsidian-links ()
  "Enable obsidian link composition."
  (font-lock-add-keywords
   nil
   '((my/org-compose-obsidian-prefix 0 t))))

(provide 'org-links)
