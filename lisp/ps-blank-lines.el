;;; ps-blank-lines.el --- Reinsert blank lines before org level-1 headers -*- lexical-binding: t; -*-

;; Declared by org; referenced by the interactive driver below.
(defvar org-agenda-files)

(defun ps/blank-lines--reinsert-in-buffer ()
  "Insert a blank line before each level-1 header in the current buffer that
is not already preceded by a blank line.  Return the number of insertions."
  (goto-char (point-min))
  (let ((changes 0))
    (while (re-search-forward "^\\* " nil t)
      (unless (save-excursion
                (forward-line -1)
                (looking-at-p "^\\s-*$"))  ; previous line already blank?
        (save-excursion
          ;; Move two chars back to sit before the "* " header, then insert.
          (goto-char (- (point) 2))
          (insert "\n"))
        (setq changes (1+ changes))))
    changes))

(defun ps/blank-lines-reinsert ()
  "Reinsert empty lines before first-level headers in all `org-agenda-files'.
Reports the per-file and total change counts in a *Org File Changes* buffer."
  (interactive)
  (let ((modified-files '())
        (total-changes 0))
    (dolist (file org-agenda-files)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((changes (ps/blank-lines--reinsert-in-buffer)))
          (when (> changes 0)
            (write-region (point-min) (point-max) file)
            (push (cons (file-name-nondirectory file) changes) modified-files)
            (setq total-changes (+ total-changes changes))))))

    ;; Show results in a new buffer
    (let ((output-buffer (get-buffer-create "*Org File Changes*")))
      (with-current-buffer output-buffer
        (erase-buffer)
        (insert (format "Modified files and line changes:\n\n"))
        (dolist (file-change modified-files)
          (insert (format "File: %s, Lines changed: %d\n"
                          (car file-change) (cdr file-change))))
        (insert (format "\nTotal lines changed: %d" total-changes)))
      (display-buffer output-buffer))))

(provide 'ps-blank-lines)
;;; ps-blank-lines.el ends here
