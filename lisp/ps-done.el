;;; ps-done.el --- Visual fading and folding of DONE org tasks -*- lexical-binding: t; -*-

(require 'org)

;; Bound dynamically by org during `org-after-todo-state-change-hook'.
(defvar org-state)

;;; Customization

(defcustom ps/done-fade-color "gray75"
  "Color used to fade DONE tasks and their timestamps."
  :type 'string)

;;; Folding helpers

(defun ps/done--fold-subtree-keep-newlines ()
  "Fold the current subtree but explicitly preserve trailing empty lines."
  (let ((beg (line-end-position))
        (end (save-excursion
               (org-end-of-subtree t t)
               ;; Move backward over all blank lines and spaces
               (skip-chars-backward " \t\n")
               ;; Stop at the end of the last line of actual text
               (line-end-position))))
    (when (< beg end)
      (org-fold-region beg end t 'outline))))

(defun ps/done-collapse-subtrees ()
  "Collapse all DONE subtrees in the current buffer."
  (interactive)
  (org-map-entries
   #'ps/done--fold-subtree-keep-newlines
   "TODO=\"DONE\"" 'file))

(defun ps/done-expand ()
  "Expand all DONE tasks."
  (interactive)
  (org-map-entries
   (lambda () (org-fold-show-subtree))
   "/DONE" 'file))

(defun ps/done-collapse ()
  "Collapse all DONE tasks."
  (interactive)
  (org-map-entries
   #'ps/done--fold-subtree-keep-newlines
   "/DONE" 'file))

;;; Fade overlays

(defun ps/done--clear-fade-overlays ()
  "Remove all DONE fade overlays."
  (remove-overlays (point-min)
                   (point-max)
                   'ps-done-fade t))

(defun ps/done-fade-subtrees (&rest _)
  "Fade DONE subtree contents and strip org-modern timestamp pills."
  (interactive)

  (when (derived-mode-p 'org-mode)

    ;; Prevent overlay accumulation
    (ps/done--clear-fade-overlays)

    (save-excursion
      (save-restriction
        (widen)

        (goto-char (point-min))

        (while (re-search-forward org-heading-regexp nil t)

          (when (string= (org-get-todo-state) "DONE")

            ;; Much safer/faster than org-element-at-point
            (let* ((begin
                    (save-excursion
                      (forward-line)
                      (point)))

                   (end
                    (save-excursion
                      (org-end-of-subtree t t))))

              (when (< begin end)

                ;; Main fade overlay
                (let ((ov (make-overlay begin end)))
                  (overlay-put ov
                               'face
                               `(:foreground ,ps/done-fade-color))
                  (overlay-put ov 'priority 10)
                  (overlay-put ov 'ps-done-fade t))

                ;; Remove org-modern timestamp pills
                (save-excursion
                  (goto-char begin)

                  (while (re-search-forward
                          org-ts-regexp-both
                          end
                          t)

                    (let* ((ts-start (match-beginning 0))
                           (ts-end   (match-end 0))
                           (ts-text
                            (buffer-substring-no-properties
                             ts-start
                             ts-end))

                           (ts-ov
                            (make-overlay ts-start ts-end)))

                      (overlay-put ts-ov
                                   'display
                                   ts-text)

                      (overlay-put ts-ov
                                   'face
                                   `(:foreground ,ps/done-fade-color
                                                  :strike-through t))

                      (overlay-put ts-ov 'priority 20)
                      (overlay-put ts-ov 'ps-done-fade t))))))))))))

(defun ps/done--refresh-after-revert ()
  "Fully rebuild Org visuals after auto-revert."
  (when (derived-mode-p 'org-mode)
    ;; Remove custom overlays
    (ps/done--clear-fade-overlays)

    ;; Remove stale Org fold overlays
    (when (fboundp 'org-fold-remove-all-overlays)
      (org-fold-remove-all-overlays))

    ;; Re-fontify
    (font-lock-flush)
    (font-lock-ensure)

    ;; Rebuild folds
    (ps/done-collapse-subtrees)

    ;; Rebuild fades
    (ps/done-fade-subtrees)

    ;; Final redraw
    (redisplay)))

(defun ps/done--after-todo-change-refresh ()
  "Refresh visuals after TODO state changes."
  (ps/done-fade-subtrees)
  (when (string= org-state "DONE")
    (save-excursion
      (org-back-to-heading t)
      (ps/done--fold-subtree-keep-newlines))))

;;; Setup

(defun ps/done-setup-hooks ()
  "Register DONE-fading buffer-local hooks and do the initial render.
Intended to be added to `org-mode-hook'."
  ;; Clean overlays before save
  (add-hook 'before-save-hook #'ps/done--clear-fade-overlays nil t)
  ;; Rebuild after save
  (add-hook 'after-save-hook #'ps/done-fade-subtrees nil t)
  ;; Rebuild after cycling/folding
  (add-hook 'org-cycle-hook #'ps/done-fade-subtrees nil t)
  ;; Rebuild after TODO state changes
  (add-hook 'org-after-todo-state-change-hook
            #'ps/done--after-todo-change-refresh nil t)
  ;; Rebuild after auto-revert
  (add-hook 'after-revert-hook #'ps/done--refresh-after-revert nil t)
  ;; Initial render
  (ps/done-collapse-subtrees)
  (ps/done-fade-subtrees))

(provide 'ps-done)
;;; ps-done.el ends here
