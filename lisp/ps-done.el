;;; ps-done.el --- Visual fading and folding of DONE org tasks -*- lexical-binding: t; -*-

(require 'org)

;; Bound dynamically by org during `org-after-todo-state-change-hook'.
(defvar org-state)

;;; Customization

(defcustom ps/done-fade-color "gray75"
  "Color used to fade DONE tasks and their timestamps.
May be a color name/hex string, or the symbol `auto' to derive a
theme-appropriate dim color from the `shadow' face (so DONE fading
looks right on both light and dark themes)."
  :type '(choice (string :tag "Color")
                 (const :tag "Derive from theme (shadow face)" auto)))

(defun ps/done--fade-color ()
  "Resolve `ps/done-fade-color' to a concrete color string.
When it is the symbol `auto', use the `shadow' face foreground of the
active theme, falling back to \"gray50\" if that is unavailable."
  (if (eq ps/done-fade-color 'auto)
      (or (face-foreground 'shadow nil t) "gray50")
    ps/done-fade-color))

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

    (let ((fade-color (ps/done--fade-color)))
    (save-excursion
      (save-restriction
        (widen)

        (goto-char (point-min))

        ;; `org-outline-regexp-bol' ("^\\*+ "), *not* `org-heading-regexp'.
        ;; The latter makes the title optional ("^\\(\\*+\\)\\(?: +\\(.*?\\)\\)?
        ;; [ \t]*$"), so a line holding a lone "*" -- exactly what a
        ;; half-typed new heading looks like -- matches it while Org itself
        ;; does not consider it a headline.  `org-get-todo-state' then walks
        ;; back to the enclosing headline and, in a file that has none yet,
        ;; signals "Before first headline"; from the debounced idle timer
        ;; below that surfaced as an error mid-typing.
        (while (re-search-forward org-outline-regexp-bol nil t)

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
                               `(:foreground ,fade-color))
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
                                   `(:foreground ,fade-color
                                                  :strike-through t))

                      (overlay-put ts-ov 'priority 20)
                      (overlay-put ts-ov 'ps-done-fade t)))))))))))))

;;; Debounced re-fade on edit

(defvar ps/done-refade-idle-delay 0.2
  "Idle seconds before DONE fade overlays are rebuilt after an edit.
Fade overlays are static, so text typed right after a DONE task (e.g. a new
sibling heading) would otherwise inherit the stale overlay's fade until the
next save.  The rebuild is debounced onto idle time so it does not run on
every keystroke; it fires once typing pauses for this long.")

(defvar-local ps/done--refade-timer nil
  "Pending idle timer that will rebuild this buffer's fade overlays, or nil.")

(defun ps/done--refade-now (buffer)
  "Rebuild BUFFER's DONE fade overlays.  The debounced worker.
Errors are demoted to a message: this fires from an idle timer while the
user is typing, so the buffer can be in any half-finished state, and a
purely cosmetic overlay rebuild must never interrupt editing with an error."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ps/done--refade-timer nil)
      (with-demoted-errors "ps/done refade: %S"
        (ps/done-fade-subtrees)))))

(defun ps/done--schedule-refade (&rest _)
  "Debounce a fade-overlay rebuild onto idle time.
Added to `after-change-functions' (whose BEG/END/LEN args are ignored), so it
runs on actual edits rather than on cursor movement.  Arming only when no timer
is pending keeps the per-edit cost negligible.  Rebuilding overlays does not
modify buffer text, so it cannot re-trigger `after-change-functions'."
  (unless ps/done--refade-timer
    (setq ps/done--refade-timer
          (run-with-idle-timer ps/done-refade-idle-delay nil
                               #'ps/done--refade-now (current-buffer)))))

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
  ;; Rebuild (debounced) after ordinary edits, so text typed right after a DONE
  ;; task is not left dimmed by a stale overlay until the next save.
  (add-hook 'after-change-functions #'ps/done--schedule-refade nil t)
  ;; Initial render
  (ps/done-collapse-subtrees)
  (ps/done-fade-subtrees))

(provide 'ps-done)
;;; ps-done.el ends here
