;;; ps-window.el --- Shared "take over the selected window" display rule -*- lexical-binding: t; -*-

;;; Commentary:
;; Every planning view/tool buffer in this config (Agenda, Calendar, Tasks,
;; Availability, Conflicts) follows the same display rule when opened: take
;; over the currently selected window, leaving every other window's buffer,
;; split orientation, and sizing untouched -- except when the selected window
;; is the only content window in the frame, in which case it's split first so
;; the buffer that was already visible stays visible alongside the new one.
;;
;; "Content window" excludes side windows (the file tree, the Claude Code IDE
;; panel) -- both are persistent docks via Emacs's own `window-side' window
;; parameter, and should never be swept into or out of this rule.

;;; Code:

(defun ps/window--side-window-p (window)
  "Non-nil when WINDOW is a side window (file tree, Claude Code, etc.)."
  (window-parameter window 'window-side))

(defun ps/window--content-windows ()
  "Windows in the selected frame that aren't side windows."
  (seq-remove #'ps/window--side-window-p (window-list)))

(defun ps/window--alone-p ()
  "Non-nil when the selected window is the only content window in the frame."
  (<= (length (ps/window--content-windows)) 1))

(defun ps/window--current-buffer-visible-p ()
  "Non-nil when the current buffer is displayed in a window somewhere.
Splitting/selecting a window on its behalf is only safe when this holds --
otherwise we're running inside a background process (e.g. a timer-driven
`org-agenda-redo') that pinned `current-buffer' via `with-current-buffer' on
a windowless buffer, and `select-window' would silently override that
binding, redirecting whatever gets inserted next into the wrong buffer."
  (get-buffer-window (current-buffer) t))

(defun ps/window--split-if-alone ()
  "Split the selected window when it's the only content window, then select
the new pane.  Mirrors the single-window convenience already familiar from
the agenda views: the buffer that was already visible stays visible
alongside whatever gets shown next.  No-op when the current buffer isn't
actually visible anywhere -- see `ps/window--current-buffer-visible-p'."
  (when (and (ps/window--current-buffer-visible-p) (ps/window--alone-p))
    (when-let ((new (split-window-sensibly)))
      (select-window new))))

;;;###autoload
(defun ps/window-show-here (buffer-or-name)
  "Display BUFFER-OR-NAME in the selected window, preserving the rest of the
window layout.  See `ps/window--split-if-alone' for the single-window case."
  (ps/window--split-if-alone)
  (switch-to-buffer (get-buffer buffer-or-name)))

;;;###autoload
(defun ps/window--split-if-alone-advice (orig-fn &rest args)
  "Around-advice for `org-agenda'/`org-todo-list': split first when alone,
mirroring `ps/window-show-here'.  `org-agenda-window-setup' (set to
`current-window' in config.org) then takes over the (possibly new) selected
window without touching any other one."
  (ps/window--split-if-alone)
  (apply orig-fn args))

(provide 'ps-window)
;;; ps-window.el ends here
