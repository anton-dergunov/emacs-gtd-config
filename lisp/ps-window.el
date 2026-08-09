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

(defvar org-agenda-multi)

(defvar ps/window-inhibit-split nil
  "When non-nil, `ps/window--split-if-alone' does nothing.
Bound around an agenda *rebuild* (`org-agenda-redo'), which re-runs the
command that built the view while that view is already on screen.  There is
then nothing to preserve alongside it -- splitting would just add a second
window showing the same thing on every refresh.")

(defun ps/window--side-window-p (window)
  "Non-nil when WINDOW is a side window (file tree, Claude Code, etc.)."
  (window-parameter window 'window-side))

(defun ps/window--content-windows ()
  "Windows in the selected frame that aren't side windows."
  (seq-remove #'ps/window--side-window-p (window-list)))

(defun ps/window--select-main ()
  "Select a content window when the selected one is a side window.

A side window is dedicated with the value `side', not t, and `switch-to-buffer'
only refuses a window dedicated with t -- so opening a view from the file tree
would silently replace the tree inside its own narrow slot, which reads as the
command having done nothing at all.  Returns the selected window; leaves it
alone when the frame has nothing but side windows, since there is then no
content window to move to."
  (when (ps/window--side-window-p (selected-window))
    (when-let ((main (car (ps/window--content-windows))))
      (select-window main)))
  (selected-window))

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

(defun ps/window--building-agenda-block-p ()
  "Non-nil while a block agenda is being assembled.
`org-agenda-run-series' binds `org-agenda-multi' around the whole series and
runs each block through the ordinary command (`org-agenda-list',
`org-todo-list', ...).  Those commands are advised to split when alone, so
without this check every `todo'/`alltodo' block in a block agenda would try
to split again -- which is what turned each refresh of a single-window
Agenda into a second Agenda window."
  (bound-and-true-p org-agenda-multi))

(defun ps/window--split-if-alone ()
  "Split the selected window when it's the only content window, then select
the new pane.  Mirrors the single-window convenience already familiar from
the agenda views: the buffer that was already visible stays visible
alongside whatever gets shown next.  No-op when the current buffer isn't
actually visible anywhere -- see `ps/window--current-buffer-visible-p' -- and
while a view is being rebuilt rather than opened (`ps/window-inhibit-split',
`ps/window--building-agenda-block-p')."
  (when (and (not ps/window-inhibit-split)
             (not (ps/window--building-agenda-block-p))
             (ps/window--current-buffer-visible-p)
             (ps/window--alone-p))
    (when-let ((new (split-window-sensibly)))
      (select-window new))))

;;;###autoload
(defun ps/window-show-here (buffer-or-name)
  "Display BUFFER-OR-NAME in the selected window, preserving the rest of the
window layout.  A side window is never taken over -- see
`ps/window--select-main'.  See `ps/window--split-if-alone' for the
single-window case."
  (ps/window--select-main)
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

;;;###autoload
(defun ps/window--inhibit-split-advice (orig-fn &rest args)
  "Around-advice for `org-agenda-redo': rebuild in place, never split.
A redo re-runs the command that built the view (`org-agenda-list',
`org-todo-list', a custom series, ...) with that view already displayed, so
the split-when-alone convenience would open a redundant second window on
every refresh."
  (let ((ps/window-inhibit-split t))
    (apply orig-fn args)))

(provide 'ps-window)
;;; ps-window.el ends here
