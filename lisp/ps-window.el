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

(declare-function ps/nav-note-departure "ps-nav")

(defun ps/window--note-departure ()
  "Tell `ps-nav' we are leaving, if it is loaded.
A soft dependency in this direction, so `ps-window' stays the leaf it is: the
history is a convenience layered on top of these helpers, never a requirement
for them."
  (when (fboundp 'ps/nav-note-departure)
    (ps/nav-note-departure)))

;;;###autoload
(defun ps/window-replace-here (buffer-or-name)
  "Display BUFFER-OR-NAME in the selected window, never splitting.
Like `ps/window-show-here' but without the split-when-alone convenience,
which is right for opening a view alongside what you were reading and wrong
for stepping *through* something: the Info Triage queue, an item's index, its
directory, a file inside it are one trail, and a trail that adds a window at
every step is not one.  A side window is still never taken over -- see
`ps/window--select-main'."
  (ps/window--select-main)
  (ps/window--note-departure)
  (switch-to-buffer (get-buffer buffer-or-name)))

;;;###autoload
(defun ps/window-visit-here (file)
  "Visit FILE in the selected window, splitting only when it is the only one.

The file-visiting counterpart of `ps/window-show-here', and it follows the same
rule for the same reason.  Reviewing the Info Triage queue alone, the item
should appear *beside* the queue rather than replace it; with Claude Code
already in the other window there is no room for a third, and the item should
take over the window it was launched from.  \"Split when alone\" is exactly that
distinction, and it needs no setting to express it.

Use `ps/window-visit-only-here' where a split would be wrong whatever the
layout -- retracing a trail, for one."
  (ps/window--select-main)
  ;; Split BEFORE noting the departure, not after.  The split selects a new
  ;; window still showing the old buffer, so noting afterwards records the
  ;; departure in the window that is about to leave it -- back then returns the
  ;; item window to the queue, and the queue's own window is left alone.  Noting
  ;; first put a step into the history of a window that never went anywhere.
  (ps/window--split-if-alone)
  (ps/window--note-departure)
  (find-file file))

;;;###autoload
(defun ps/window-visit-only-here (file)
  "Visit FILE in the selected window, never splitting.
For callers that must not change the window layout at all: `ps-nav' going back
along a trail is one, since a step backwards that adds a window is not a step
backwards."
  (ps/window--select-main)
  (find-file file))

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
