;;; ps-claude.el --- Claude Code IDE window/resize tweaks -*- lexical-binding: t; -*-

;;; Commentary:
;; Small fixes for `claude-code-ide.el' (eat backend):
;;
;; 1. The side window opens too wide by default (100 columns); we offer a
;;    tunable `ps/claude-window-width' instead.
;;
;; 2. Resizing the Claude Code window can leave new output rendering into
;;    only part of the window (e.g. the top half) until something forces a
;;    redraw.  `claude-code-ide--terminal-reflow-filter' (the workaround for
;;    upstream claude-code#1422) suppresses `eat--adjust-process-window-size'
;;    for height-only changes, but that function has *already* resized eat's
;;    internal terminal by the time it runs -- the underlying `claude'
;;    process is never told its new dimensions via
;;    `set-process-window-size'/SIGWINCH, so it keeps rendering for the old,
;;    smaller size while eat's buffer/window is already the new, larger one.
;;    We add a `window-size-change-functions' hook that re-syncs the process
;;    size once the resize settles, using eat's own `eat-term-size' (the
;;    terminal's already-resized internal model) rather than recomputing the
;;    size independently via `window-body-height'/`window-body-width' --
;;    that second computation could drift from eat's own by a row or column
;;    and never converge, which is why a stuck window sometimes needed
;;    several manual resizes before it "caught".  We also force an
;;    immediate `eat-term-redisplay' after the resync, so a pane that is
;;    already stale-blank repaints right away instead of waiting for
;;    Claude's next output chunk or another manual resize.  The resync timer
;;    is a plain timer, not an idle timer, so it fires on a fixed delay
;;    regardless of Emacs's idle bookkeeping.  Emacs also runs that
;;    machinery once per mouse-motion event during a divider drag, and each
;;    reflow rewrites eat's whole display region and makes Claude repaint --
;;    the visible flicker -- so reflows are rate-limited to
;;    `ps/claude-resize-throttle-interval' while a drag is in flight.  Set
;;    `ps/claude-debug-resize' to log each resize/resync event if this ever
;;    needs diagnosing again.
;;
;; 3. `claude-code-ide--get-working-directory' defaults to the current
;;    project root, which for any buffer in this repo is this config's own
;;    source tree. This integration exists to assist with org-mode planning
;;    content, not this config's source code, so we override it to always
;;    use `my-org-base-directory' instead.
;;
;; 4. Selection / active-buffer tracking is keyed by project: the running
;;    session is stored under the working directory (pinned to
;;    `my-org-base-directory' by #3), but
;;    `claude-code-ide-mcp--get-buffer-project' looks the session up by
;;    `project-current', which for an Org file in this repo resolves to the
;;    git repo root -- a different key.  The lookup misses, so the editor
;;    selection never reaches Claude.  We advise that lookup to return
;;    `my-org-base-directory' for buffers under it, matching the session key.
;;
;; 5. When Claude writes a file it visits it again (via `find-file-noselect'
;;    in the ediff/open path).  If `global-auto-revert' has not yet reverted
;;    the unmodified buffer, `find-file-noselect' pops a "Reread from disk?"
;;    prompt.  We advise the open/diff entry points to quietly revert a
;;    *stale, unmodified* visiting buffer first, closing the race.  Modified
;;    buffers are deliberately left alone, so a genuine edit conflict still
;;    prompts ("Discard your edits?").
;;
;; 6. When the diff window first opens, eat's terminal width state can briefly
;;    desync and `eat--process-output-queue' signals `args-out-of-range' from
;;    inside its timer.  We wrap it to swallow that transient error and
;;    schedule a resync so the terminal re-renders cleanly.
;;
;; 7. `claude-code-ide-window-side' is a fixed `right' by default, which is
;;    impractical when the Emacs frame itself is taller than it is wide (e.g.
;;    positioned on the narrow edge of an ultrawide monitor).  We advise the
;;    side window's display function to dock `right' when the frame is wider
;;    than tall, `bottom' otherwise -- recomputed fresh on every open, so it
;;    tracks whatever shape the frame currently has.  Deliberately stays a
;;    side window (not unified with the take-over-the-selected-window rule
;;    used by the Agenda/Calendar/Tasks/Availability/Conflicts views, see
;;    `lisp/ps-window.el'): the point of this panel is to stay in a stable,
;;    predictable spot while you work elsewhere, the same way the file tree
;;    does.
;;
;; 8. eat never calls `set-window-start' anywhere -- it positions windows
;;    only via `set-window-point' plus an unclamped `recenter'.  Two
;;    consequences after a resize: `eat--t-resize' refuses to move the
;;    display region's start backwards when the window grows, so `recenter'
;;    is handed a more negative argument than there are lines and drags the
;;    window up into stale scrollback (content missing at the top, dead
;;    space at the bottom); and `eat--synchronize-scroll-windows' only
;;    re-anchors windows whose point sits exactly on the terminal cursor, so
;;    a window the user scrolled up in is abandoned while the reflow
;;    rewrites and trims the text under it (a blank window).  We supply the
;;    missing explicit anchoring: windows at the bottom are pinned to the
;;    display beginning, scrolled-up windows keep their position and are
;;    only corrected when it is no longer valid.
;;
;; 9. A running session keeps two processes alive whose query-on-exit flags
;;    would make `save-buffers-kill-emacs' prompt: eat's terminal and the
;;    MCP HTTP listener (`ws-start' omits `:noquery').  Both are cleared, so
;;    Emacs quits without asking (see `ps/claude-no-exit-prompt').
;;
;; Not handled here: glyphs taller than the default font (Claude's spinner,
;; bullets, emoji) used to change the line height and make text below them
;; dance.  That is fixed globally by `face-font-rescale-alist' in config.org's
;; Font section, not per-buffer -- `line-height' cannot cap a line, only
;; raise its floor.
;;
;; If upstream #1422 is fixed and the reflow workaround is no longer needed,
;; the reflow-glitch suppression itself can be disabled with:
;;   (setq claude-code-ide-prevent-reflow-glitch nil)
;; The resync hook below is harmless either way and can stay.

;;; Code:

(require 'seq)

(declare-function claude-code-ide--sync-terminal-dimensions "claude-code-ide")
(declare-function claude-code-ide--get-working-directory "claude-code-ide")
(declare-function claude-code-ide-mcp--get-buffer-project "claude-code-ide-mcp")
(declare-function claude-code-ide-mcp--create-diff-buffers "claude-code-ide-mcp-handlers")
(declare-function claude-code-ide-mcp-handle-open-file "claude-code-ide-mcp-handlers")
(declare-function eat--process-output-queue "eat")
(declare-function eat-term-size "eat")
(declare-function eat-term-redisplay "eat")
(declare-function eat-term-display-beginning "eat")
(declare-function eat-term-display-cursor "eat")
(declare-function eat--adjust-process-window-size "eat")
(declare-function ps/freeze-log--timestamp "ps-freeze-log")
(declare-function ps/freeze-log--format-line "ps-freeze-log")
(declare-function claude-code-ide-mcp-http-server-start "claude-code-ide-mcp-http-server")
(declare-function ws-process "web-server")
(defvar eat-query-before-killing-running-terminal)
(declare-function claude-code-ide--display-buffer-in-side-window "claude-code-ide")
(defvar claude-code-ide-window-width)
(defvar claude-code-ide-window-side)
(defvar my-org-base-directory)
(defvar eat-terminal)

(defcustom ps/claude-window-width 90
  "Width (in columns) of the Claude Code IDE side window.
Applied to `claude-code-ide-window-width' by `ps/claude-setup'."
  :type 'integer
  :group 'claude-code-ide)

(defcustom ps/claude-resize-debounce-delay 0.3
  "Delay, in seconds, before re-syncing Claude Code terminal dimensions
after a window resize settles.  A plain timer, not an idle timer: the
resync must fire on this fixed delay regardless of Emacs's idle state."
  :type 'number
  :group 'claude-code-ide)

(defcustom ps/claude-reanchor-delay 0.2
  "Seconds after a resize before the follow-up window re-anchor pass.
SIGWINCH is asynchronous: Claude's Ink renderer repaints tens to hundreds
of milliseconds after the terminal is resized, so a single re-anchor at
resize time can land before the new content exists.  This second pass
corrects the window once it does."
  :type 'number
  :group 'claude-code-ide)

(defcustom ps/claude-resize-throttle-interval 0.25
  "Minimum seconds between terminal reflows while a window drag is in flight.
Emacs runs the process-window-size machinery once per mouse-motion event
during a divider drag, and each reflow rewrites eat's whole display region
and makes Claude repaint -- which is what the flicker is.  Throttling to
this interval keeps the terminal tracking the new width without paying for
every pixel.  Only applies while dragging; a settled resize always reflows."
  :type 'number
  :group 'claude-code-ide)

(defcustom ps/claude-debug-resize nil
  "When non-nil, append Claude Code resize/resync diagnostics to a log file.
The file is `ps/claude-debug-resize-file'.  Records each size-change event,
throttled reflows, the dimensions sent to the `claude' process, and the
window re-anchor decisions.  Off by default (no cost when disabled); turn
on only to diagnose a stuck or partially-rendered Claude Code pane."
  :type 'boolean
  :group 'claude-code-ide)

(defcustom ps/claude-debug-resize-file
  (expand-file-name "tmp/ps-claude-resize.log" user-emacs-directory)
  "File `ps/claude-debug-resize' diagnostics are appended to.
Deliberately separate from `ps/freeze-log-file' so the freeze
investigation's log stays uncluttered, and kept on local disk (never the
Dropbox-backed org tree) so logging cannot block on that mount."
  :type 'file
  :group 'claude-code-ide)

(defvar ps/claude--resize-timer nil
  "Pending timer for the next terminal-size resync, or nil.")

(defvar ps/claude--reanchor-timer nil
  "Pending timer for the follow-up re-anchor pass, or nil.")

(defvar ps/claude--last-reflow-time nil
  "Time of the last non-throttled terminal reflow, or nil.")

(defun ps/claude--debug-log (format-string &rest args)
  "Append FORMAT-STRING/ARGS to the resize log when debugging is enabled.
Reuses `ps-freeze-log's pure formatting helpers when that module is loaded
so timestamps match across logs, and falls back to a plain stamp otherwise.
Never signals -- diagnostics must not become their own source of failures."
  (when ps/claude-debug-resize
    (ignore-errors
      (let* ((message (apply #'format format-string args))
             (line (if (and (fboundp 'ps/freeze-log--timestamp)
                            (fboundp 'ps/freeze-log--format-line))
                       (ps/freeze-log--format-line
                        (ps/freeze-log--timestamp) 'claude-resize message)
                     (format "%s [claude-resize] %s\n"
                             (format-time-string "%Y-%m-%d %H:%M:%S") message)))
             (write-region-inhibit-fsync nil)
             (coding-system-for-write 'utf-8-unix)
             (inhibit-message t)
             (message-log-max nil))
        (make-directory (file-name-directory ps/claude-debug-resize-file) t)
        (write-region line nil ps/claude-debug-resize-file 'append 'silent)))))

(defun ps/claude--session-buffer-p (buffer-or-name)
  "Return non-nil if BUFFER-OR-NAME is a Claude Code session buffer.
Pure string match on the `*claude-code[...]*' naming convention used by
`claude-code-ide.el', so it works without loading that package."
  (let ((name (if (bufferp buffer-or-name)
                   (buffer-name buffer-or-name)
                 buffer-or-name)))
    (and (stringp name) (string-prefix-p "*claude-code[" name))))

(defun ps/claude--terminal-live-p ()
  "Non-nil if the current buffer has a usable eat terminal."
  (and (boundp 'eat-terminal) eat-terminal (fboundp 'eat-term-size)))

(defun ps/claude--clamp-window-start (start display-begin)
  "Return a valid window-start given the current START and DISPLAY-BEGIN.
Pure/testable.  eat rewrites and trims buffer text on every resize but only
re-anchors windows whose point sits exactly on the terminal cursor, so a
window the user scrolled up in can be left anchored outside the text that
still exists -- which is what renders it blank.  Clamp into the valid range
instead of jumping to the bottom, preserving roughly where the user was
reading.  Returns nil when START is already valid and needs no correction."
  (cond
   ((not (integerp start)) display-begin)
   ((< start (point-min)) (point-min))
   ((> start display-begin) display-begin)
   (t nil)))

(defun ps/claude--reanchor-window (window)
  "Re-anchor WINDOW to eat's live display region.

eat never calls `set-window-start' anywhere -- it positions windows only
via `set-window-point' plus an unclamped `recenter' -- so after a resize a
window can end up anchored above the display region (stale content at the
top, dead space at the bottom) or, if the user had scrolled up, anchored
into text that the reflow deleted (a blank window).  This supplies the
missing explicit anchoring.

A window sitting on the terminal cursor (eat's own \"at the bottom\" test)
is pinned deterministically to the display beginning.  A window the user
scrolled up in keeps its position and is only corrected when that position
is no longer valid."
  (when (window-live-p window)
    (let ((buffer (window-buffer window)))
      (when (ps/claude--session-buffer-p buffer)
        (with-current-buffer buffer
          (when (ps/claude--terminal-live-p)
            (let* ((display-begin (eat-term-display-beginning eat-terminal))
                   (cursor (eat-term-display-cursor eat-terminal))
                   (start (window-start window))
                   (at-bottom (= (window-point window) cursor)))
              (if at-bottom
                  (progn
                    (ps/claude--debug-log
                     "%s: anchor at-bottom start=%s -> %s cursor=%s"
                     (buffer-name buffer) start display-begin cursor)
                    (set-window-start window display-begin)
                    (set-window-point window cursor))
                (let ((clamped (ps/claude--clamp-window-start
                                start display-begin)))
                  (ps/claude--debug-log
                   "%s: anchor scrolled-up start=%s display-begin=%s clamped=%s"
                   (buffer-name buffer) start display-begin clamped)
                  (when clamped
                    (set-window-start window clamped))
                  ;; Safety net: a scrolled-up window can survive the clamp
                  ;; with an in-range start that nonetheless shows nothing,
                  ;; because eat rewrote and trimmed the text under it (and
                  ;; claude-code-ide's own position keeper moves point too).
                  ;; A blank window is never a position worth preserving, so
                  ;; fall back to the live display region.
                  (when (ps/claude--window-blank-p window)
                    (ps/claude--debug-log
                     "%s: window blank after anchor, falling back to display-begin"
                     (buffer-name buffer))
                    (set-window-start window display-begin)
                    (set-window-point window cursor)))))))))))

(defun ps/claude--window-blank-p (window)
  "Non-nil if WINDOW's visible region holds no non-whitespace text.
Used as a last-resort check after re-anchoring: whatever the user's scroll
position was, showing an empty pane is never what they wanted."
  (when (window-live-p window)
    (let ((start (window-start window))
          (end (window-end window t)))
      (and (integerp start) (integerp end)
           (with-current-buffer (window-buffer window)
             (save-excursion
               (save-restriction
                 (widen)
                 (let ((from (max (point-min) start))
                       (to (min (point-max) end)))
                   (and (< from to)
                        (not (string-match-p
                              "[^ \t\n\r\f]"
                              (buffer-substring-no-properties from to))))))))))))

(defun ps/claude--resync-window (window)
  "Re-sync terminal dimensions for WINDOW's Claude Code session buffer.
Prefers eat's own `eat-term-size' -- the terminal's already-resized
internal model -- over recomputing independently via `window-body-height'/
`window-body-width', so the process is told exactly the size eat is
actually using rather than a value that could drift from it by a
row/column and never converge.  Falls back to
`claude-code-ide--sync-terminal-dimensions' for backends other than eat,
or before the terminal is initialized.  Re-anchoring is done separately by
`ps/claude--reanchor-window'."
  (when (window-live-p window)
    (let ((buffer (window-buffer window)))
      (when (ps/claude--session-buffer-p buffer)
        (with-current-buffer buffer
          (if (ps/claude--terminal-live-p)
              (let* ((size (eat-term-size eat-terminal))
                     (width (car size))
                     (height (cdr size))
                     (proc (get-buffer-process buffer)))
                (ps/claude--debug-log "%s: eat-term-size=%dx%d proc=%s"
                                       (buffer-name buffer) width height
                                       (and proc t))
                (when proc
                  (set-process-window-size proc height width)
                  (eat-term-redisplay eat-terminal)))
            (ps/claude--debug-log "%s: eat-terminal unset, falling back to window-body-height/width"
                                   (buffer-name buffer))
            (claude-code-ide--sync-terminal-dimensions buffer window)))))))

(defun ps/claude--claude-windows ()
  "Return every live window currently showing a Claude Code session buffer."
  (seq-filter (lambda (w) (ps/claude--session-buffer-p (window-buffer w)))
              (window-list)))

(defun ps/claude--reanchor-windows ()
  "Re-anchor every Claude Code window, then mark them for repaint.
Uses `force-window-update' rather than a forced `redisplay': on the macOS
NS build a redisplay forced from inside a timer can re-enter AppKit's event
loop and wedge the whole UI (see the freeze notes in config.org)."
  (setq ps/claude--reanchor-timer nil)
  (dolist (window (ps/claude--claude-windows))
    (ps/claude--reanchor-window window)
    (force-window-update (window-buffer window))))

(defun ps/claude--resync-windows ()
  "Re-sync dimensions and re-anchor all live Claude Code session windows.
Schedules a second re-anchor pass: SIGWINCH is asynchronous, so Claude's
own repaint lands tens to hundreds of milliseconds after the resize, and
the window must be re-anchored again once that content exists."
  (setq ps/claude--resize-timer nil)
  ;; The drag has settled: stop clipping before the authoritative reflow, so
  ;; the resize below is computed against the buffer's real wrapping mode.
  (dolist (window (ps/claude--claude-windows))
    (with-current-buffer (window-buffer window)
      (ps/claude--end-drag-clipping)))
  (dolist (window (ps/claude--claude-windows))
    (ps/claude--resync-window window))
  (ps/claude--reanchor-windows)
  (when (timerp ps/claude--reanchor-timer)
    (cancel-timer ps/claude--reanchor-timer))
  (setq ps/claude--reanchor-timer
        (run-with-timer ps/claude-reanchor-delay nil
                         #'ps/claude--reanchor-windows)))

(defun ps/claude--schedule-resync ()
  "(Re)arm the debounced timer that re-syncs Claude Code window dimensions."
  (when (timerp ps/claude--resize-timer)
    (cancel-timer ps/claude--resize-timer))
  (setq ps/claude--resize-timer
        (run-with-timer ps/claude-resize-debounce-delay nil
                         #'ps/claude--resync-windows)))

(defun ps/claude--on-window-size-change (_frame)
  "Schedule a debounced terminal resync after a window-size change.
Added to `window-size-change-functions'; only schedules work when a Claude
Code session buffer is currently displayed."
  (when (ps/claude--claude-windows)
    (ps/claude--debug-log "window-size-change detected, scheduling resync in %ss"
                           ps/claude-resize-debounce-delay)
    (ps/claude--schedule-resync)))

;;; Reflow throttling while dragging (fix #2b)

(defun ps/claude--dragging-p ()
  "Non-nil while a window-divider or frame drag is in flight.
`mouse-drag-line' sets `track-mouse' to the symbol `dragging' for the whole
drag and restores it on release, so this is a zero-cost probe that needs no
advice on any mouse command -- and therefore cannot collide with the
scrollbar module's own drag handling.

The `default-value' check is the load-bearing one: `eat-mode' makes
`track-mouse' buffer-local, and `mouse-drag-line' assigns it with a plain
`setq' in whichever buffer was selected when the drag started -- normally
the file buffer, i.e. the global binding.  Since this runs inside the eat
buffer (`window--adjust-process-windows' wraps the call in
`with-current-buffer'), eat's buffer-local value would otherwise shadow the
global `dragging' and the throttle would never engage.  The buffer-local
arm still catches a drag begun with the Claude window selected."
  (or (eq (default-value 'track-mouse) 'dragging)
      (eq track-mouse 'dragging)))

(defun ps/claude--throttle-reflow-p (now)
  "Non-nil if a reflow at time NOW should be skipped.
Pure/testable given `ps/claude--last-reflow-time'.  Only throttles while a
drag is in flight; a settled resize always reflows."
  (and (ps/claude--dragging-p)
       ps/claude--last-reflow-time
       (< (float-time (time-subtract now ps/claude--last-reflow-time))
          ps/claude-resize-throttle-interval)))

(defun ps/claude--reflow-throttle-advice (orig-fn &rest args)
  "Rate-limit eat's terminal reflow while a divider drag is in flight.
Emacs's process-window-size machinery runs once per mouse-motion event, and
each reflow rewrites eat's entire display region and makes Claude repaint --
which is the visible flicker.  Returning nil skips both the reflow and the
SIGWINCH for that event; the debounced resync then delivers one
authoritative resize once the drag settles.

Also clips lines for the duration of the drag (see
`ps/claude--begin-drag-clipping'): with the reflow suppressed, eat's rows
are still padded to the old terminal width, so a narrower window would wrap
every single row into a two-screen-line continuation -- a full-height
re-wrap on every drag pixel that throttling alone cannot prevent."
  (let ((now (current-time)))
    (if (ps/claude--throttle-reflow-p now)
        (progn
          (ps/claude--begin-drag-clipping)
          (ps/claude--debug-log "reflow throttled (drag in flight)")
          nil)
      (setq ps/claude--last-reflow-time now)
      (when (ps/claude--dragging-p) (ps/claude--begin-drag-clipping))
      (apply orig-fn args))))

;;; Line clipping while dragging

(defvar-local ps/claude--saved-truncate-lines 'unset
  "Buffer's `truncate-lines' before drag clipping, or the symbol `unset'.")

(defun ps/claude--begin-drag-clipping ()
  "Clip lines in the current Claude buffer for the duration of a drag.
Idempotent: the original `truncate-lines' is saved only on the first call,
so repeated motion events cannot lose it."
  (when (and (ps/claude--session-buffer-p (current-buffer))
             (eq ps/claude--saved-truncate-lines 'unset))
    (setq ps/claude--saved-truncate-lines truncate-lines)
    (setq-local truncate-lines t)
    (ps/claude--debug-log "drag clipping on (was truncate-lines=%s)"
                           ps/claude--saved-truncate-lines)))

(defun ps/claude--end-drag-clipping ()
  "Restore `truncate-lines' in the current Claude buffer after a drag."
  (unless (eq ps/claude--saved-truncate-lines 'unset)
    (setq-local truncate-lines ps/claude--saved-truncate-lines)
    (ps/claude--debug-log "drag clipping off (restored truncate-lines=%s)"
                           ps/claude--saved-truncate-lines)
    (setq ps/claude--saved-truncate-lines 'unset)))

(defun ps/claude--working-directory ()
  "Always use `my-org-base-directory' as the Claude Code IDE working directory.
Overrides `claude-code-ide--get-working-directory', whose default
(current project root) would resolve to this config's own source tree
for any buffer in this repo."
  (expand-file-name my-org-base-directory))

;;; Selection / active-buffer project key (fix #4)

(defun ps/claude--path-under-base-p (path)
  "Non-nil if PATH is inside `my-org-base-directory' (by expanded name).
Uses a plain expanded-name prefix test -- not `file-in-directory-p' -- so it
matches how claude-code-ide keys sessions (via `expand-file-name', not
`file-truename') and does not depend on the directory existing on disk."
  (and (stringp path)
       (boundp 'my-org-base-directory)
       my-org-base-directory
       (string-prefix-p (file-name-as-directory
                         (expand-file-name my-org-base-directory))
                        (expand-file-name path))))

(defun ps/claude--buffer-under-org-base-p ()
  "Non-nil if the current buffer visits a file under `my-org-base-directory'."
  (and buffer-file-name (ps/claude--path-under-base-p buffer-file-name)))

(defun ps/claude--buffer-project-advice (orig-fn &rest args)
  "Return `my-org-base-directory' as the project for buffers under it.
Keeps the project key used for selection / active-buffer tracking in sync
with the session key (also pinned to `my-org-base-directory' by
`ps/claude--working-directory').  Without this, selections from Org buffers
are looked up under the git repo root and silently dropped.  Buffers outside
the Org base fall through to ORIG-FN unchanged."
  (if (ps/claude--buffer-under-org-base-p)
      (expand-file-name my-org-base-directory)
    (apply orig-fn args)))

;;; Silent reload of stale, unmodified buffers Claude just wrote (fix #5)

(defun ps/claude--revert-stale-unmodified (path)
  "Quietly revert an unmodified, stale buffer visiting PATH under the Org base.
Closes the race where claude-code-ide re-visits a file it just wrote (via
`find-file-noselect') before `global-auto-revert' reverts the buffer, which
would otherwise pop a \"Reread from disk?\" prompt.  Modified buffers are
left alone so genuine edit conflicts still prompt (\"Discard your edits?\")."
  (when (ps/claude--path-under-base-p path)
    (when-let ((buf (find-buffer-visiting path)))
      (with-current-buffer buf
        (when (and (not (buffer-modified-p))
                   (not (verify-visited-file-modtime buf)))
          (revert-buffer :ignore-auto :noconfirm))))))

(defun ps/claude--diff-revert-advice (old-file-path &rest _)
  "Before opening a diff for OLD-FILE-PATH, refresh a stale unmodified buffer."
  (ps/claude--revert-stale-unmodified old-file-path))

(defun ps/claude--open-revert-advice (arguments &rest _)
  "Before opening a file from ARGUMENTS, refresh a stale unmodified buffer."
  (ps/claude--revert-stale-unmodified (alist-get 'filePath arguments)))

;;; eat output-queue crash guard (fix #6)

(defun ps/claude--eat-output-guard (orig-fn &rest args)
  "Swallow transient `args-out-of-range' errors from eat's output timer.
When the Claude diff window first opens, eat's terminal width state can
momentarily desync, making `eat--process-output-queue' signal
`args-out-of-range' from inside its timer.  Catch it, schedule a window
resync so the terminal re-renders cleanly, and continue.

Also brackets the call with the freeze-log op marker (see
`lisp/ps-freeze-log.el'): eat's output processing forces terminal redraws
and is a suspect for the whole-main-thread `ns_flush_display' freeze.  The
marker (not the append log) is used because this runs per output chunk; if
Emacs wedges here, the marker file still names this op."
  ;; `unwind-protect' so the marker is cleared even on the caught error.
  (when (fboundp 'ps/freeze-log-op-begin)
    (ps/freeze-log-op-begin "eat--process-output-queue"))
  (unwind-protect
      (condition-case err
          (apply orig-fn args)
        (args-out-of-range
         (ps/claude--schedule-resync)
         (message "ps/claude: recovered from eat output glitch (%s)"
                  (error-message-string err))
         nil))
    (when (fboundp 'ps/freeze-log-op-end)
      (ps/freeze-log-op-end))))

;;; Adaptive dock side (fix #7)

(defun ps/claude--adaptive-side-advice (orig-fn &rest args)
  "Dock the Claude Code side window `right' when the frame is wider than
tall, `bottom' otherwise.  Recomputed on every call, so it tracks the
frame's current shape rather than whatever it was when Emacs started."
  (let ((claude-code-ide-window-side
         (if (> (frame-pixel-width) (frame-pixel-height)) 'right 'bottom)))
    (apply orig-fn args)))

;;; Quiet exit while a session is running (fix #9)

(defcustom ps/claude-no-exit-prompt t
  "When non-nil, quitting Emacs does not prompt about Claude Code processes.
`save-buffers-kill-emacs' asks to confirm killing any live process whose
query-on-exit flag is set.  A running session contributes two such
processes -- the `eat' terminal and the MCP HTTP listener -- so both are
cleared.  Deliberately scoped to Claude Code rather than setting
`confirm-kill-processes' globally, which would also silently kill shells,
compilations and language servers."
  :type 'boolean
  :group 'claude-code-ide)

(defun ps/claude--suppress-terminal-exit-query ()
  "Stop the eat terminal process from prompting on exit.
Set buffer-locally from `eat-mode-hook', which runs before the process
exists.  That is fine and in fact necessary: `eat-exec' reads this variable
inside the buffer when it creates the process, so the flag is never set in
the first place, and eat's `auto' re-arming is skipped because the value is
no longer `auto'."
  (when ps/claude-no-exit-prompt
    (setq-local eat-query-before-killing-running-terminal nil)))

(defun ps/claude--suppress-mcp-exit-query (result)
  "Clear the query-on-exit flag on the MCP HTTP listener in RESULT.
`ws-start' creates its `make-network-process' without `:noquery', so the
listening socket prompts on exit by itself -- silencing only the terminal
is not enough.  Used as `:filter-return' advice, so RESULT is passed
through unchanged."
  (when ps/claude-no-exit-prompt
    (ignore-errors
      (let ((server (if (consp result) (car result) result)))
        (when (and server (fboundp 'ws-process))
          (when-let ((proc (ws-process server)))
            (when (processp proc)
              (set-process-query-on-exit-flag proc nil)))))))
  result)

(defun ps/claude--install-mode-line ()
  "Replace the default eat mode line with a plain \"Claude Code\" label."
  (when (ps/claude--session-buffer-p (current-buffer))
    (setq-local mode-line-format
                '(" " (:propertize "Claude Code" face mode-line-emphasis)))))

(defun ps/claude--setup-buffer ()
  "Apply the per-buffer Claude Code display tweaks.
Runs from `eat-mode-hook', which fires for every eat buffer, so each tweak
is guarded by `ps/claude--session-buffer-p'."
  (when (ps/claude--session-buffer-p (current-buffer))
    (ps/claude--install-mode-line)
    (ps/claude--suppress-terminal-exit-query)))

(defun ps/claude-setup ()
  "Apply Claude Code IDE window-size, working-directory and reliability tweaks.
Sets `claude-code-ide-window-width' from `ps/claude-window-width', installs
the debounced resize-resync/re-anchor hook and the drag reflow throttle,
pins the working directory and project key to `my-org-base-directory',
silences the post-write \"Reread from disk?\" race for unmodified buffers,
guards eat's output timer against transient `args-out-of-range' glitches,
docks the panel `right'/`bottom' to match the frame's current shape,
installs the per-buffer mode line, and stops a running session from
prompting when Emacs quits.  Idempotent."
  (setq claude-code-ide-window-width ps/claude-window-width)
  (add-hook 'window-size-change-functions #'ps/claude--on-window-size-change)
  (advice-add 'claude-code-ide--get-working-directory
              :override #'ps/claude--working-directory)
  (advice-add 'claude-code-ide-mcp--get-buffer-project
              :around #'ps/claude--buffer-project-advice)
  (advice-add 'claude-code-ide-mcp--create-diff-buffers
              :before #'ps/claude--diff-revert-advice)
  (advice-add 'claude-code-ide-mcp-handle-open-file
              :before #'ps/claude--open-revert-advice)
  (advice-add 'eat--process-output-queue
              :around #'ps/claude--eat-output-guard)
  (advice-add 'eat--adjust-process-window-size
              :around #'ps/claude--reflow-throttle-advice)
  (advice-add 'claude-code-ide--display-buffer-in-side-window
              :around #'ps/claude--adaptive-side-advice)
  (advice-add 'claude-code-ide-mcp-http-server-start
              :filter-return #'ps/claude--suppress-mcp-exit-query)
  (add-hook 'eat-mode-hook #'ps/claude--setup-buffer))

(provide 'ps-claude)
;;; ps-claude.el ends here
