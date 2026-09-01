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
;;    `ps/claude-resize-throttle-interval' during a burst of closely-spaced
;;    resize attempts (`ps/claude-resize-burst-gap' apart or less).  This is
;;    deliberately not gated on `track-mouse': a live diagnostic found
;;    Emacs's own drag bookkeeping can leave it stuck reporting an active
;;    drag forever (see `ps/claude--in-resize-burst-p'), which would have
;;    made every future resize misbehave the same way.  Set
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
;;    prompts ("Discard your edits?").  This is only the Claude-specific fast
;;    path, and it covers just the two entry points advised below for files
;;    under `my-org-base-directory'; the general backstop is
;;    `revert-without-query' in config.org's auto-revert block, which closes
;;    the same race on every `find-file-noselect' path and for any path.
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
;;    only via `set-window-point' plus an unclamped `recenter', and it
;;    derives that `recenter' argument from `how-many "\n"', a count of
;;    *buffer* lines, while `recenter' itself positions by *screen* lines.
;;    Those two agree only for as long as nothing wraps, which is not
;;    guaranteed (see #12).  Three consequences: `eat--t-resize' refuses to
;;    move the display region's start backwards when the window grows, so
;;    `recenter' is handed a more negative argument than there are lines and
;;    drags the window up into stale scrollback (content missing at the top,
;;    dead space at the bottom); a single wrapped row shifts the whole region
;;    up by one line for the same reason; and
;;    `eat--synchronize-scroll-windows' only re-anchors windows whose point
;;    sits exactly on the terminal cursor, so a window the user scrolled up
;;    in is abandoned while the reflow rewrites and trims the text under it
;;    (a blank window).  So in session buffers eat's synchronizer is replaced
;;    by `ps/claude--synchronize-scroll', which sets the window start
;;    straight to eat's display beginning -- the position all that arithmetic
;;    is trying to arrive at, stated directly so it cannot drift -- and the
;;    same explicit anchoring is applied after a resize: windows at the
;;    bottom are pinned to the display beginning, scrolled-up windows keep
;;    their position and are only corrected when it is no longer valid.
;;
;; 9. A running session keeps several processes alive whose query-on-exit
;;    flags would make `save-buffers-kill-emacs' prompt: eat's terminal, the
;;    MCP HTTP listener (`ws-start' omits `:noquery'), and each accepted MCP
;;    websocket connection (`websocket-server' sets `:noquery' on the
;;    listener, but accepted connections do not inherit it).  All are
;;    cleared in one sweep just before exit, so Emacs quits without asking
;;    (see `ps/claude-no-exit-prompt').
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
;;
;; 10. The editor selection reaches Claude as a `selection_changed'
;;     notification carrying the file, the selected text and its start/end
;;     positions; the CLI turns a *non-empty* text into its "selected lines"
;;     context block and an empty one into a plain "this file is open" block.
;;     Three things kept that from working here.  (a) Positions: the package
;;     sends 1-based lines and columns, but the protocol is VS Code's and the
;;     CLI adds one to the start line, so every range Claude was told about
;;     was one line too high -- `ps/claude--current-selection' replaces the
;;     payload builder with a 0-based one.  (b) Nothing was sent when a
;;     session connects, so the first prompt did not know which file was
;;     open.  (c) The CLI *clears* what it holds after every prompt you
;;     submit, while the package only sends on change -- so from the second
;;     prompt on it had nothing at all, which is why asking about a selection
;;     was answered with "no selection was passed" even though the panel's
;;     footer still showed one.  We therefore re-assert the current selection
;;     when a session connects and whenever the panel window becomes the
;;     selected one -- the last moment before a prompt is typed.  A resend is
;;     skipped when the payload is unchanged *and* no prompt has been
;;     submitted since it was last sent (`ps/claude--panel-input-seen'), so
;;     follow-up prompts typed without leaving the panel never re-attach the
;;     same lines.  Finally, a region is easy to lose on the way to the panel
;;     (any scroll that moves point past it collapses it), so the last
;;     non-empty region of a file buffer is remembered and used when the live
;;     one is gone; the snapshot is dropped as soon as a command runs in that
;;     buffer with no region, which is what an explicit deselect looks like.
;;
;; 11. `eat's interactive keymaps redirect most keys straight to the
;;     underlying process, but their `:ascii' category (see
;;     `eat-term-make-keymap') only covers the control-character range plus
;;     a remap of `self-insert-command' -- it never touches Super-modified
;;     keys. `C-y' is explicitly rebound there to `eat-yank' (send the
;;     yanked text to the process as real terminal input, honoring
;;     bracketed paste), but `s-v' is not, so it falls through to the
;;     *global* map's `s-v' -> `yank' (a stock macOS default from
;;     `ns-win.el'). Plain `yank' inserts text directly into the Emacs
;;     buffer instead of feeding it to the pty, and eat's own redraw
;;     immediately overwrites that insertion -- invisible to the user. That
;;     is why Cmd-V (typed by hand, or synthesized by a dictation app such
;;     as Handy) silently does nothing in a Claude Code session while `C-y'
;;     already works. We bind `s-v' to `eat-yank' in both of eat's
;;     interactive keymaps so it behaves like `C-y' already does.
;;
;; 12. A session buffer must never soft-wrap.  eat has already wrapped the
;;     program's output at the terminal width, so one terminal row is one
;;     buffer line -- but only `truncate-lines' makes that also one *screen*
;;     line, and every scroll calculation above assumes it is.  Several
;;     glyphs Claude puts on almost every line are drawn wider than the cell
;;     they are counted as: measured here against an 8px monospace cell,
;;     U+23FA (the tool bullet), U+23BF (its result), U+23F5 (the mode
;;     footer), U+29C9 (the selection badge) and U+26A0 all render at 14px
;;     and U+2713 at 9px, while `char-width' reports 1 for each.  No
;;     installed font supplied a one-cell version of them all, so this cannot
;;     be fixed by choosing a font, and it is not Claude's error either --
;;     it pads a row to the width it was told.  Such a row then overruns the
;;     window by those few pixels and spills one character onto a second
;;     screen line, which is both visible as a stray fragment and enough to
;;     make the display region need more screen lines than the terminal has
;;     rows: the last rows fall out of view and the anchoring above is off by
;;     the number of wrapped rows.  Measured live at 67x39: 39 buffer lines
;;     occupying 41 screen lines in a 39-line window, which is what a
;;     half-painted pane that a manual resize "fixes" actually is.  Clipping
;;     is the right trade -- a few overhanging pixels of one glyph, rather
;;     than the layout of the whole pane.  The truncation arrow is suppressed
;;     along with it, because the right fringe here is the scroll-bar track.

;;; Code:

(require 'seq)
(require 'ps-vault)

(declare-function claude-code-ide--sync-terminal-dimensions "claude-code-ide")
(declare-function claude-code-ide--get-working-directory "claude-code-ide")
(declare-function claude-code-ide-mcp--get-buffer-project "claude-code-ide-mcp")
(declare-function claude-code-ide-mcp--send-notification "claude-code-ide-mcp")
(declare-function claude-code-ide-mcp--on-open "claude-code-ide-mcp")
(declare-function claude-code-ide-mcp--get-current-selection "claude-code-ide-mcp")
(declare-function claude-code-ide-mcp--create-diff-buffers "claude-code-ide-mcp-handlers")
(declare-function claude-code-ide-mcp-handle-open-file "claude-code-ide-mcp-handlers")
(declare-function eat--process-output-queue "eat")
(declare-function eat-term-size "eat")
(declare-function eat-term-redisplay "eat")
(declare-function eat-term-resize "eat")
(declare-function eat-term-display-beginning "eat")
(declare-function eat-term-display-cursor "eat")
(declare-function eat--adjust-process-window-size "eat")
(declare-function ps/freeze-log--timestamp "ps-freeze-log")
(declare-function ps/freeze-log--format-line "ps-freeze-log")
(defvar eat-query-before-killing-running-terminal)
(declare-function claude-code-ide--display-buffer-in-side-window "claude-code-ide")
(declare-function claude-code-ide--terminal-position-keeper "claude-code-ide")
(declare-function eat-yank "eat")
(defvar claude-code-ide-window-width)
(defvar claude-code-ide-window-side)
(defvar my-org-base-directory)
(defvar eat-terminal)
(defvar eat-semi-char-mode-map)
(defvar eat-char-mode-map)
(defvar eat--synchronize-scroll-function)

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
  "Minimum seconds between terminal reflows during a burst of resize events.
Emacs runs the process-window-size machinery once per mouse-motion event
during a divider drag, and each reflow rewrites eat's whole display region
and makes Claude repaint -- which is what the flicker is.  Throttling to
this interval keeps the terminal tracking the new width without paying for
every pixel.  Only applies during an active burst (see
`ps/claude-resize-burst-gap'); an isolated resize always reflows."
  :type 'number
  :group 'claude-code-ide)

(defcustom ps/claude-resize-burst-gap 0.15
  "Maximum seconds between resize attempts still counted as the same burst.
A burst (many closely-spaced attempts, e.g. a divider drag) is what gets
throttled and line-clipped; an isolated resize -- one arriving more than
this long after the previous attempt -- never is."
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

(defcustom ps/claude-debug-selection nil
  "When non-nil, log what is sent to Claude about the editor selection.
Records each selection sent (file, range, size) and each resend that was
skipped and why, into `ps/claude-debug-resize-file'.  Off by default; turn
it on only to diagnose Claude not seeing the file or lines you selected."
  :type 'boolean
  :group 'claude-code-ide)

(defcustom ps/claude-selection-max-lines 500
  "Largest selection, in lines, sent to Claude as text.
The protocol has no way to name a range without its content, so a larger
selection is reported as \"this file is open\" instead of pasting the whole
region into every prompt.  Claude can read the file itself."
  :type 'integer
  :group 'claude-code-ide)

(defcustom ps/claude-selection-max-chars 20000
  "Largest selection, in characters, sent to Claude as text.
The companion of `ps/claude-selection-max-lines': whichever limit is
reached first turns the selection into a plain \"this file is open\"
report."
  :type 'integer
  :group 'claude-code-ide)

(defcustom ps/claude-selection-connect-delay 1.5
  "Seconds after a session connects before its first selection is sent.
The CLI only starts listening for editor updates once it has finished
connecting, so a notification sent immediately would be dropped."
  :type 'number
  :group 'claude-code-ide)

(defvar ps/claude--resize-timer nil
  "Pending timer for the next terminal-size resync, or nil.")

(defvar ps/claude--reanchor-timer nil
  "Pending timer for the follow-up re-anchor pass, or nil.")

(defvar ps/claude--last-reflow-time nil
  "Time of the last non-throttled terminal reflow, or nil.")

(defun ps/claude--write-log (tag format-string args)
  "Append FORMAT-STRING/ARGS to the debug log under TAG.
Reuses `ps-freeze-log's pure formatting helpers when that module is loaded
so timestamps match across logs, and falls back to a plain stamp otherwise.
Never signals -- diagnostics must not become their own source of failures."
  (ignore-errors
    (let* ((message (apply #'format format-string args))
           (line (if (and (fboundp 'ps/freeze-log--timestamp)
                          (fboundp 'ps/freeze-log--format-line))
                     (ps/freeze-log--format-line
                      (ps/freeze-log--timestamp) tag message)
                   (format "%s [%s] %s\n"
                           (format-time-string "%Y-%m-%d %H:%M:%S") tag message)))
           (write-region-inhibit-fsync nil)
           (coding-system-for-write 'utf-8-unix)
           (inhibit-message t)
           (message-log-max nil))
      (make-directory (file-name-directory ps/claude-debug-resize-file) t)
      (write-region line nil ps/claude-debug-resize-file 'append 'silent))))

(defun ps/claude--debug-log (format-string &rest args)
  "Append FORMAT-STRING/ARGS to the resize log when debugging is enabled."
  (when ps/claude-debug-resize
    (ps/claude--write-log 'claude-resize format-string args)))

(defun ps/claude--selection-log (format-string &rest args)
  "Append FORMAT-STRING/ARGS to the log when selection debugging is enabled."
  (when ps/claude-debug-selection
    (ps/claude--write-log 'claude-selection format-string args)))

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

(defun ps/claude--anchor-skip (rows window-lines)
  "Rows of the display region that belong above a window's first line.
ROWS is the terminal's height, WINDOW-LINES the window's.  Pure/testable.
Normally zero -- the terminal is kept as tall as the window, so its first
row is the window's first line.  While the window is shorter than the
terminal (the transient state between a shrink and the reflow that follows
it) only the last WINDOW-LINES rows fit, and the region has to start that
many rows further down."
  (if (and (integerp rows) (integerp window-lines))
      (max 0 (- rows window-lines))
    0))

(defun ps/claude--display-anchor (window)
  "Return the position WINDOW should start at to show eat's display region.
Nil when the current buffer has no live terminal."
  (when (ps/claude--terminal-live-p)
    (let ((skip (ps/claude--anchor-skip (cdr (eat-term-size eat-terminal))
                                        (window-body-height window))))
      (save-excursion
        (goto-char (eat-term-display-beginning eat-terminal))
        (forward-line skip)
        (point)))))

(defun ps/claude--synchronize-scroll (windows)
  "Anchor WINDOWS on eat's display region.

Installed buffer-locally in session buffers in place of
`eat--synchronize-scroll' (see `ps/claude--setup-buffer').  eat's own
version never sets a window start: it puts point on the terminal cursor and
`recenter's by an offset counted in buffer lines, which is the screen line
only for as long as nothing wraps (Commentary #8, #12).  Setting the start
directly says the same thing without the arithmetic that can drift.

WINDOWS may contain the symbol `buffer', as eat's version does, meaning the
current buffer's point rather than a window.

Installed by two routes, because two packages want this slot and their
order is not ours to rely on: the buffer-local `setq' above wins when
`claude-code-ide-eat-preserve-position' is off (eat's own default is then
left in place for us to replace), and the `:override' on
`claude-code-ide--terminal-position-keeper' wins when it is on -- that
function is assigned after `eat-mode' has already run our hook.  Its own
strategy is `recenter' too, and `recenter' counts screen lines, so it has
the same defect.

Its one behaviour worth keeping is the `buffer-read-only' guard: that is
what `eat-emacs-mode' (\[eat-emacs-mode]) sets to let you move around the
buffer with ordinary Emacs keys, and a window being read in must not be
dragged back to the prompt under you."
  (when (ps/claude--terminal-live-p)
    (let ((cursor (eat-term-display-cursor eat-terminal)))
      (dolist (window windows)
        (if (eq window 'buffer)
            (goto-char cursor)
          (when (and (window-live-p window) (not buffer-read-only))
            (when-let ((start (ps/claude--display-anchor window)))
              (set-window-start window start))
            (set-window-point window cursor)))))))

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
                   (anchor (ps/claude--display-anchor window))
                   (cursor (eat-term-display-cursor eat-terminal))
                   (start (window-start window))
                   (at-bottom (= (window-point window) cursor)))
              (if at-bottom
                  (progn
                    (ps/claude--debug-log
                     "%s: anchor at-bottom start=%s -> %s cursor=%s"
                     (buffer-name buffer) start anchor cursor)
                    (set-window-start window anchor)
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
  "Bring WINDOW's eat terminal and the `claude' process to WINDOW's size.

The window is the authority here, not `eat-term-size'.  Throttling means
eat's own reflow is skipped for most events of a resize burst, so eat's
internal size can still be the pre-drag one when this runs -- reading it
back and sending it to the process would just re-assert the stale size, and
the terminal would stay hard-wrapped at the old width no matter how many
times the resize settled.  So resize eat explicitly, then tell the process
that same size, leaving the two in agreement.

Falls back to `claude-code-ide--sync-terminal-dimensions' for backends
other than eat, or before the terminal is initialized.  Re-anchoring is
done separately by `ps/claude--reanchor-window'."
  (when (window-live-p window)
    (let ((buffer (window-buffer window)))
      (when (ps/claude--session-buffer-p buffer)
        (with-current-buffer buffer
          (if (ps/claude--terminal-live-p)
              (let* ((width (max (window-body-width window) 1))
                     (height (max (window-body-height window) 1))
                     (current (eat-term-size eat-terminal))
                     (proc (get-buffer-process buffer))
                     (inhibit-read-only t))
                (ps/claude--debug-log "%s: resync eat=%s -> window=(%d . %d) proc=%s"
                                       (buffer-name buffer) current width height
                                       (and proc t))
                (unless (equal current (cons width height))
                  (eat-term-resize eat-terminal width height))
                (eat-term-redisplay eat-terminal)
                (when proc
                  (set-process-window-size proc height width)))
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

;;; Reflow throttling during a resize burst (fix #2b)

(defvar ps/claude--last-attempt-time nil
  "Time of the last terminal-resize attempt, throttled or not, or nil.")

(defun ps/claude--in-resize-burst-p (now)
  "Non-nil if NOW arrives within `ps/claude-resize-burst-gap' of the
previous resize attempt -- i.e. events are arriving in rapid succession,
the signature of an active divider drag.

Deliberately not based on `track-mouse': `eat-mode' makes it buffer-local,
and `mouse-drag-line' restores it with a plain `setq' (not a `let') in
whichever buffer happens to be current when the drag ends.  Live diagnostic
on this machine confirmed that when the selected window changes mid-drag
-- e.g. the scrollbar module's own vertical-line click handler calls
`other-window' -- the restore lands in the wrong (buffer-local) slot and
the *global* value is left stuck at `dragging' forever, which would make
every future resize look like an active drag.  A frequency-based signal is
self-contained and self-clearing: it cannot get stuck, because it depends
on nothing but the timing of our own events."
  (and ps/claude--last-attempt-time
       (< (float-time (time-subtract now ps/claude--last-attempt-time))
          ps/claude-resize-burst-gap)))

(defun ps/claude--throttle-reflow-p (now burst)
  "Non-nil if a reflow at time NOW should be skipped.
Pure/testable given `ps/claude--last-reflow-time'.  Only throttles within
an active BURST (see `ps/claude--in-resize-burst-p'); an isolated resize --
including one right after a burst that has already gone stale -- always
reflows."
  (and burst
       ps/claude--last-reflow-time
       (< (float-time (time-subtract now ps/claude--last-reflow-time))
          ps/claude-resize-throttle-interval)))

(defun ps/claude--reflow-throttle-advice (orig-fn &rest args)
  "Rate-limit eat's terminal reflow during a burst of resize events.
Emacs's process-window-size machinery runs once per mouse-motion event, and
each reflow rewrites eat's entire display region and makes Claude repaint --
which is the visible flicker.  Returning nil skips both the reflow and the
SIGWINCH for that event; the debounced resync then delivers one
authoritative resize once the burst settles.

Rows left padded to the pre-drag width while the reflow is suppressed do
not need clipping of their own: session buffers never soft-wrap at all
(see `ps/claude--no-soft-wrap')."
  (let* ((now (current-time))
         (burst (ps/claude--in-resize-burst-p now)))
    (setq ps/claude--last-attempt-time now)
    (if (ps/claude--throttle-reflow-p now burst)
        (progn
          (ps/claude--debug-log "reflow throttled (burst in flight)")
          nil)
      (setq ps/claude--last-reflow-time now)
      (apply orig-fn args))))

;;; No soft wrapping in a session buffer (fix #12)

(defun ps/claude--no-soft-wrap ()
  "Stop the current session buffer from soft-wrapping eat's terminal rows.

eat has already wrapped the program's output at the terminal width, so one
terminal row is one buffer line -- and every scroll calculation here and in
eat assumes it is also one *screen* line.  A glyph drawn wider than the cell
it is counted as is enough to break that (Commentary #12), and the cost of
letting it is the layout of the whole pane, so the row is clipped instead.
`truncate-partial-width-windows' is pinned to nil so `truncate-lines' is the
only thing deciding, whatever width the panel is docked at, and the
truncation arrow is turned off because the right fringe here is the
scroll-bar track."
  (setq-local truncate-lines t)
  (setq-local truncate-partial-width-windows nil)
  (setq-local fringe-indicator-alist
              (cons '(truncation nil nil) fringe-indicator-alist)))

(defun ps/claude--working-directory ()
  "Always use `my-org-base-directory' as the Claude Code IDE working directory.
Overrides `claude-code-ide--get-working-directory', whose default
(current project root) would resolve to this config's own source tree
for any buffer in this repo.  With no vault open there is nothing better to
offer than `default-directory'."
  (if (ps/vault-configured-p)
      (expand-file-name my-org-base-directory)
    default-directory))

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

;;; The editor selection Claude is told about (fix #10)

(defvar-local ps/claude--region-snapshot nil
  "Bounds (START . END) of the last active region in this buffer, or nil.
Kept so that a selection which the editor has since collapsed -- scrolling
past it moves point, which moves the region's end -- can still be reported
to Claude.  Cleared as soon as a command runs in this buffer with no region,
which is what deselecting looks like.")

(defvar ps/claude--last-file-buffer nil
  "Last file buffer under the Org base that had point, or nil.
The buffer a resend reports from: by the time the panel is focused the
current buffer is the panel itself.")

(defvar ps/claude--last-sent nil
  "The last `selection_changed' payload handed to Claude, or nil.")

(defvar ps/claude--panel-input-seen nil
  "Non-nil once a prompt has been submitted since the last selection was sent.
The CLI discards what it knows about the editor after every submitted
prompt, so an unchanged selection has to be sent again -- but only then.")

(defun ps/claude--position (pos)
  "Return the 0-based (LINE . CHARACTER) of POS in the current buffer.
The protocol is VS Code's, where both are 0-based; Org buffers are
character-addressed, so CHARACTER counts characters from the line start
rather than display columns."
  (save-excursion
    (goto-char pos)
    (cons (1- (line-number-at-pos pos))
          (- pos (line-beginning-position)))))

(defun ps/claude--selection-params (file-path start end text)
  "Build the `selection_changed' params for FILE-PATH.
START and END are 0-based (LINE . CHARACTER) conses and TEXT the selected
string (empty when there is no selection).  Pure."
  `((text . ,text)
    (filePath . ,file-path)
    (selection . ((start . ((line . ,(car start))
                            (character . ,(cdr start))))
                  (end . ((line . ,(car end))
                          (character . ,(cdr end))))))))

(defun ps/claude--selection-too-large-p (start end)
  "Non-nil if the region from START to END is too big to send as text."
  (or (> (- end start) ps/claude-selection-max-chars)
      (> (count-lines start end) ps/claude-selection-max-lines)))

(defun ps/claude--snapshot-bounds ()
  "Return this buffer's remembered region bounds if they are still valid."
  (pcase ps/claude--region-snapshot
    (`(,start . ,end)
     (and (integerp start) (integerp end)
          (<= (point-min) start) (< start end) (<= end (point-max))
          (cons start end)))))

(defun ps/claude--selection-bounds ()
  "Return the (START . END) this buffer should report, or nil for none.
The live region wins; the remembered one stands in when the editor has
collapsed it."
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (ps/claude--snapshot-bounds)))

(defun ps/claude--current-selection ()
  "Return the `selection_changed' params for the current buffer.
Overrides `claude-code-ide-mcp--get-current-selection', whose payload is
1-based -- one line off, since the CLI reads it as VS Code's 0-based
positions -- and which knows nothing about a region the editor has since
collapsed.  An oversized selection degrades to the cursor position, which
the CLI reports as \"this file is open\"."
  (let* ((file-path (or (buffer-file-name) ""))
         (bounds (ps/claude--selection-bounds))
         (sendable (and bounds (not (ps/claude--selection-too-large-p
                                     (car bounds) (cdr bounds))))))
    (if sendable
        (ps/claude--selection-params
         file-path
         (ps/claude--position (car bounds))
         (ps/claude--position (cdr bounds))
         (buffer-substring-no-properties (car bounds) (cdr bounds)))
      (let ((position (ps/claude--position (if bounds (car bounds) (point)))))
        (ps/claude--selection-params file-path position position "")))))

(defun ps/claude--track-region ()
  "Remember this buffer and its region for a later resend.
Runs from `post-command-hook', so it only ever sees the buffer the user is
actually working in."
  (when (ps/claude--buffer-under-org-base-p)
    (setq ps/claude--last-file-buffer (current-buffer))
    (setq ps/claude--region-snapshot
          (when (use-region-p)
            (cons (region-beginning) (region-end))))))

(defun ps/claude--note-sent-selection (method params)
  "Record PARAMS as the last selection sent when METHOD is a selection change.
Added as advice on the package's own notification sender, so a selection it
sends counts as much as one we send ourselves."
  (when (equal method "selection_changed")
    (setq ps/claude--last-sent params
          ps/claude--panel-input-seen nil)))

(defun ps/claude--resend-needed-p (payload)
  "Non-nil if PAYLOAD has to be sent to Claude again.
Either it differs from what Claude was last told, or a prompt has been
submitted since -- which is when the CLI discards what it knew.  Pure given
`ps/claude--last-sent' and `ps/claude--panel-input-seen'."
  (or ps/claude--panel-input-seen
      (not (equal payload ps/claude--last-sent))))

(defun ps/claude--resend-target ()
  "Return the buffer a resend should report from, or nil."
  (and (buffer-live-p ps/claude--last-file-buffer)
       (with-current-buffer ps/claude--last-file-buffer
         (and (ps/claude--buffer-under-org-base-p) (current-buffer)))))

(defun ps/claude--resend-selection (reason)
  "Tell Claude about the current file and selection again, because REASON."
  (when (fboundp 'claude-code-ide-mcp--send-notification)
    (if-let ((buffer (ps/claude--resend-target)))
        (with-current-buffer buffer
          (let ((payload (ps/claude--current-selection)))
            (if (not (ps/claude--resend-needed-p payload))
                (ps/claude--selection-log "%s: unchanged, not resent" reason)
              (ps/claude--selection-log
               "%s: %s lines %s-%s, %d chars of text"
               reason (buffer-name)
               (1+ (alist-get 'line (alist-get 'start (alist-get 'selection payload))))
               (1+ (alist-get 'line (alist-get 'end (alist-get 'selection payload))))
               (length (alist-get 'text payload)))
              (claude-code-ide-mcp--send-notification "selection_changed" payload))))
      (ps/claude--selection-log "%s: no file buffer to report from" reason))))

(defun ps/claude--panel-selected-p (window)
  "Non-nil if WINDOW is the selected window and shows a Claude session."
  (and (window-live-p window)
       (eq window (selected-window))
       (ps/claude--session-buffer-p (window-buffer window))))

(defun ps/claude--selected-window (frame-or-window)
  "Return the window FRAME-OR-WINDOW stands for, or nil.
`window-selection-change-functions' hands its *default* value a frame --
only a buffer-local registration is handed a window -- so a handler that
assumed a window would never fire at all."
  (cond ((framep frame-or-window) (frame-selected-window frame-or-window))
        ((window-live-p frame-or-window) frame-or-window)))

(defun ps/claude--on-window-selection-change (frame-or-window)
  "Re-assert the selection when the panel becomes the selected window.
This is the last moment before a prompt is typed, and the CLI has usually
discarded what it knew by now (it does so after every prompt).  Deferred
onto a timer because this runs from redisplay."
  (when (ps/claude--panel-selected-p
         (ps/claude--selected-window frame-or-window))
    (run-with-timer 0 nil #'ps/claude--resend-selection "panel focused")))

(defun ps/claude--note-panel-input ()
  "Note that a prompt was probably just submitted in this panel.
Buffer-local `post-command-hook' entry in session buffers: RET is the
CLI's submit key, and submitting is what makes it forget the editor state."
  (when (memq last-command-event '(?\r ?\n return))
    (setq ps/claude--panel-input-seen t)))

(defun ps/claude--on-session-connected (&rest _)
  "Send the current file and selection once a new session has connected.
Without this the first prompt of a session does not know which file is
open, since the package only reports changes."
  (setq ps/claude--panel-input-seen t)
  (run-with-timer ps/claude-selection-connect-delay nil
                  #'ps/claude--resend-selection "session connected"))

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

(defun ps/claude--eat-geometry ()
  "Return (EAT-COLS EAT-ROWS WIN-W WIN-H PMAX DISP-BEGIN) for the current eat
buffer, or nil if there is no live terminal.  Each element may be nil if
unavailable.  Pure-ish snapshot used only for freeze diagnostics; never signals."
  (ignore-errors
    (when (ps/claude--terminal-live-p)
      (let* ((size (ignore-errors (eat-term-size eat-terminal)))
             (win  (get-buffer-window (current-buffer)))
             (db   (ignore-errors (eat-term-display-beginning eat-terminal))))
        (list (and (consp size) (car size))
              (and (consp size) (cdr size))
              (and win (window-body-width win))
              (and win (window-body-height win))
              (point-max)
              (cond ((markerp db) (marker-position db))
                    ((integerp db) db)
                    (t nil)))))))

(defun ps/claude--eat-desync-p (geom)
  "Non-nil if GEOM (from `ps/claude--eat-geometry') shows eat's terminal size
disagreeing with the window body size -- the corrupted state that precedes an
`eat--process-output-queue' runaway.  Pure/testable.  Only reports when BOTH
sides are known, so a windowless buffer is never a false positive."
  (pcase geom
    (`(,ec ,er ,ww ,wh ,_pmax ,_db)
     (and (integerp ec) (integerp er) (integerp ww) (integerp wh)
          (or (/= ec ww) (/= er wh))))))

(defun ps/claude--eat-geometry-string (geom)
  "Format GEOM compactly for the freeze-log op marker.  Pure/testable."
  (pcase geom
    ('nil "eat=none")
    (`(,ec ,er ,ww ,wh ,pmax ,db)
     (format "eat=%sx%s win=%sx%s%s pmax=%s db=%s"
             ec er ww wh (if (ps/claude--eat-desync-p geom) " DESYNC" "")
             pmax db))))

(defvar ps/claude--eat-desync-log-time nil
  "Time of the last appended eat-desync line, to throttle the append log.")

(defun ps/claude--eat-output-guard (orig-fn &rest args)
  "Swallow transient `args-out-of-range' errors from eat's output timer.
When the Claude diff window first opens, eat's terminal width state can
momentarily desync, making `eat--process-output-queue' signal
`args-out-of-range' from inside its timer.  Catch it, schedule a window
resync so the terminal re-renders cleanly, and continue.

Also brackets the call with the freeze-log op marker (see
`lisp/ps-freeze-log.el'): a single non-returning `eat--process-output-queue'
call, spinning at 100%% CPU over a desynced terminal, is a confirmed freeze
mode.  The marker records eat's geometry at entry (via
`ps/claude--eat-geometry-string')
so that after a freeze `cat ps-freeze-current-op.txt' shows the exact
terminal-vs-window mismatch that triggered the runaway; a gross desync is
also appended (throttled) to the main log so recoverable near-misses leave a
timeline.  The marker (not per-call appends) carries the state because this
runs per output chunk."
  (let ((geom (ps/claude--eat-geometry)))
    ;; `unwind-protect' so the marker is cleared even on the caught error.
    (when (fboundp 'ps/freeze-log-op-begin)
      (ps/freeze-log-op-begin
       (concat "eat--process-output-queue " (ps/claude--eat-geometry-string geom))))
    ;; Throttled append (<=1/5s) when grossly desynced, for a timeline of
    ;; near-misses that recovered vs. the one that eventually wedged.
    (when (and (ps/claude--eat-desync-p geom) (fboundp 'ps/freeze-log)
               (let ((now (float-time)))
                 (prog1 (or (null ps/claude--eat-desync-log-time)
                            (> (- now ps/claude--eat-desync-log-time) 5))
                   (setq ps/claude--eat-desync-log-time now))))
      (ps/freeze-log 'eat "desync at output-queue entry: %s"
                     (ps/claude--eat-geometry-string geom)))
    (unwind-protect
        (condition-case err
            (apply orig-fn args)
          (args-out-of-range
           (ps/claude--schedule-resync)
           (message "ps/claude: recovered from eat output glitch (%s)"
                    (error-message-string err))
           nil))
      (when (fboundp 'ps/freeze-log-op-end)
        (ps/freeze-log-op-end)))))

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
query-on-exit flag is set.  A running session contributes several: the
`eat' terminal, the MCP HTTP listener, the MCP websocket listener, and one
accepted websocket connection per client.  Deliberately scoped to Claude
Code rather than setting `confirm-kill-processes' globally, which would
also silently kill shells, compilations and language servers."
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

(defun ps/claude--exit-query-process-p (proc)
  "Non-nil if PROC belongs to Claude Code and may block exit.
Recognises three shapes, all created by `claude-code-ide':
- the `eat' terminal, by its session buffer;
- websocket processes, by the `:websocket' property `websocket-server-accept'
  puts on every accepted connection (the listener sets `:noquery' itself,
  but the accepted connections do not inherit it), or by the listener's own
  \"websocket server on port N\" name;
- the MCP HTTP listener, whose `web-server' process is named \"ws-server\".

Matching on these rather than on a name pattern alone keeps unrelated
network processes -- and any other package's shells or servers -- untouched."
  (and (processp proc)
       (let ((name (or (process-name proc) "")))
         (or (ps/claude--session-buffer-p (process-buffer proc))
             (process-get proc :websocket)
             (string-prefix-p "websocket server on port" name)
             (string-prefix-p "ws-server" name)))))

(defun ps/claude--clear-exit-queries (&rest _)
  "Clear the query-on-exit flag on every live Claude Code process.
Run as `:before' advice on `save-buffers-kill-emacs', i.e. immediately
before it scans the process list, so it also covers connections accepted
after startup -- which is why this is a sweep rather than advice on each
server's constructor.  Never signals: quitting must not be blocked by a
failure in here."
  (when ps/claude-no-exit-prompt
    (ignore-errors
      (dolist (proc (process-list))
        (when (ps/claude--exit-query-process-p proc)
          (set-process-query-on-exit-flag proc nil))))))

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
    (ps/claude--no-soft-wrap)
    (setq-local eat--synchronize-scroll-function
                #'ps/claude--synchronize-scroll)
    (ps/claude--suppress-terminal-exit-query)
    (add-hook 'post-command-hook #'ps/claude--note-panel-input nil t)))

(defun ps/claude-setup ()
  "Apply Claude Code IDE window-size, working-directory and reliability tweaks.
Sets `claude-code-ide-window-width' from `ps/claude-window-width', installs
the debounced resize-resync/re-anchor hook and the drag reflow throttle,
pins the working directory and project key to `my-org-base-directory',
keeps Claude told about the open file and the selected lines (see fix #10
in the Commentary), silences the post-write \"Reread from disk?\" race for
unmodified buffers,
guards eat's output timer against transient `args-out-of-range' glitches,
docks the panel `right'/`bottom' to match the frame's current shape,
installs the per-buffer mode line, stops session buffers from soft-wrapping
eat's rows and anchors their windows on eat's display region directly,
binds Cmd-V to send pasted text to the
process instead of the buffer, and stops a running session from prompting
when Emacs quits.  Idempotent."
  (setq claude-code-ide-window-width ps/claude-window-width)
  (add-hook 'window-size-change-functions #'ps/claude--on-window-size-change)
  (advice-add 'claude-code-ide--get-working-directory
              :override #'ps/claude--working-directory)
  (advice-add 'claude-code-ide-mcp--get-buffer-project
              :around #'ps/claude--buffer-project-advice)
  (advice-add 'claude-code-ide-mcp--get-current-selection
              :override #'ps/claude--current-selection)
  (advice-add 'claude-code-ide-mcp--send-notification
              :after #'ps/claude--note-sent-selection)
  (advice-add 'claude-code-ide-mcp--on-open
              :after #'ps/claude--on-session-connected)
  (add-hook 'post-command-hook #'ps/claude--track-region)
  (add-hook 'window-selection-change-functions
            #'ps/claude--on-window-selection-change)
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
  (advice-add 'claude-code-ide--terminal-position-keeper
              :override #'ps/claude--synchronize-scroll)
  (advice-add 'save-buffers-kill-emacs
              :before #'ps/claude--clear-exit-queries)
  (add-hook 'eat-mode-hook #'ps/claude--setup-buffer)
  ;; `eat' is `:defer t' -- its keymaps do not exist yet at this point, only
  ;; its autoloads do, so binding into them has to wait for the real load.
  (with-eval-after-load 'eat
    (define-key eat-semi-char-mode-map (kbd "s-v") #'eat-yank)
    (define-key eat-char-mode-map (kbd "s-v") #'eat-yank)))

(provide 'ps-claude)
;;; ps-claude.el ends here
