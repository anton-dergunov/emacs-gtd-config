;;; ps-freeze-log.el --- Diagnostic log for the NS redisplay-flush freeze -*- lexical-binding: t; -*-

;;; Commentary:
;; Temporary diagnostic instrumentation for a hard-to-catch freeze: on this
;; macOS NS build (behind a BetterDisplay mirrored/scaled virtual screen),
;; Emacs occasionally wedges with the *entire* main thread blocked inside
;; `ns_flush_display' -- a display flush during redisplay that enters AppKit's
;; event loop and never returns.  Once wedged, NOTHING inside Emacs runs: no
;; keyboard, no mouse, not even its network MCP server, and no Emacs timer or
;; watchdog can recover it (they all run on the same blocked thread).  Recovery
;; is an external restart only.
;;
;; A native stack sample proves *where* it blocks (`ns_flush_display') but not
;; *which* Lisp path forced that redisplay.  The two prime suspects both bottom
;; out in the same C frame: the `ps-scrollbar' child-frame `(redisplay t)', and
;; the Claude Code `eat' terminal's high-frequency redraw.
;;
;; This module writes a small, immediately-flushed, chronological log so that
;; after a freeze the *tail* of the file names the last thing that ran:
;;
;;   - A low-rate heartbeat line (every `ps/freeze-log-heartbeat-interval'
;;     seconds).  The heartbeat runs on the main thread, so it stops the instant
;;     the wedge happens -- the last heartbeat timestamp is the freeze moment.
;;   - Focus in/out events, so a freeze can be correlated with returning from
;;     the background (the observed trigger).
;;   - BEGIN/END pairs around risky operations, logged by their own modules via
;;     `ps/freeze-log' (e.g. the scrollbar's forced redisplay).  A BEGIN with no
;;     matching END, immediately before the heartbeat stops, points the finger.
;;
;; The whole log line is written and the file is fsync-flushed before the risky
;; operation runs, so a "BEGIN redisplay ..." survives even if that redisplay is
;; exactly what wedges.
;;
;; Remove this module (and its `ps/freeze-log' call sites) once the root cause
;; is pinned down.

;;; Code:

(defgroup ps-freeze-log nil
  "Diagnostic logging for the NS redisplay-flush freeze."
  :group 'ps)

(defcustom ps/freeze-log-enabled t
  "When non-nil, `ps/freeze-log-setup' installs freeze instrumentation.
Set to nil (and re-run setup, or restart) to turn all of it off."
  :type 'boolean :group 'ps-freeze-log)

(defcustom ps/freeze-log-file
  (expand-file-name "tmp/ps-freeze.log" user-emacs-directory)
  "File the freeze diagnostics are appended to.
Kept under `user-emacs-directory' (local disk), deliberately NOT on the
Dropbox-backed org tree, so logging never itself blocks on that mount."
  :type 'file :group 'ps-freeze-log)

(defcustom ps/freeze-log-max-bytes (* 5 1024 1024)
  "Rotate `ps/freeze-log-file' once it grows past this many bytes.
A single old generation is kept as the file with \".1\" appended."
  :type 'integer :group 'ps-freeze-log)

(defcustom ps/freeze-log-heartbeat-interval 2
  "Seconds between heartbeat lines.  Lower pins the freeze moment more
precisely at the cost of more log volume."
  :type 'number :group 'ps-freeze-log)

(defcustom ps/freeze-log-marker-file
  (expand-file-name "tmp/ps-freeze-current-op.txt" user-emacs-directory)
  "Single-line file naming the operation currently in progress, or empty.
Used by high-frequency call sites (e.g. the Claude Code `eat' output queue)
via `ps/freeze-log-op-begin'/`ps/freeze-log-op-end' instead of the append
log, so they add no per-call log volume.  After a freeze, whatever this file
still contains is the operation that wedged."
  :type 'file :group 'ps-freeze-log)

(defvar ps/freeze-log--heartbeat-timer nil
  "Repeating timer that writes heartbeat lines, or nil.")

(defvar ps/freeze-log--heartbeat-count 0
  "Monotonic heartbeat counter, included in each heartbeat line.")

(defun ps/freeze-log--timestamp (&optional time)
  "Format TIME (default now) as an absolute, millisecond-precision stamp.
Pure/testable: takes an explicit TIME so tests need not depend on the clock."
  (let* ((time (or time (current-time)))
         (ms (mod (car (time-convert time 1000)) 1000)))
    (format "%s.%03d" (format-time-string "%Y-%m-%d %H:%M:%S" time) ms)))

(defun ps/freeze-log--format-line (timestamp category message)
  "Assemble one log line from TIMESTAMP, CATEGORY and MESSAGE.
Pure/testable.  CATEGORY is a symbol or string; MESSAGE is already formatted."
  (format "%s [%s] %s\n" timestamp category message))

(defun ps/freeze-log--should-rotate-p (size max)
  "Non-nil if a file of SIZE bytes should rotate given limit MAX.
Pure/testable.  MAX <= 0 disables rotation."
  (and (integerp max) (> max 0) (integerp size) (> size max)))

(defun ps/freeze-log--maybe-rotate ()
  "Rotate `ps/freeze-log-file' if it has grown past the size cap."
  (when (file-exists-p ps/freeze-log-file)
    (let ((size (ignore-errors
                  (file-attribute-size (file-attributes ps/freeze-log-file)))))
      (when (ps/freeze-log--should-rotate-p size ps/freeze-log-max-bytes)
        (ignore-errors
          (rename-file ps/freeze-log-file
                       (concat ps/freeze-log-file ".1") t))))))

(defun ps/freeze-log (category fmt &rest args)
  "Append one immediately-flushed line to `ps/freeze-log-file'.
CATEGORY is a short symbol (e.g. `focus', `scrollbar', `heartbeat'); FMT/ARGS
are a `format' spec.  A no-op when `ps/freeze-log-enabled' is nil.  Never
signals -- diagnostics must not become their own source of failures."
  (when ps/freeze-log-enabled
    (ignore-errors
      (ps/freeze-log--maybe-rotate)
      (let* ((line (ps/freeze-log--format-line
                    (ps/freeze-log--timestamp)
                    category
                    (apply #'format fmt args)))
             ;; Immediate, non-buffered append + fsync so the line survives
             ;; even if the very next thing this process does is wedge.
             (write-region-inhibit-fsync nil)
             (coding-system-for-write 'utf-8-unix)
             (inhibit-message t)
             (message-log-max nil))
        (write-region line nil ps/freeze-log-file 'append 'silent)))))

(defun ps/freeze-log-op-begin (label)
  "Record LABEL as the operation currently in progress (overwrites the marker).
For high-frequency risky call sites that must not flood the append log.
Pair with `ps/freeze-log-op-end'.  Never signals."
  (when ps/freeze-log-enabled
    (ignore-errors
      (let ((write-region-inhibit-fsync nil)
            (coding-system-for-write 'utf-8-unix)
            (inhibit-message t)
            (message-log-max nil))
        (write-region (format "%s %s\n" (ps/freeze-log--timestamp) label)
                      nil ps/freeze-log-marker-file nil 'silent)))))

(defun ps/freeze-log-op-end ()
  "Clear the in-progress-operation marker (see `ps/freeze-log-op-begin')."
  (when ps/freeze-log-enabled
    (ignore-errors
      (let ((write-region-inhibit-fsync nil)
            (coding-system-for-write 'utf-8-unix)
            (inhibit-message t)
            (message-log-max nil))
        (write-region "" nil ps/freeze-log-marker-file nil 'silent)))))

(defun ps/freeze-log--heartbeat ()
  "Write one heartbeat line (main-thread liveness marker)."
  (setq ps/freeze-log--heartbeat-count (1+ ps/freeze-log--heartbeat-count))
  (ps/freeze-log 'heartbeat "alive #%d focus=%s"
                 ps/freeze-log--heartbeat-count
                 (frame-focus-state)))

(defun ps/freeze-log--on-focus-change ()
  "Log every OS focus gain/loss (correlates freezes with backgrounding)."
  (ps/freeze-log 'focus "focus-change -> %s" (frame-focus-state)))

;;;###autoload
(defun ps/freeze-log-setup ()
  "Install freeze instrumentation: heartbeat timer + focus logging.
Idempotent; safe to call again to pick up changed settings."
  (interactive)
  (ps/freeze-log-teardown)
  (when ps/freeze-log-enabled
    ;; Ensure the log/marker directory exists: `ps/freeze-log-setup' runs
    ;; during early init, before the `tmp/' dir the auto-save block creates,
    ;; and the writers swallow errors -- so without this the first writes
    ;; would silently vanish.
    (dolist (f (list ps/freeze-log-file ps/freeze-log-marker-file))
      (ignore-errors (make-directory (file-name-directory f) t)))
    (ps/freeze-log 'session "---- freeze-log started (pid %s, emacs %s) ----"
                   (emacs-pid) emacs-version)
    (add-function :after after-focus-change-function
                  #'ps/freeze-log--on-focus-change)
    (setq ps/freeze-log--heartbeat-timer
          (run-with-timer ps/freeze-log-heartbeat-interval
                          ps/freeze-log-heartbeat-interval
                          #'ps/freeze-log--heartbeat))))

(defun ps/freeze-log-teardown ()
  "Remove freeze instrumentation installed by `ps/freeze-log-setup'."
  (interactive)
  (when ps/freeze-log--heartbeat-timer
    (cancel-timer ps/freeze-log--heartbeat-timer)
    (setq ps/freeze-log--heartbeat-timer nil))
  (remove-function after-focus-change-function
                   #'ps/freeze-log--on-focus-change))

(provide 'ps-freeze-log)
;;; ps-freeze-log.el ends here
