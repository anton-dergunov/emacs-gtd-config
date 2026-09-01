;;; test-ps-claude.el --- ERT tests for ps-claude -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path "lisp")
(require 'ps-claude)

;;; Session buffer detection

(ert-deftest ps/claude-test-session-buffer-p-name ()
  "Recognizes the `*claude-code[...]*' naming convention from a string."
  (should (ps/claude--session-buffer-p "*claude-code[my-project]*"))
  (should-not (ps/claude--session-buffer-p "*scratch*"))
  (should-not (ps/claude--session-buffer-p "claude-code[my-project]")))

(ert-deftest ps/claude-test-session-buffer-p-buffer ()
  "Recognizes the naming convention from a live buffer object."
  (let ((buf (generate-new-buffer "*claude-code[demo]*")))
    (unwind-protect
        (should (ps/claude--session-buffer-p buf))
      (kill-buffer buf)))
  (let ((buf (generate-new-buffer "*not-claude*")))
    (unwind-protect
        (should-not (ps/claude--session-buffer-p buf))
      (kill-buffer buf))))

(ert-deftest ps/claude-test-session-buffer-p-non-string ()
  "Returns nil for nil/non-string, non-buffer input rather than erroring."
  (should-not (ps/claude--session-buffer-p nil)))

;;; Resize debounce scheduling

(ert-deftest ps/claude-test-on-window-size-change-no-claude-buffer ()
  "No timer is scheduled when no Claude session buffer is displayed."
  (let ((ps/claude--resize-timer nil))
    (ps/claude--on-window-size-change (selected-frame))
    (should-not ps/claude--resize-timer)))

;;; Working directory override

(defvar my-org-base-directory)

(ert-deftest ps/claude-test-working-directory-uses-org-base ()
  "Always returns the expanded `my-org-base-directory', not the project root."
  (let ((dir (file-name-as-directory (make-temp-file "ps-claude-" t))))
    (unwind-protect
        (let ((my-org-base-directory dir))
          (should (equal (ps/claude--working-directory) (expand-file-name dir))))
      (delete-directory dir t))))

(ert-deftest ps/claude-test-working-directory-without-a-vault ()
  "With no vault open, Claude starts in `default-directory' rather than erroring."
  (let ((my-org-base-directory nil)
        (default-directory "/tmp/"))
    (should (equal (ps/claude--working-directory) "/tmp/"))))

;;; Buffer project key for selection / active-buffer tracking

(ert-deftest ps/claude-test-buffer-under-org-base-p ()
  "True only for file-backed buffers under `my-org-base-directory'."
  (let ((my-org-base-directory "/tmp/org-base/"))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/org-base/Areas/Inbox.org")
      (should (ps/claude--buffer-under-org-base-p)))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/other/file.el")
      (should-not (ps/claude--buffer-under-org-base-p)))
    (with-temp-buffer
      (setq buffer-file-name nil)
      (should-not (ps/claude--buffer-under-org-base-p)))))

(ert-deftest ps/claude-test-buffer-project-advice-under-base ()
  "Under the Org base, returns the base and never calls ORIG-FN."
  (let ((my-org-base-directory "/tmp/org-base/"))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/org-base/Areas/Inbox.org")
      (should (equal (ps/claude--buffer-project-advice
                      (lambda (&rest _) (error "ORIG-FN should not be called")))
                     (expand-file-name "/tmp/org-base/"))))))

(ert-deftest ps/claude-test-buffer-project-advice-falls-through ()
  "Outside the Org base, delegates to ORIG-FN unchanged."
  (let ((my-org-base-directory "/tmp/org-base/"))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/elsewhere/x.el")
      (should (equal (ps/claude--buffer-project-advice (lambda (&rest _) "ORIG"))
                     "ORIG")))))

;;; Silent reload of stale, unmodified buffers

(ert-deftest ps/claude-test-revert-stale-skips-modified ()
  "A modified buffer is left alone so genuine edit conflicts still prompt."
  (let* ((dir (make-temp-file "ps-claude-" t))
         (my-org-base-directory (file-name-as-directory dir))
         (file (expand-file-name "note.org" dir)))
    (unwind-protect
        (let ((buf (find-file-noselect file)))
          (unwind-protect
              (with-current-buffer buf
                (insert "local edit\n")
                (should (buffer-modified-p))
                (with-temp-file file (insert "changed on disk\n"))
                (set-file-times file (time-add (current-time) 100))
                (ps/claude--revert-stale-unmodified file)
                (should (buffer-modified-p))
                (should (string-match-p "local edit" (buffer-string))))
            (set-buffer-modified-p nil)
            (kill-buffer buf)))
      (delete-directory dir t))))

(ert-deftest ps/claude-test-revert-stale-reverts-unmodified ()
  "An unmodified, stale buffer is reverted to the on-disk content."
  (let* ((dir (make-temp-file "ps-claude-" t))
         (my-org-base-directory (file-name-as-directory dir))
         (file (expand-file-name "note.org" dir)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "original\n"))
          (let ((buf (find-file-noselect file)))
            (unwind-protect
                (with-current-buffer buf
                  (should-not (buffer-modified-p))
                  (with-temp-file file (insert "new content\n"))
                  (set-file-times file (time-add (current-time) 100))
                  (should-not (verify-visited-file-modtime buf))
                  (ps/claude--revert-stale-unmodified file)
                  (should (string-match-p "new content" (buffer-string)))
                  (should-not (buffer-modified-p)))
              (set-buffer-modified-p nil)
              (kill-buffer buf))))
      (delete-directory dir t))))

(ert-deftest ps/claude-test-revert-stale-ignores-outside-base ()
  "Paths outside `my-org-base-directory' are not touched."
  (let ((my-org-base-directory "/tmp/org-base/"))
    ;; No buffer visits this path; should simply do nothing without error.
    (should-not (ps/claude--revert-stale-unmodified "/tmp/elsewhere/x.org"))
    (should-not (ps/claude--revert-stale-unmodified nil))))

;;; eat output-queue crash guard

(ert-deftest ps/claude-test-eat-output-guard-passes-value ()
  "Normal calls pass their return value through untouched."
  (let ((ps/claude--resize-timer nil))
    (should (equal (ps/claude--eat-output-guard (lambda (&rest _) 'ok) 'arg) 'ok))
    (should-not ps/claude--resize-timer)))

(ert-deftest ps/claude-test-eat-output-guard-swallows-range-error ()
  "An `args-out-of-range' is swallowed and a resync is scheduled."
  (let ((ps/claude--resize-timer nil))
    (should-not (ps/claude--eat-output-guard
                 (lambda (&rest _) (signal 'args-out-of-range '("s" -1 2)))))
    (should (timerp ps/claude--resize-timer))
    (when (timerp ps/claude--resize-timer)
      (cancel-timer ps/claude--resize-timer))))

;;; Resync dimensions: the window is the authority, not eat's stale size

(ert-deftest ps/claude-test-resync-window-resizes-eat-to-window ()
  "A stale eat terminal is resized to the window, and the process is told
the window's size -- not eat's.  Regression test for the state seen live:
eat stuck at 123 columns inside an 82-column window, hard-wrapping every
line, because the resync read eat's own size back and re-sent it."
  (let ((buf (generate-new-buffer "*claude-code[demo]*"))
        sent-height sent-width resized redisplayed)
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local eat-terminal 'fake-terminal))
          (cl-letf (((symbol-function 'eat-term-size)
                     (lambda (_term) (cons 123 53)))   ; stale, pre-drag
                    ((symbol-function 'eat-term-resize)
                     (lambda (_term w h) (setq resized (cons w h))))
                    ((symbol-function 'eat-term-redisplay)
                     (lambda (_term) (setq redisplayed t)))
                    ((symbol-function 'get-buffer-process)
                     (lambda (_buf) 'fake-proc))
                    ((symbol-function 'set-process-window-size)
                     (lambda (_proc height width)
                       (setq sent-height height sent-width width)))
                    ((symbol-function 'window-live-p) (lambda (_w) t))
                    ((symbol-function 'window-buffer) (lambda (_w) buf))
                    ((symbol-function 'window-body-width) (lambda (_w) 82))
                    ((symbol-function 'window-body-height) (lambda (_w) 53)))
            (ps/claude--resync-window 'fake-window))
          (should (equal resized '(82 . 53)))
          (should (= sent-width 82))
          (should (= sent-height 53))
          (should redisplayed))
      (kill-buffer buf))))

(ert-deftest ps/claude-test-resync-window-skips-resize-when-already-correct ()
  "No needless reflow when eat already matches the window."
  (let ((buf (generate-new-buffer "*claude-code[demo]*"))
        resized)
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local eat-terminal 'fake-terminal))
          (cl-letf (((symbol-function 'eat-term-size)
                     (lambda (_term) (cons 82 53)))
                    ((symbol-function 'eat-term-resize)
                     (lambda (_term w h) (setq resized (cons w h))))
                    ((symbol-function 'eat-term-redisplay) (lambda (_term) nil))
                    ((symbol-function 'get-buffer-process) (lambda (_buf) nil))
                    ((symbol-function 'window-live-p) (lambda (_w) t))
                    ((symbol-function 'window-buffer) (lambda (_w) buf))
                    ((symbol-function 'window-body-width) (lambda (_w) 82))
                    ((symbol-function 'window-body-height) (lambda (_w) 53)))
            (ps/claude--resync-window 'fake-window))
          (should-not resized))
      (kill-buffer buf))))

(ert-deftest ps/claude-test-resync-window-falls-back-without-eat-terminal ()
  "Without a live `eat-terminal', falls back to `claude-code-ide--sync-terminal-dimensions'."
  (let ((buf (generate-new-buffer "*claude-code[demo]*"))
        fallback-called)
    (unwind-protect
        (cl-letf (((symbol-function 'claude-code-ide--sync-terminal-dimensions)
                   (lambda (_buf _win) (setq fallback-called t)))
                  ((symbol-function 'window-live-p) (lambda (_w) t))
                  ((symbol-function 'window-buffer) (lambda (_w) buf)))
          (ps/claude--resync-window 'fake-window)
          (should fallback-called))
      (kill-buffer buf))))

(ert-deftest ps/claude-test-resync-window-skips-non-claude-buffer ()
  "Does nothing for a window not showing a Claude Code session buffer."
  (let ((buf (generate-new-buffer "*not-claude*")))
    (unwind-protect
        (cl-letf (((symbol-function 'window-live-p) (lambda (_w) t))
                  ((symbol-function 'window-buffer) (lambda (_w) buf)))
          (should-not (ps/claude--resync-window 'fake-window)))
      (kill-buffer buf))))

;;; Window re-anchoring (window-start clamping)

(ert-deftest ps/claude-test-clamp-window-start-valid-needs-no-change ()
  "A start already inside the valid range needs no correction."
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (should-not (ps/claude--clamp-window-start 3 8))))

(ert-deftest ps/claude-test-clamp-window-start-past-display-begin ()
  "A start below the display beginning is pulled back to it."
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (should (= (ps/claude--clamp-window-start 12 8) 8))))

(ert-deftest ps/claude-test-clamp-window-start-before-point-min ()
  "A start above `point-min' is clamped to `point-min'."
  (with-temp-buffer
    (insert "one\ntwo\n")
    (should (= (ps/claude--clamp-window-start 0 5) (point-min)))))

(ert-deftest ps/claude-test-clamp-window-start-non-integer ()
  "A missing/invalid start falls back to the display beginning."
  (should (= (ps/claude--clamp-window-start nil 42) 42)))

;;; Resize burst detection

(ert-deftest ps/claude-test-burst-p-false-with-no-history ()
  "With no prior attempt recorded, nothing is in a burst."
  (let ((ps/claude--last-attempt-time nil))
    (should-not (ps/claude--in-resize-burst-p (current-time)))))

(ert-deftest ps/claude-test-burst-p-true-within-gap ()
  "Two attempts closer together than the burst gap count as a burst."
  (let ((ps/claude--last-attempt-time (current-time))
        (ps/claude-resize-burst-gap 10))
    (should (ps/claude--in-resize-burst-p (current-time)))))

(ert-deftest ps/claude-test-burst-p-false-past-gap ()
  "Two attempts farther apart than the burst gap are not a burst."
  (let ((ps/claude--last-attempt-time (time-subtract (current-time) 5))
        (ps/claude-resize-burst-gap 0.1))
    (should-not (ps/claude--in-resize-burst-p (current-time)))))

;;; Reflow throttling

(ert-deftest ps/claude-test-no-throttle-outside-burst ()
  "An isolated (non-burst) resize always reflows, however recent the last
reflow was."
  (let ((ps/claude--last-reflow-time (current-time)))
    (should-not (ps/claude--throttle-reflow-p (current-time) nil))))

(ert-deftest ps/claude-test-throttle-during-burst-within-interval ()
  "Inside a burst, a reflow inside the interval is skipped."
  (let ((ps/claude--last-reflow-time (current-time))
        (ps/claude-resize-throttle-interval 10))
    (should (ps/claude--throttle-reflow-p (current-time) t))))

(ert-deftest ps/claude-test-no-throttle-during-burst-past-interval ()
  "Inside a burst, a reflow past the interval is allowed through."
  (let ((ps/claude--last-reflow-time (time-subtract (current-time) 5))
        (ps/claude-resize-throttle-interval 0.1))
    (should-not (ps/claude--throttle-reflow-p (current-time) t))))

(ert-deftest ps/claude-test-no-throttle-on-first-reflow ()
  "With no recorded reflow yet, nothing is throttled even inside a burst."
  (let ((ps/claude--last-reflow-time nil))
    (should-not (ps/claude--throttle-reflow-p (current-time) t))))

(ert-deftest ps/claude-test-throttle-advice-passes-through ()
  "The advice runs ORIG-FN and records both times when not throttled."
  (let ((ps/claude--last-reflow-time nil)
        (ps/claude--last-attempt-time nil)
        called)
    (should (eq (ps/claude--reflow-throttle-advice
                 (lambda (&rest _) (setq called t) 'size))
                'size))
    (should called)
    (should ps/claude--last-reflow-time)
    (should ps/claude--last-attempt-time)))

(ert-deftest ps/claude-test-throttle-advice-skips-during-burst ()
  "The advice returns nil without calling ORIG-FN while throttled."
  (let ((ps/claude--last-reflow-time (current-time))
        (ps/claude--last-attempt-time (current-time))
        (ps/claude-resize-throttle-interval 10)
        (ps/claude-resize-burst-gap 10))
    (should-not (ps/claude--reflow-throttle-advice
                 (lambda (&rest _) (error "ORIG-FN should not be called"))))))

(ert-deftest ps/claude-test-throttle-advice-never-stuck-after-burst-goes-stale ()
  "Once a burst goes stale (no recent attempts), reflow resumes even if a
much older reflow looked like it was mid-drag -- the scenario that a stuck
global `track-mouse' used to cause permanently."
  (let ((ps/claude--last-reflow-time (time-subtract (current-time) 5))
        (ps/claude--last-attempt-time (time-subtract (current-time) 5))
        (ps/claude-resize-throttle-interval 0.25)
        (ps/claude-resize-burst-gap 0.15)
        called)
    (should (eq (ps/claude--reflow-throttle-advice
                 (lambda (&rest _) (setq called t) 'size))
                'size))
    (should called)))

;;; No soft wrapping in a session buffer

(ert-deftest ps/claude-test-no-soft-wrap-clips-lines ()
  "A session buffer truncates rather than wraps eat's rows."
  (let ((buf (generate-new-buffer "*claude-code[demo]*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local truncate-lines nil)
          (ps/claude--no-soft-wrap)
          (should truncate-lines))
      (kill-buffer buf))))

(ert-deftest ps/claude-test-no-soft-wrap-ignores-partial-width ()
  "`truncate-lines' decides alone, whichever width the panel is docked at.
The panel is a side window, so it is never full width, and the stock
`truncate-partial-width-windows' would otherwise get a say."
  (let ((buf (generate-new-buffer "*claude-code[demo]*")))
    (unwind-protect
        (with-current-buffer buf
          (ps/claude--no-soft-wrap)
          (should-not truncate-partial-width-windows))
      (kill-buffer buf))))

(ert-deftest ps/claude-test-no-soft-wrap-hides-the-truncation-arrow ()
  "No truncation arrow: the right fringe here is the scroll-bar track."
  (let ((buf (generate-new-buffer "*claude-code[demo]*")))
    (unwind-protect
        (with-current-buffer buf
          (ps/claude--no-soft-wrap)
          (should (equal (assq 'truncation fringe-indicator-alist)
                         '(truncation nil nil))))
      (kill-buffer buf))))

;;; Window anchoring on eat's display region

(ert-deftest ps/claude-test-synchronize-scroll-leaves-a-read-only-buffer-alone ()
  "Navigation mode (`eat-emacs-mode') must not be dragged back to the prompt.
That mode is exactly `buffer-read-only' plus ordinary Emacs keys, so a
window being read in has to keep its position."
  (let ((buf (generate-new-buffer "*claude-code[demo]*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "one\ntwo\nthree\n")
          (setq buffer-read-only t)
          (let ((window (display-buffer buf)))
            (unwind-protect
                (let ((start (window-start window)))
                  ;; No terminal here, so this also covers the guard that
                  ;; keeps the scroll path from signalling before one exists.
                  (ps/claude--synchronize-scroll (list window))
                  (should (= (window-start window) start)))
              (delete-window window))))
      (kill-buffer buf))))

(ert-deftest ps/claude-test-anchor-skip-is-zero-when-terminal-fits ()
  "The terminal is normally as tall as the window: nothing is skipped."
  (should (= (ps/claude--anchor-skip 39 39) 0)))

(ert-deftest ps/claude-test-anchor-skip-is-zero-when-window-is-taller ()
  "A window taller than the terminal still starts at the first row."
  (should (= (ps/claude--anchor-skip 30 39) 0)))

(ert-deftest ps/claude-test-anchor-skip-drops-rows-that-do-not-fit ()
  "Between a shrink and its reflow, only the last rows fit."
  (should (= (ps/claude--anchor-skip 39 30) 9)))

(ert-deftest ps/claude-test-anchor-skip-tolerates-unknown-sizes ()
  "A size that is not yet known must not signal from the scroll path."
  (should (= (ps/claude--anchor-skip nil 39) 0))
  (should (= (ps/claude--anchor-skip 39 nil) 0)))

;;; Blank-window detection

(ert-deftest ps/claude-test-window-blank-p-detects-whitespace-only ()
  "A window showing only whitespace counts as blank."
  (let ((buf (generate-new-buffer "*claude-code[demo]*")))
    (unwind-protect
        (progn
          (with-current-buffer buf (insert "\n\n   \n\t\n"))
          (cl-letf (((symbol-function 'window-live-p) (lambda (_w) t))
                    ((symbol-function 'window-buffer) (lambda (_w) buf))
                    ((symbol-function 'window-start) (lambda (_w) 1))
                    ((symbol-function 'window-end)
                     (lambda (_w &optional _u)
                       (with-current-buffer buf (point-max)))))
            (should (ps/claude--window-blank-p 'w))))
      (kill-buffer buf))))

(ert-deftest ps/claude-test-window-blank-p-false-with-content ()
  "A window showing real text is not blank."
  (let ((buf (generate-new-buffer "*claude-code[demo]*")))
    (unwind-protect
        (progn
          (with-current-buffer buf (insert "\n\n  hello  \n\n"))
          (cl-letf (((symbol-function 'window-live-p) (lambda (_w) t))
                    ((symbol-function 'window-buffer) (lambda (_w) buf))
                    ((symbol-function 'window-start) (lambda (_w) 1))
                    ((symbol-function 'window-end)
                     (lambda (_w &optional _u)
                       (with-current-buffer buf (point-max)))))
            (should-not (ps/claude--window-blank-p 'w))))
      (kill-buffer buf))))

;;; Quiet exit

(ert-deftest ps/claude-test-suppress-terminal-exit-query ()
  "The eat kill-confirmation variable is cleared buffer-locally."
  (let ((ps/claude-no-exit-prompt t))
    (with-temp-buffer
      (ps/claude--suppress-terminal-exit-query)
      (should (local-variable-p 'eat-query-before-killing-running-terminal))
      (should-not eat-query-before-killing-running-terminal))))

(ert-deftest ps/claude-test-suppress-terminal-exit-query-respects-toggle ()
  "Nothing is changed when `ps/claude-no-exit-prompt' is nil."
  (let ((ps/claude-no-exit-prompt nil))
    (with-temp-buffer
      (ps/claude--suppress-terminal-exit-query)
      (should-not (local-variable-p 'eat-query-before-killing-running-terminal)))))

(ert-deftest ps/claude-test-exit-query-recognises-accepted-websocket ()
  "An accepted websocket connection is recognised by its `:websocket' property.
The listener sets `:noquery' itself but accepted connections do not inherit
it -- this was the one process still blocking exit."
  (cl-letf (((symbol-function 'processp) (lambda (_p) t))
            ((symbol-function 'process-name) (lambda (_p) "websocket server on port 61672<1>"))
            ((symbol-function 'process-buffer) (lambda (_p) nil))
            ((symbol-function 'process-get)
             (lambda (_p prop) (eq prop :websocket))))
    (should (ps/claude--exit-query-process-p 'proc))))

(ert-deftest ps/claude-test-exit-query-recognises-http-listener ()
  "The web-server MCP listener is recognised by name."
  (cl-letf (((symbol-function 'processp) (lambda (_p) t))
            ((symbol-function 'process-name) (lambda (_p) "ws-server"))
            ((symbol-function 'process-buffer) (lambda (_p) nil))
            ((symbol-function 'process-get) (lambda (_p _prop) nil)))
    (should (ps/claude--exit-query-process-p 'proc))))

(ert-deftest ps/claude-test-exit-query-recognises-session-terminal ()
  "The eat terminal is recognised by its session buffer."
  (let ((buf (generate-new-buffer "*claude-code[demo]*")))
    (unwind-protect
        (cl-letf (((symbol-function 'processp) (lambda (_p) t))
                  ((symbol-function 'process-name) (lambda (_p) "claude"))
                  ((symbol-function 'process-buffer) (lambda (_p) buf))
                  ((symbol-function 'process-get) (lambda (_p _prop) nil)))
          (should (ps/claude--exit-query-process-p 'proc)))
      (kill-buffer buf))))

(ert-deftest ps/claude-test-exit-query-ignores-unrelated-process ()
  "Unrelated processes are left alone, so shells still prompt on exit."
  (let ((buf (generate-new-buffer "*shell*")))
    (unwind-protect
        (cl-letf (((symbol-function 'processp) (lambda (_p) t))
                  ((symbol-function 'process-name) (lambda (_p) "bash"))
                  ((symbol-function 'process-buffer) (lambda (_p) buf))
                  ((symbol-function 'process-get) (lambda (_p _prop) nil)))
          (should-not (ps/claude--exit-query-process-p 'proc)))
      (kill-buffer buf))))

(ert-deftest ps/claude-test-clear-exit-queries-only-touches-claude ()
  "The sweep clears Claude processes and leaves everything else alone."
  (let ((ps/claude-no-exit-prompt t)
        cleared)
    (cl-letf (((symbol-function 'process-list) (lambda () '(claude other)))
              ((symbol-function 'ps/claude--exit-query-process-p)
               (lambda (p) (eq p 'claude)))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (p flag) (push (cons p flag) cleared))))
      (ps/claude--clear-exit-queries)
      (should (equal cleared '((claude . nil)))))))

(ert-deftest ps/claude-test-clear-exit-queries-respects-toggle ()
  "Nothing is cleared when `ps/claude-no-exit-prompt' is nil."
  (let ((ps/claude-no-exit-prompt nil)
        cleared)
    (cl-letf (((symbol-function 'process-list) (lambda () '(claude)))
              ((symbol-function 'ps/claude--exit-query-process-p) (lambda (_p) t))
              ((symbol-function 'set-process-query-on-exit-flag)
               (lambda (p flag) (push (cons p flag) cleared))))
      (ps/claude--clear-exit-queries)
      (should-not cleared))))

;;; Debug logging toggle

(ert-deftest ps/claude-test-debug-log-off-by-default ()
  "Writes nothing when `ps/claude-debug-resize' is nil (the default)."
  (let* ((file (make-temp-name (expand-file-name "ps-claude-log-"
                                                 temporary-file-directory)))
         (ps/claude-debug-resize nil)
         (ps/claude-debug-resize-file file))
    (ps/claude--debug-log "test %d" 1)
    (should-not (file-exists-p file))))

(ert-deftest ps/claude-test-debug-log-appends-when-enabled ()
  "Appends a tagged line to the log file when enabled."
  (let* ((file (make-temp-name (expand-file-name "ps-claude-log-"
                                                 temporary-file-directory)))
         (ps/claude-debug-resize t)
         (ps/claude-debug-resize-file file))
    (unwind-protect
        (progn
          (ps/claude--debug-log "hello %d" 42)
          (ps/claude--debug-log "again")
          (should (file-exists-p file))
          (let ((content (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))))
            (should (string-match-p "claude-resize" content))
            (should (string-match-p "hello 42" content))
            (should (string-match-p "again" content))))
      (ignore-errors (delete-file file)))))

;;; adaptive dock side

(ert-deftest ps/claude-test-adaptive-side-docks-right-when-wide ()
  "A wider-than-tall frame docks the panel to the right."
  (cl-letf (((symbol-function 'frame-pixel-width) (lambda (&rest _) 1600))
            ((symbol-function 'frame-pixel-height) (lambda (&rest _) 900)))
    (let (seen)
      (ps/claude--adaptive-side-advice
       (lambda (&rest _) (setq seen claude-code-ide-window-side)))
      (should (eq seen 'right)))))

(ert-deftest ps/claude-test-adaptive-side-docks-bottom-when-tall ()
  "A taller-than-wide frame docks the panel to the bottom."
  (cl-letf (((symbol-function 'frame-pixel-width) (lambda (&rest _) 900))
            ((symbol-function 'frame-pixel-height) (lambda (&rest _) 1600)))
    (let (seen)
      (ps/claude--adaptive-side-advice
       (lambda (&rest _) (setq seen claude-code-ide-window-side)))
      (should (eq seen 'bottom)))))

;;; eat geometry freeze diagnostics

(ert-deftest ps/claude-test-eat-desync-p-detects-mismatch ()
  "Desync is reported only when both eat and window sizes are known and differ."
  ;; cols differ
  (should (ps/claude--eat-desync-p '(80 24 100 24 500 1)))
  ;; rows differ
  (should (ps/claude--eat-desync-p '(80 24 80 40 500 1)))
  ;; agree -> no desync
  (should-not (ps/claude--eat-desync-p '(80 24 80 24 500 1)))
  ;; window size unknown (windowless buffer) -> never a false positive
  (should-not (ps/claude--eat-desync-p '(80 24 nil nil 500 1)))
  ;; eat size unknown -> not reported
  (should-not (ps/claude--eat-desync-p '(nil nil 80 24 500 1)))
  ;; nil geometry -> nil
  (should-not (ps/claude--eat-desync-p nil)))

(ert-deftest ps/claude-test-eat-geometry-string ()
  "The marker string is compact and flags a desync."
  (should (equal (ps/claude--eat-geometry-string nil) "eat=none"))
  (should (equal (ps/claude--eat-geometry-string '(80 24 80 24 500 1))
                 "eat=80x24 win=80x24 pmax=500 db=1"))
  (should (equal (ps/claude--eat-geometry-string '(80 24 100 24 500 1))
                 "eat=80x24 win=100x24 DESYNC pmax=500 db=1"))
  ;; nil fields render without erroring
  (should (equal (ps/claude--eat-geometry-string '(80 24 nil nil 500 nil))
                 "eat=80x24 win=nilxnil pmax=500 db=nil")))

;;; The editor selection Claude is told about

(defun ps/claude-test--selection (payload key &rest path)
  "Return PAYLOAD's KEY, descending through PATH inside the selection."
  (let ((value (alist-get key payload)))
    (dolist (step path value)
      (setq value (alist-get step value)))))

(ert-deftest ps/claude-test-position-is-zero-based ()
  "Lines and characters are 0-based, as the protocol (VS Code's) expects."
  (with-temp-buffer
    (insert "first line\nsecond line\nthird line\n")
    ;; Start of the buffer.
    (should (equal (ps/claude--position (point-min)) '(0 . 0)))
    ;; Third character of the second line.
    (goto-char (point-min))
    (forward-line 1)
    (should (equal (ps/claude--position (+ (point) 2)) '(1 . 2)))))

(ert-deftest ps/claude-test-selection-params-shape ()
  "The payload matches the schema the CLI parses."
  (let ((payload (ps/claude--selection-params "/tmp/a.org" '(3 . 0) '(5 . 7) "body")))
    (should (equal (alist-get 'text payload) "body"))
    (should (equal (alist-get 'filePath payload) "/tmp/a.org"))
    (should (equal (ps/claude-test--selection payload 'selection 'start 'line) 3))
    (should (equal (ps/claude-test--selection payload 'selection 'start 'character) 0))
    (should (equal (ps/claude-test--selection payload 'selection 'end 'line) 5))
    (should (equal (ps/claude-test--selection payload 'selection 'end 'character) 7))))

(ert-deftest ps/claude-test-current-selection-reports-the-region ()
  "An active region is reported with its text and 0-based bounds."
  (with-temp-buffer
    (insert "one\ntwo\nthree\nfour\n")
    (goto-char (point-min))
    (forward-line 1)
    (set-mark (point))
    (forward-line 2)
    (activate-mark)
    (let* ((ps/claude--region-snapshot nil)
           (payload (ps/claude--current-selection)))
      (should (equal (alist-get 'text payload) "two\nthree\n"))
      (should (equal (ps/claude-test--selection payload 'selection 'start 'line) 1))
      ;; A whole-line selection ends at column 0 of the following line, which
      ;; is what keeps the CLI's own line count right.
      (should (equal (ps/claude-test--selection payload 'selection 'end 'line) 3))
      (should (equal (ps/claude-test--selection payload 'selection 'end 'character) 0)))))

(ert-deftest ps/claude-test-current-selection-without-a-region ()
  "With no region the payload carries the cursor and no text."
  (with-temp-buffer
    (insert "one\ntwo\n")
    (goto-char (point-min))
    (forward-line 1)
    (let* ((ps/claude--region-snapshot nil)
           (payload (ps/claude--current-selection)))
      (should (equal (alist-get 'text payload) ""))
      (should (equal (ps/claude-test--selection payload 'selection 'start 'line) 1))
      (should (equal (ps/claude-test--selection payload 'selection 'end 'line) 1)))))

(ert-deftest ps/claude-test-current-selection-uses-the-snapshot ()
  "A collapsed region still reports the last one seen in that buffer."
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (let* ((ps/claude--region-snapshot (cons 5 (point-max)))
           (payload (ps/claude--current-selection)))
      (should (equal (alist-get 'text payload) "two\nthree\n")))))

(ert-deftest ps/claude-test-snapshot-bounds-are-validated ()
  "A snapshot pointing outside the buffer is ignored rather than signalling."
  (with-temp-buffer
    (insert "short\n")
    (let ((ps/claude--region-snapshot (cons 2 900)))
      (should-not (ps/claude--snapshot-bounds)))
    (let ((ps/claude--region-snapshot (cons 5 2)))
      (should-not (ps/claude--snapshot-bounds)))
    (let ((ps/claude--region-snapshot (cons 1 4)))
      (should (equal (ps/claude--snapshot-bounds) (cons 1 4))))))

(ert-deftest ps/claude-test-track-region-clears-the-snapshot ()
  "Deselecting in the buffer drops the remembered region."
  (with-temp-buffer
    (insert "one\ntwo\n")
    (let ((my-org-base-directory "/tmp/vault/")
          (buffer-file-name "/tmp/vault/plan.org"))
      (goto-char (point-min))
      (set-mark (point))
      (goto-char (point-max))
      (activate-mark)
      (ps/claude--track-region)
      (should ps/claude--region-snapshot)
      (deactivate-mark)
      (ps/claude--track-region)
      (should-not ps/claude--region-snapshot))))

(ert-deftest ps/claude-test-track-region-ignores-other-buffers ()
  "A buffer outside the Org base never becomes the resend target."
  (with-temp-buffer
    (let ((my-org-base-directory "/tmp/vault/")
          (buffer-file-name "/tmp/elsewhere/notes.org")
          (ps/claude--last-file-buffer 'unchanged))
      (ps/claude--track-region)
      (should (eq ps/claude--last-file-buffer 'unchanged)))))

(ert-deftest ps/claude-test-oversized-selection-degrades-to-the-file ()
  "Past the size cap the file is reported without pasting the region."
  (with-temp-buffer
    (dotimes (i 40) (insert (format "line %d\n" i)))
    (let ((ps/claude-selection-max-lines 10)
          (ps/claude-selection-max-chars 100000)
          (ps/claude--region-snapshot (cons (point-min) (point-max))))
      (should (equal (alist-get 'text (ps/claude--current-selection)) "")))
    (let ((ps/claude-selection-max-lines 1000)
          (ps/claude-selection-max-chars 10)
          (ps/claude--region-snapshot (cons (point-min) (point-max))))
      (should (equal (alist-get 'text (ps/claude--current-selection)) "")))
    (let ((ps/claude-selection-max-lines 1000)
          (ps/claude-selection-max-chars 100000)
          (ps/claude--region-snapshot (cons (point-min) (point-max))))
      (should-not (equal (alist-get 'text (ps/claude--current-selection)) "")))))

(ert-deftest ps/claude-test-cli-reconstructs-the-real-line-range ()
  "The range Claude ends up being told is the range that was selected.
The CLI reads the payload as VS Code does -- it reports the start line as
`start.line + 1' and the line count as `end.line - start.line + 1', minus
one when the selection ends at column 0.  Running that arithmetic here is
what pins the 0-based conversion: with the package's original 1-based
payload every range came out one line too high."
  (with-temp-buffer
    (insert "one\ntwo\nthree\nfour\nfive\n")
    ;; Select lines 2 to 4 inclusive, as a whole-line selection.
    (goto-char (point-min))
    (forward-line 1)
    (set-mark (point))
    (forward-line 3)
    (activate-mark)
    (let* ((payload (ps/claude--current-selection))
           (selection (alist-get 'selection payload))
           (start (alist-get 'start selection))
           (end (alist-get 'end selection))
           (line-start (1+ (alist-get 'line start)))
           (count (- (alist-get 'line end) (alist-get 'line start)))
           (count (if (zerop (alist-get 'character end)) count (1+ count)))
           (line-end (+ line-start count -1)))
      (should (equal line-start 2))
      (should (equal line-end 4))
      (should (equal (alist-get 'text payload) "two\nthree\nfour\n")))))

(ert-deftest ps/claude-test-resend-needed-p ()
  "An unchanged selection is resent only after a prompt was submitted."
  (let ((ps/claude--last-sent '((text . "x")))
        (ps/claude--panel-input-seen nil))
    (should-not (ps/claude--resend-needed-p '((text . "x"))))
    (should (ps/claude--resend-needed-p '((text . "y")))))
  (let ((ps/claude--last-sent '((text . "x")))
        (ps/claude--panel-input-seen t))
    (should (ps/claude--resend-needed-p '((text . "x"))))))

(ert-deftest ps/claude-test-note-sent-selection ()
  "Recording a sent selection also clears the submitted-prompt flag."
  (let ((ps/claude--last-sent nil)
        (ps/claude--panel-input-seen t))
    (ps/claude--note-sent-selection "selection_changed" '((text . "x")))
    (should (equal ps/claude--last-sent '((text . "x"))))
    (should-not ps/claude--panel-input-seen))
  ;; Other notifications leave the state alone.
  (let ((ps/claude--last-sent 'kept)
        (ps/claude--panel-input-seen t))
    (ps/claude--note-sent-selection "notifications/tools/list_changed" nil)
    (should (eq ps/claude--last-sent 'kept))
    (should ps/claude--panel-input-seen)))

(ert-deftest ps/claude-test-note-panel-input ()
  "Only RET counts as submitting a prompt."
  (let ((ps/claude--panel-input-seen nil)
        (last-command-event ?a))
    (ps/claude--note-panel-input)
    (should-not ps/claude--panel-input-seen)
    (setq last-command-event ?\r)
    (ps/claude--note-panel-input)
    (should ps/claude--panel-input-seen)))

(ert-deftest ps/claude-test-panel-selected-p ()
  "Only the selected window showing a session buffer counts."
  (let ((buf (generate-new-buffer "*claude-code[demo]*")))
    (unwind-protect
        (progn
          (should-not (ps/claude--panel-selected-p nil))
          (should-not (ps/claude--panel-selected-p (selected-window)))
          (set-window-buffer (selected-window) buf)
          (should (ps/claude--panel-selected-p (selected-window))))
      (set-window-buffer (selected-window) (get-buffer-create "*scratch*"))
      (kill-buffer buf))))

(ert-deftest ps/claude-test-selected-window-accepts-a-frame ()
  "The hook's default value is handed a frame, so a frame must resolve to
its selected window -- a handler that only understood windows would never
fire at all."
  (should (eq (ps/claude--selected-window (selected-frame))
              (frame-selected-window (selected-frame))))
  (should (eq (ps/claude--selected-window (selected-window)) (selected-window)))
  (should-not (ps/claude--selected-window nil)))

(ert-deftest ps/claude-test-resend-target-requires-a-live-buffer ()
  "A killed or unrelated buffer is never the resend target."
  (let ((ps/claude--last-file-buffer nil))
    (should-not (ps/claude--resend-target)))
  (let ((buf (generate-new-buffer " *ps-claude-target*")))
    (with-current-buffer buf
      (setq buffer-file-name "/tmp/vault/plan.org"))
    (let ((my-org-base-directory "/tmp/vault/")
          (ps/claude--last-file-buffer buf))
      (should (eq (ps/claude--resend-target) buf))
      (kill-buffer buf)
      (should-not (ps/claude--resend-target)))))

(provide 'test-ps-claude)
;;; test-ps-claude.el ends here
