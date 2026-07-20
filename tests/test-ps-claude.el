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
  (let ((my-org-base-directory "~/org/"))
    (should (equal (ps/claude--working-directory)
                   (expand-file-name "~/org/")))))

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

;;; Resync dimensions: eat-term-size over window-body-height/width

(ert-deftest ps/claude-test-resync-window-uses-eat-term-size ()
  "Resync sends eat's own `eat-term-size', not a recomputed value, and
forces an immediate `eat-term-redisplay'."
  (let ((buf (generate-new-buffer "*claude-code[demo]*"))
        sent-height sent-width redisplayed)
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local eat-terminal 'fake-terminal))
          (cl-letf (((symbol-function 'eat-term-size)
                     (lambda (_term) (cons 77 22)))
                    ((symbol-function 'eat-term-redisplay)
                     (lambda (_term) (setq redisplayed t)))
                    ((symbol-function 'get-buffer-process)
                     (lambda (_buf) 'fake-proc))
                    ((symbol-function 'set-process-window-size)
                     (lambda (_proc height width)
                       (setq sent-height height sent-width width)))
                    ((symbol-function 'window-live-p) (lambda (_w) t))
                    ((symbol-function 'window-buffer) (lambda (_w) buf)))
            (ps/claude--resync-window 'fake-window))
          (should (= sent-height 22))
          (should (= sent-width 77))
          (should redisplayed))
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

;;; Reflow throttling

(ert-deftest ps/claude-test-no-throttle-when-not-dragging ()
  "A settled resize always reflows, however recent the last one was."
  (let ((ps/claude--last-reflow-time (current-time))
        (track-mouse nil))
    (should-not (ps/claude--throttle-reflow-p (current-time)))))

(ert-deftest ps/claude-test-throttle-during-drag-within-interval ()
  "During a drag, a reflow inside the interval is skipped."
  (let ((ps/claude--last-reflow-time (current-time))
        (ps/claude-resize-throttle-interval 10)
        (track-mouse 'dragging))
    (should (ps/claude--throttle-reflow-p (current-time)))))

(ert-deftest ps/claude-test-no-throttle-during-drag-past-interval ()
  "During a drag, a reflow past the interval is allowed through."
  (let ((ps/claude--last-reflow-time (time-subtract (current-time) 5))
        (ps/claude-resize-throttle-interval 0.1)
        (track-mouse 'dragging))
    (should-not (ps/claude--throttle-reflow-p (current-time)))))

(ert-deftest ps/claude-test-no-throttle-on-first-reflow ()
  "With no recorded reflow yet, nothing is throttled."
  (let ((ps/claude--last-reflow-time nil)
        (track-mouse 'dragging))
    (should-not (ps/claude--throttle-reflow-p (current-time)))))

(ert-deftest ps/claude-test-throttle-advice-passes-through ()
  "The advice runs ORIG-FN and records the time when not throttled."
  (let ((ps/claude--last-reflow-time nil)
        (track-mouse nil)
        called)
    (should (eq (ps/claude--reflow-throttle-advice
                 (lambda (&rest _) (setq called t) 'size))
                'size))
    (should called)
    (should ps/claude--last-reflow-time)))

(ert-deftest ps/claude-test-throttle-advice-skips-during-drag ()
  "The advice returns nil without calling ORIG-FN while throttled."
  (let ((ps/claude--last-reflow-time (current-time))
        (ps/claude-resize-throttle-interval 10)
        (track-mouse 'dragging))
    (should-not (ps/claude--reflow-throttle-advice
                 (lambda (&rest _) (error "ORIG-FN should not be called"))))))

;;; Spinner display table

(ert-deftest ps/claude-test-spinner-display-table-maps-glyphs ()
  "Each configured character maps to its replacement's glyph code."
  (let ((ps/claude-spinner-glyph-replacements '((?\N{U+273B} . ?*))))
    (let ((table (ps/claude--make-spinner-display-table)))
      (should table)
      (should (equal (aref table ?\N{U+273B})
                     (vector (make-glyph-code ?*)))))))

(ert-deftest ps/claude-test-spinner-display-table-nil-when-disabled ()
  "No table is built when the replacement list is empty."
  (let ((ps/claude-spinner-glyph-replacements nil))
    (should-not (ps/claude--make-spinner-display-table))))

(ert-deftest ps/claude-test-spinner-display-table-installed-buffer-locally ()
  "Installing sets `buffer-display-table' in the current buffer only."
  (let ((ps/claude-spinner-glyph-replacements '((?\N{U+273B} . ?*))))
    (with-temp-buffer
      (ps/claude--install-spinner-display-table)
      (should (local-variable-p 'buffer-display-table))
      (should buffer-display-table))))

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

(provide 'test-ps-claude)
;;; test-ps-claude.el ends here
