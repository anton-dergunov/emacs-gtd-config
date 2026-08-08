;;; ps-blank-lines-review.el --- Review and apply blank-line proposals -*- lexical-binding: t; -*-

;;; Commentary:

;; The writing half of blank-line recovery.  `ps-blank-lines.el' finds what
;; could be restored; this walks the proposals one file at a time through
;; Ediff, and saves a file only when the review actually changed it.
;;
;; Buffer A is your file as it stands, **read-only**; buffer B is the proposal,
;; editable, and B is what gets written.  That is the diff-review convention —
;; left is what you have, right is what you would end up with — and it means
;; the review starts from *everything accepted*, which is the right default for
;; blank lines you once typed and a phone deleted.  Ediff's own `a' therefore
;; reads as "reject this change": it copies your file's version of the region
;; back into the proposal.  `rb' undoes a rejection.
;;
;; `b' is disabled.  It would copy into the read-only reference, which changes
;; nothing that gets saved while making the difference vanish as though it had
;; been decided.  Ediff's copy commands bind `inhibit-read-only', so
;; `buffer-read-only' on A cannot stop `b' by itself — the key has to go.
;;
;; Three keys are added to the session's keymap (`ediff-mode-map' is
;; `ediff-defvar-local', so none of this leaks into an unrelated Ediff): `+'
;; accepts everything again, `x' rejects everything, `Q' abandons the queue.
;; All three come from what stock Ediff leaves free — `!' is not free, it is
;; `ediff-update-diffs'.
;;
;; Three guards stand between a review and the disk, in the order they fire:
;;
;;   - A file whose buffer is modified, or whose text no longer matches what
;;     the scan read, is skipped rather than reviewed.  The proposal was
;;     computed against a specific version, and applying it to a newer one
;;     would revert whatever came in between.
;;   - Nothing is written when the proposal ends up identical to the file, so
;;     rejecting everything is a no-op rather than a rewrite.
;;   - Before saving, the proposal is checked against the scanned text with
;;     blank lines removed.  Equal means only blank lines changed, which is the
;;     whole promise of the feature; unequal means the review edited content,
;;     and that is never saved silently.
;;
;; The review takes over the content area of the current frame rather than
;; opening frames of its own — see the Layout section for why that needs global
;; variables held for the length of the session.
;;
;; The Ediff exit path is load-bearing and was verified rather than assumed.
;; `ediff-really-quit' runs `ediff-cleanup-hook', then `ediff-janitor', then
;; `ediff-quit-hook' (whose global value is `ediff-cleanup-mess'), and finally
;; the *buffer-local* part of `ediff-after-quit-hook-internal'.  A hook added
;; with `add-hook ... nil t' lands in the local value ahead of the `t' that
;; stands for the global one, so the save below runs while buffers A and B and
;; the control buffer are all still alive.  Advancing to the next file has to
;; wait for `ediff-after-quit-hook-internal', by which point the session is
;; fully torn down and a new one can start.  `ediff-keep-variants' is set
;; buffer-locally in the control buffer because `ediff-janitor' reads it there,
;; and without it a user who has customized it away from t gets a prompt about
;; killing their own file buffer between those two hooks.
;;
;; See `design-docs/blank-line-recovery.md'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ediff)
(require 'ps-blank-lines-tree)
(require 'ps-file-tree)

(declare-function ps/blank-lines-result-file "ps-blank-lines")
(declare-function ps/blank-lines-result-relpath "ps-blank-lines")
(declare-function ps/blank-lines-result-restored "ps-blank-lines")
(declare-function ps/blank-lines-result-removed "ps-blank-lines")
(declare-function ps/blank-lines-result-scanned "ps-blank-lines")
(declare-function ps/blank-lines-result-proposed "ps-blank-lines")
(declare-function ps/blank-lines-result-error "ps-blank-lines")

;;; Settings

(defcustom ps/blank-lines-review-side-by-side t
  "Non-nil shows your file and the proposal in side-by-side columns.
Nil stacks them, which is Ediff's own default."
  :type 'boolean
  :group 'ps-blank-lines)

;;; Session state

(defvar ps/blank-lines-review--queue nil
  "Results still to be reviewed in the current session.")
(defvar ps/blank-lines-review--applied nil
  "Alist of (RELPATH . BLANK-LINE-DELTA) for files saved in this session.")
(defvar ps/blank-lines-review--skipped nil
  "Alist of (RELPATH . REASON) for files this session did not write.")
(defvar ps/blank-lines-review--on-finish nil
  "Function called with no arguments when the queue drains.")

(defvar ps/blank-lines-review--saved nil
  "Plist of layout state to put back when the review ends.")

(defvar ps/blank-lines-review--reviewing nil
  "Plist for the file on screen: :current and its :read-only flag before us.")

(defun ps/blank-lines-review--release-file ()
  "Give the reviewed file back its own read-only flag.

Ediff does not do this for us — `buffer-read-only' is commented out of
`ediff-protected-variables', so a session that sets it has to clear it, or the
user is left with a file buffer they cannot type in."
  (when-let* ((session ps/blank-lines-review--reviewing)
              (current (plist-get session :current)))
    (when (buffer-live-p current)
      (with-current-buffer current
        (setq buffer-read-only (plist-get session :read-only)))))
  (setq ps/blank-lines-review--reviewing nil))

(defun ps/blank-lines-review-in-progress-p ()
  "Return non-nil while a review session still has files queued."
  (and ps/blank-lines-review--queue t))

;;; Layout

;; Ediff defaults to a control panel in its own frame on a graphical display,
;; and to stacking A above B.  A review wants neither: it should take over the
;; content area of the frame the user is already in, with the two versions side
;; by side.  Both variables have to be set *globally* for the duration —
;; `ediff-setup-windows' reads the setup function out of the control buffer,
;; but the first layout runs inside `ediff-buffers', before that buffer is ours
;; to configure.  They are restored when the queue drains.
;;
;; The file tree is hidden first because `ediff-setup-windows-plain-compare'
;; calls `delete-other-windows': hiding it gives Ediff the whole content area.
;;
;; The report stays put in a *side* window — the one kind
;; `delete-other-windows' leaves alone, so it survives every session in the
;; queue without a custom window-setup function.  Two window parameters carry
;; that: `no-delete-other-windows', and `no-other-window' so Ediff's
;; `(other-window 1)' cannot land buffer A in the report.  A main window has to
;; be selected before Ediff runs, because `delete-other-windows' signals an
;; error when the selected window is a side window.
;;
;; It goes on the *right*, where the report already was: the file tree owns the
;; left, so the diff opening on the left is the diff taking the tree's place
;; rather than the report's.

(defconst ps/blank-lines-review--report-window
  '((display-buffer-in-side-window)
    (side . right)
    (slot . 0)
    (window-width . 0.32)
    (window-parameters . ((no-other-window . t)
                          (no-delete-other-windows . t))))
  "How the report is displayed beside a review.")

(defun ps/blank-lines-review--keep-visible (buffer)
  "Put BUFFER in a side window that will survive Ediff; return that window.
Leaves a main window selected.  BUFFER may also still be showing in a main
window; Ediff takes that one over, which is what resolves the duplicate."
  (when (buffer-live-p buffer)
    (let ((side (display-buffer buffer ps/blank-lines-review--report-window)))
      (when (window-live-p side)
        (when-let* ((main (seq-find (lambda (window)
                                      (not (window-parameter window 'window-side)))
                                    (window-list))))
          (select-window main))
        side))))

(defun ps/blank-lines-review--enter-layout (&optional keep-visible)
  "Give the content area over to Ediff, remembering what to put back.
KEEP-VISIBLE, if given, is a buffer to park beside the review."
  (setq ps/blank-lines-review--saved
        (list :window-config (current-window-configuration)
              :file-tree (and (ps/file-tree-window-exists-p) t)
              :setup ediff-window-setup-function
              :split ediff-split-window-function))
  (ps/file-tree-hide)
  (ps/blank-lines-review--keep-visible keep-visible)
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function (if ps/blank-lines-review-side-by-side
                                        #'split-window-horizontally
                                      #'split-window-vertically)))

(defun ps/blank-lines-review--exit-layout ()
  "Put back what `ps/blank-lines-review--enter-layout' took."
  (when-let* ((saved ps/blank-lines-review--saved))
    (setq ps/blank-lines-review--saved nil)
    (setq ediff-window-setup-function (plist-get saved :setup)
          ediff-split-window-function (plist-get saved :split))
    (when (window-configuration-p (plist-get saved :window-config))
      (set-window-configuration (plist-get saved :window-config)))
    (when (plist-get saved :file-tree)
      (ps/file-tree-show))))

;;; Pure helpers

(defun ps/blank-lines-review-blank-count (text)
  "Return the number of blank lines in TEXT.
Blank means the same thing it means to the parser: empty, or whitespace only.
A final newline terminates the last line rather than starting an empty one,
so it is not counted — otherwise every file would report one blank too many,
and a file without a trailing newline would report one too few."
  (let ((lines (split-string (or text "") "\n")))
    (when (and (cdr lines) (equal "" (car (last lines))))
      (setq lines (butlast lines)))
    (seq-count #'ps/blank-lines-blank-p lines)))

(defun ps/blank-lines-review-actionable-p (result)
  "Return non-nil when RESULT is a proposal a review could apply."
  (and (null (ps/blank-lines-result-error result))
       (stringp (ps/blank-lines-result-proposed result))
       (stringp (ps/blank-lines-result-scanned result))
       (not (equal (ps/blank-lines-result-proposed result)
                   (ps/blank-lines-result-scanned result)))))

(defun ps/blank-lines-review-safe-to-save-p (scanned current)
  "Return non-nil when CURRENT differs from SCANNED only in blank lines."
  (and (stringp scanned)
       (stringp current)
       (ps/blank-lines-strip-equal-p scanned current)))

(defun ps/blank-lines-review-staleness (result buffer)
  "Return a reason string when BUFFER is not what RESULT was computed against.
Return nil when it is safe to review."
  (cond
   ((not (buffer-live-p buffer)) "the file could not be opened")
   ((buffer-modified-p buffer) "the buffer has unsaved changes")
   ((not (equal (with-current-buffer buffer
                  (buffer-substring-no-properties (point-min) (point-max)))
                (ps/blank-lines-result-scanned result)))
    "the file changed since the scan")
   (t nil)))

;;; Applying without a review

(defun ps/blank-lines-review-apply (result)
  "Write RESULT's proposal to its file, with no Ediff session.

For accepting a whole file from the report, where the proposal has already
been read there.  Returns (t . BLANK-LINES-RESTORED) or (nil . REASON); the
same staleness and blank-lines-only guards apply as in a review."
  (let* ((file (ps/blank-lines-result-file result))
         (buffer (find-file-noselect file))
         (scanned (ps/blank-lines-result-scanned result))
         (proposed (ps/blank-lines-result-proposed result))
         (stale (ps/blank-lines-review-staleness result buffer)))
    (cond
     (stale (cons nil stale))
     ((not (ps/blank-lines-review-safe-to-save-p scanned proposed))
      ;; The engine already asserts this, but nothing else stands between here
      ;; and a write that no human looked at.
      (cons nil "the proposal changes text, not only blank lines"))
     (t
      (let ((source (generate-new-buffer " *ps-blank-lines-apply*")))
        (unwind-protect
            (progn
              (with-current-buffer source (insert proposed))
              (with-current-buffer buffer
                ;; `replace-buffer-contents' rather than erase-and-insert: it
                ;; keeps point, markers and folding, and touches only the lines
                ;; that actually differ.
                (replace-buffer-contents source)
                (save-buffer)))
          (kill-buffer source)))
      (cons t (- (ps/blank-lines-review-blank-count proposed)
                 (ps/blank-lines-review-blank-count scanned)))))))

;;; The loop

(defun ps/blank-lines-review--skip (relpath reason)
  "Record that RELPATH was not written, because REASON."
  (push (cons relpath reason) ps/blank-lines-review--skipped))

(defun ps/blank-lines-review-start (results &optional on-finish keep-visible)
  "Review RESULTS one file at a time with Ediff, saving what is accepted.

RESULTS is a list of `ps/blank-lines-result'; those with nothing to apply are
dropped.  ON-FINISH, if given, is called once the last file has been reviewed,
with an alist of (RELPATH . BLANK-LINES-RESTORED) for what was written and an
alist of (RELPATH . REASON) for what was not.  KEEP-VISIBLE, if given, is a
buffer to keep on screen beside the review.  Returns the number queued."
  (let ((queue (seq-filter #'ps/blank-lines-review-actionable-p results)))
    (setq ps/blank-lines-review--queue queue
          ps/blank-lines-review--applied nil
          ps/blank-lines-review--skipped nil
          ps/blank-lines-review--on-finish on-finish)
    (if (null queue)
        (progn (message "Nothing to review.") 0)
      (ps/blank-lines-review--enter-layout keep-visible)
      (ps/blank-lines-review--next)
      (length queue))))

(defun ps/blank-lines-review--next ()
  "Review the next queued file, or finish when the queue is empty."
  (let ((result (pop ps/blank-lines-review--queue)))
    (if (null result)
        (ps/blank-lines-review--finish)
      ;; A file that cannot be reviewed must not strand the queue, so failures
      ;; here are recorded and the loop moves on.
      (condition-case err
          (ps/blank-lines-review--session result)
        (error
         (ps/blank-lines-review--skip (ps/blank-lines-result-relpath result)
                                      (error-message-string err))
         (ps/blank-lines-review--next))))))

(defun ps/blank-lines-review--proposed-buffer (result)
  "Return a buffer holding RESULT's proposed text, in `org-mode'.

`org-mode' runs with its hooks, not under `delay-mode-hooks', so the proposal
is fontified and prettified the same way the file beside it is — a review is a
comparison, and the two sides have to be comparable.  Startup visibility is
inhibited so the buffer opens unfolded: a folded proposal hides the very lines
under review."
  (let ((buffer (get-buffer-create
                 (format "*proposed: %s*"
                         (file-name-nondirectory
                          (ps/blank-lines-result-file result))))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (org-inhibit-startup t))
        (erase-buffer)
        (insert (ps/blank-lines-result-proposed result))
        (org-mode)
        (font-lock-ensure)
        (goto-char (point-min))
        (set-buffer-modified-p nil)))
    buffer))

(defun ps/blank-lines-review--session (result)
  "Start an Ediff session for RESULT, or skip it and move on."
  (let* ((file (ps/blank-lines-result-file result))
         (relpath (ps/blank-lines-result-relpath result))
         (current (find-file-noselect file))
         (stale (ps/blank-lines-review-staleness result current)))
    (if stale
        (progn (ps/blank-lines-review--skip relpath stale)
               (ps/blank-lines-review--next))
      (let* ((proposed (ps/blank-lines-review--proposed-buffer result))
             (control (ediff-buffers current proposed)))
        (unless (buffer-live-p control)
          (kill-buffer proposed)
          (error "Ediff did not return a control buffer"))
        ;; Your file is the reference, not the workspace.  Ediff will not put
        ;; this flag back, so the session records what it was and clears it in
        ;; `ps/blank-lines-review--release-file'.
        (with-current-buffer current
          (setq ps/blank-lines-review--reviewing
                (list :current current :read-only buffer-read-only))
          (setq buffer-read-only t))
        (with-current-buffer control
          (setq-local ediff-keep-variants t)
          (define-key ediff-mode-map "+" #'ps/blank-lines-review-accept-all)
          (define-key ediff-mode-map "x" #'ps/blank-lines-review-reject-all)
          (define-key ediff-mode-map "Q" #'ps/blank-lines-review-abort)
          (define-key ediff-mode-map "b" #'ps/blank-lines-review--no-copy-to-file)
          (add-hook 'ediff-quit-hook
                    (lambda ()
                      (ps/blank-lines-review--commit result current proposed))
                    nil t)
          (setq-local ediff-after-quit-hook-internal
                      (list #'ps/blank-lines-review--next))
          ;; Ediff starts with no difference selected (`ediff-current-difference'
          ;; is -1), and `a' refuses to act until one is — "Bad diff region
          ;; number, 0".  Select the first, so the session opens ready to act.
          (when (> ediff-number-of-differences 0)
            (ediff-jump-to-difference 1)))
        (message "%s: all accepted — %s reject one, %s undo, %s reject all, %s keep and go on"
                 relpath
                 (propertize "a" 'face 'help-key-binding)
                 (propertize "rb" 'face 'help-key-binding)
                 (propertize "x" 'face 'help-key-binding)
                 (propertize "q" 'face 'help-key-binding))))))

(defun ps/blank-lines-review--no-copy-to-file ()
  "Refuse Ediff's `b', which would edit the read-only reference.

The left side is your file as it stands; the right side is what will be
written.  Copying leftwards changes nothing that gets saved, while making the
difference disappear as though it had been decided."
  (interactive)
  (message "The left side is read-only.  %s rejects a change, %s undoes that"
           (propertize "a" 'face 'help-key-binding)
           (propertize "rb" 'face 'help-key-binding)))

(defun ps/blank-lines-review-accept-all ()
  "Take back every rejection, so the whole proposal is accepted again."
  (interactive)
  (ediff-barf-if-not-control-buffer)
  ;; `ediff-pop-diff' restores what `a' saved; regions never rejected have
  ;; nothing saved and simply signal, which is the no-op we want.
  (dotimes (i ediff-number-of-differences)
    (ignore-errors (ediff-pop-diff i 'B)))
  (message "Whole file accepted — %s to keep it and go on"
           (propertize "q" 'face 'help-key-binding)))

(defun ps/blank-lines-review-reject-all ()
  "Reject every change, leaving this file exactly as it is."
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (let ((n ediff-number-of-differences))
    (dotimes (i n)
      (ediff-copy-diff i 'A 'B))
    (message "All %d change(s) rejected — %s to leave the file alone, %s to undo"
             n
             (propertize "q" 'face 'help-key-binding)
             (propertize "+" 'face 'help-key-binding))))

(defun ps/blank-lines-review-abort ()
  "Finish reviewing this file and drop the rest of the queue."
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (setq ps/blank-lines-review--queue nil)
  (call-interactively #'ediff-quit))

(defun ps/blank-lines-review--control-buffer ()
  "Return the live Ediff control buffer of a review in progress, if any."
  (seq-find (lambda (buffer)
              (with-current-buffer buffer
                (and (eq major-mode 'ediff-mode)
                     (buffer-live-p (bound-and-true-p ediff-buffer-A)))))
            (buffer-list)))

(defun ps/blank-lines-review-cancel ()
  "Abandon a review in progress, writing nothing, and restore the layout.

For deciding a file from the report instead: the diff on screen describes a
question that has just been answered elsewhere, so leaving it up would show a
stale comparison.  Nothing needs undoing in the file — under this model the
review only ever edits the proposal, which is discarded."
  (interactive)
  (setq ps/blank-lines-review--queue nil)
  (ps/blank-lines-review--release-file)
  (when-let* ((control (ps/blank-lines-review--control-buffer)))
    (with-current-buffer control
      ;; Drop our hooks first: quitting must neither save nor advance.
      (setq-local ediff-quit-hook (default-value 'ediff-quit-hook))
      (setq-local ediff-after-quit-hook-internal nil)
      (let ((proposed ediff-buffer-B))
        (ediff-really-quit nil)
        (when (buffer-live-p proposed) (kill-buffer proposed)))))
  (setq ps/blank-lines-review--applied nil
        ps/blank-lines-review--skipped nil
        ps/blank-lines-review--on-finish nil)
  (ps/blank-lines-review--exit-layout))

(defun ps/blank-lines-review--write (current text scanned relpath)
  "Put TEXT into CURRENT and save it, recording RELPATH as applied."
  (let ((source (generate-new-buffer " *ps-blank-lines-commit*")))
    (unwind-protect
        (progn
          (with-current-buffer source (insert text))
          (with-current-buffer current
            (let ((inhibit-read-only t))
              (replace-buffer-contents source))
            (save-buffer)))
      (kill-buffer source)))
  (push (cons relpath (- (ps/blank-lines-review-blank-count text)
                         (ps/blank-lines-review-blank-count scanned)))
        ps/blank-lines-review--applied))

(defun ps/blank-lines-review--commit (result current proposed)
  "Write what the review left in PROPOSED into CURRENT, if anything changed.
RESULT is the proposal under review."
  (let ((relpath (ps/blank-lines-result-relpath result))
        (scanned (ps/blank-lines-result-scanned result))
        (text (and (buffer-live-p proposed)
                   (with-current-buffer proposed
                     (buffer-substring-no-properties (point-min) (point-max))))))
    (ps/blank-lines-review--release-file)
    (when (buffer-live-p proposed)
      (kill-buffer proposed))
    (cond
     ((null text)
      (ps/blank-lines-review--skip relpath "the proposal was closed during review"))
     ((not (buffer-live-p current))
      (ps/blank-lines-review--skip relpath "the buffer was closed during review"))
     ;; Every change rejected — leave the file alone rather than rewrite it
     ;; with its own contents.
     ((equal text scanned)
      (ps/blank-lines-review--skip relpath "nothing accepted"))
     ((ps/blank-lines-review-safe-to-save-p scanned text)
      (ps/blank-lines-review--write current text scanned relpath))
     ;; The review edited text, not just blank lines.  Recovery promises never
     ;; to do that, so this can only be the user's own typing, and only the
     ;; user can approve writing it.
     ((yes-or-no-p
       (format "%s: the review changed text, not only blank lines.  Save anyway? "
               relpath))
      (ps/blank-lines-review--write current text scanned relpath))
     (t
      (ps/blank-lines-review--skip
       relpath "left unsaved: the review changed text, not only blank lines")))))

(defun ps/blank-lines-review--finish ()
  "Report what the finished session wrote, and hand back to ON-FINISH."
  (let ((applied (nreverse ps/blank-lines-review--applied))
        (skipped (nreverse ps/blank-lines-review--skipped))
        (on-finish ps/blank-lines-review--on-finish))
    (setq ps/blank-lines-review--applied nil
          ps/blank-lines-review--skipped nil
          ps/blank-lines-review--on-finish nil)
    (ps/blank-lines-review--exit-layout)
    (message "%s"
             (concat
              (if applied
                  (format "Recovered %d blank line(s) in %d file(s)"
                          (apply #'+ 0 (mapcar #'cdr applied)) (length applied))
                "Nothing written")
              (when-let* ((notable (seq-remove
                                    (lambda (s) (equal (cdr s) "nothing accepted"))
                                    skipped)))
                (format "; skipped %s"
                        (mapconcat (lambda (s) (format "%s (%s)" (car s) (cdr s)))
                                   notable ", ")))
              "."))
    (when on-finish
      (funcall on-finish applied skipped))))

(provide 'ps-blank-lines-review)
;;; ps-blank-lines-review.el ends here
