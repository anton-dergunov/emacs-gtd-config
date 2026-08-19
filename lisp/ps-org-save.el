;;; ps-org-save.el --- Saving Org buffers without overwriting a newer file -*- lexical-binding: t; -*-

;;; Commentary:

;; Org buffers here are saved for you, from three places: an idle timer
;; (`auto-save-hook'), the moment Emacs loses focus -- which is how Claude Code
;; and any other outside editor come to see the latest text -- and just before
;; exit.  The background git sync saves through the same function before it
;; commits.  Between them you never press C-x C-s, and the mode line never
;; needs a "modified" flag.
;;
;; That is safe only as long as the buffer is the newest version of the file.
;; These files also arrive from elsewhere: Dropbox delivers an edit made on the
;; phone or on another laptop, and `global-auto-revert-mode' picks it up --
;; *unless* the buffer has unsaved changes of its own, which auto-revert quite
;; rightly refuses to discard.  The buffer and the file have then diverged, and
;; an unconditional save would write the older text over the newer one.  A
;; laptop that slept for a day and woke to a busy Dropbox is exactly that case.
;;
;; So a save skips a diverged buffer and says so, instead of overwriting it.
;; The report (`ps/org-save-show-stale') is where the divergence is resolved --
;; compare the two, keep yours, or take the file's.  It is deliberately *not* a
;; prompt: three of the four save paths run from a timer, where a yes-or-no
;; question arrives mid-keystroke, with no way to see what differs, and answers
;; itself wrongly.  The exception is quitting Emacs, where a skipped buffer is
;; simply left modified and Emacs's own "Save buffer?" question catches it --
;; the one moment a modal question is the right one.

;;; Code:

(require 'seq)
(require 'subr-x)

(declare-function ediff-current-file "ediff")
(declare-function org-id-locations-save "org-id")
(declare-function ps/window-show-here "ps-window")
(declare-function ps/mode-line--simple-view-render "ps-mode-line")

(defgroup ps-org-save nil
  "Saving Org buffers automatically."
  :group 'ps)

(defcustom ps/org-save-on-focus-loss-idle-threshold 300
  "Seconds since the last command above which a focus-loss save is skipped.
The org files here live on a Dropbox-backed CloudStorage mount; a save
triggered long after Emacs was last actually used (e.g. a focus event
generated around system sleep/wake) is exactly when that mount is most
likely to be slow to respond -- and is not a deliberate hand-off to another
app, since nothing has happened in Emacs for a while anyway.  Skipping it
there avoids blocking all of Emacs on that write; the next real save (the
periodic `auto-save-hook' one, or a later focus-loss save) still covers it."
  :type 'number
  :group 'ps-org-save)

(defcustom ps/org-save-report-buffer-name "*Org Files Out of Sync*"
  "Name of the buffer listing buffers that have diverged from their files."
  :type 'string
  :group 'ps-org-save)

;;; Divergence

(defun ps/org-save--stale-p (&optional buffer)
  "Return non-nil if BUFFER has unsaved edits *and* its file changed on disk.
Saving such a buffer would write the older text over the newer file.

Three conditions, all necessary.  An *unmodified* buffer whose file changed
is not stale -- that is `global-auto-revert-mode's job and it handles it
silently.  A buffer whose file no longer exists is not stale either: there
is nothing newer to lose, and writing it back is the useful thing to do.
That last condition mirrors `basic-save-buffer', which asks its \"changed
since visited\" question only when the file still exists."
  (with-current-buffer (or buffer (current-buffer))
    (and buffer-file-name
         (buffer-modified-p)
         (file-exists-p buffer-file-name)
         (not (verify-visited-file-modtime (current-buffer))))))

(defun ps/org-save-stale-buffers ()
  "Return the live Org buffers that have diverged from their files."
  (seq-filter (lambda (buffer)
                (and (buffer-live-p buffer)
                     (with-current-buffer buffer (derived-mode-p 'org-mode))
                     (ps/org-save--stale-p buffer)))
              (buffer-list)))

;;; Saving

(defvar ps/org-save--reported nil
  "File names of the diverged buffers the report last announced.
Compared as a set, so the report is raised once per *distinct* divergence
rather than on every tick of the idle timer.")

(defun ps/org-save-all-org-buffers-quietly ()
  "Save every Org buffer that is safe to save, and return the ones skipped.
Quiet because `org-save-all-org-buffers' echoes \"Saving all Org buffers...
done\", which would flash in the echo area and pile up in *Messages* on
every idle save and every 60-second sync.

The diverged buffers are collected first, in their own pass, so that
`ps/org-save--stale-p' stays a plain question about a buffer and this
function stays the only thing that acts on the answer."
  (let ((stale (ps/org-save-stale-buffers)))
    (let ((inhibit-message t) (message-log-max nil))
      (save-some-buffers
       t (lambda () (and (derived-mode-p 'org-mode)
                         (not (memq (current-buffer) stale))
                         t)))
      ;; What `org-save-all-org-buffers' does after its own save, and for the
      ;; same reason: the id locations index is only as good as its last write.
      (when (featurep 'org-id) (org-id-locations-save)))
    (ps/org-save--announce stale)
    stale))

(defun ps/org-save--announce (stale)
  "Raise the report for the diverged buffers STALE, once per distinct set.
A set that has not changed is already on screen (or has already been seen),
so re-raising it every few seconds would take the window away from whatever
the user moved on to."
  (let ((files (sort (mapcar #'buffer-file-name stale) #'string<)))
    (cond
     ((null files) (setq ps/org-save--reported nil))
     ((equal files ps/org-save--reported)
      ;; Same divergence as before: keep the report current if it is open,
      ;; but do not put it back on screen.
      (ps/org-save--refresh))
     (t
      (setq ps/org-save--reported files)
      (ps/org-save-show-stale)
      (message "%d Org file%s changed on disk while you had unsaved edits — not saved"
               (length files) (if (= (length files) 1) "" "s"))))))

;;; The report

(defvar ps/org-save-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d")   #'ps/org-save-compare)
    (define-key map (kbd "RET") #'ps/org-save-compare)
    (define-key map (kbd "m")   #'ps/org-save-keep-mine)
    (define-key map (kbd "r")   #'ps/org-save-take-disk)
    (define-key map (kbd "g")   #'ps/org-save-refresh)
    map)
  "Keymap for `ps-org-save-mode'.")

(define-derived-mode ps-org-save-mode special-mode "Out of Sync"
  "Major mode for the buffer listing Org files that diverged from disk.

\\{ps-org-save-mode-map}"
  (setq-local truncate-lines t)
  (when (fboundp 'ps/mode-line--simple-view-render)
    (setq-local mode-line-format
                '((:eval (ps/mode-line--simple-view-render "Out of Sync"))))))

(defun ps/org-save--time (time)
  "Format TIME for a report row.
The date is included when it is not today: a buffer can be based on a file
read days ago, and a bare clock time then reads as if it were this morning."
  (format-time-string
   (if (equal (format-time-string "%F" time) (format-time-string "%F"))
       "%H:%M:%S"
     "%F %H:%M")
   time))

(defun ps/org-save--render-row (buffer)
  "Insert the report row for BUFFER."
  (let* ((file (buffer-file-name buffer))
         (name (file-name-nondirectory file)))
    (insert "  ")
    (insert-text-button
     (format "%-32s" (truncate-string-to-width name 32))
     'face 'bold
     'mouse-face 'highlight
     'follow-link t
     'help-echo file
     'ps/org-save-buffer buffer
     ;; `push-button' does not move point on a mouse click, so the row has to
     ;; come from the button rather than from point -- otherwise clicking one
     ;; file compares another.  Point is moved too, so the single-key commands
     ;; then act on the row that was clicked.
     'action (lambda (button)
               (when-let* ((window (get-buffer-window (current-buffer))))
                 (select-window window))
               (goto-char (button-start button))
               (ps/org-save-compare)))
    (insert (format "your copy from %s · disk changed %s\n"
                    ;; `visited-file-modtime' has no buffer argument: it is
                    ;; always about the current one.
                    (ps/org-save--time
                     (with-current-buffer buffer (visited-file-modtime)))
                    (ps/org-save--time
                     (file-attribute-modification-time
                      (file-attributes file)))))))

(defun ps/org-save--render (stale)
  "Return the report text for the diverged buffers STALE."
  (with-temp-buffer
    (if (null stale)
        (insert "No Org file has diverged from its copy on disk.\n\n"
                "This report lists files that changed on disk (synced from\n"
                "another device) while you had unsaved edits in Emacs.  They\n"
                "are never saved over automatically.\n")
      (insert (format "%d Org file%s changed on disk while you had unsaved edits.\n"
                      (length stale) (if (= (length stale) 1) "" "s"))
              "They were NOT saved — your edits and the newer file both still exist.\n\n"
              "  d / RET  compare the two      m  keep mine (overwrite the file)\n"
              "  r  take the disk version      g  refresh\n\n")
      (dolist (buffer stale) (ps/org-save--render-row buffer)))
    (buffer-string)))

(defun ps/org-save--buffer ()
  "Return the report buffer, refreshed, creating it if needed.
Point keeps its line across a refresh, so resolving one file does not throw
away the reader's place in a list of several."
  (with-current-buffer (get-buffer-create ps/org-save-report-buffer-name)
    (unless (derived-mode-p 'ps-org-save-mode) (ps-org-save-mode))
    (let ((inhibit-read-only t)
          (line (line-number-at-pos)))
      (erase-buffer)
      (insert (ps/org-save--render (ps/org-save-stale-buffers)))
      (goto-char (point-min))
      (forward-line (1- line)))
    (current-buffer)))

(defun ps/org-save--refresh ()
  "Refresh the report if it is on screen, without displaying it."
  (when (get-buffer-window ps/org-save-report-buffer-name t)
    (ps/org-save--buffer)))

;;;###autoload
(defun ps/org-save-show-stale ()
  "Show the Org buffers that have diverged from their files on disk."
  (interactive)
  (let ((buffer (ps/org-save--buffer)))
    (if (fboundp 'ps/window-show-here)
        (ps/window-show-here buffer)
      (display-buffer buffer))))

(defun ps/org-save-refresh ()
  "Rebuild the report from the current state of the buffers."
  (interactive)
  (ps/org-save--buffer))

(defun ps/org-save--buffer-at-point ()
  "Return the buffer named by the report row at point, or signal."
  (or (get-text-property (point) 'ps/org-save-buffer)
      ;; Point is somewhere else on the row -- past the name, or on the
      ;; leading indent -- so look along the line for the button.
      (save-excursion
        (let ((end (line-end-position)))
          (beginning-of-line)
          (when-let* ((pos (text-property-not-all
                            (point) end 'ps/org-save-buffer nil)))
            (get-text-property pos 'ps/org-save-buffer))))
      (user-error "Point is not on a file")))

(defun ps/org-save-compare ()
  "Compare the buffer at point with the file on disk, with ediff."
  (interactive)
  (let ((buffer (ps/org-save--buffer-at-point)))
    (require 'ediff)
    (with-current-buffer buffer (ediff-current-file))))

(defun ps/org-save-keep-mine ()
  "Save the buffer at point over the newer file, discarding what is on disk."
  (interactive)
  (let* ((buffer (ps/org-save--buffer-at-point))
         (name (file-name-nondirectory (buffer-file-name buffer))))
    (when (yes-or-no-p
           (format "Overwrite %s on disk with your unsaved version? " name))
      (with-current-buffer buffer
        ;; Accept the file as it stands as this buffer's base, so `save-buffer'
        ;; has nothing left to object to, then write over it.
        (set-visited-file-modtime)
        (save-buffer))
      (ps/org-save-refresh))))

(defun ps/org-save-take-disk ()
  "Reload the buffer at point from disk, discarding your unsaved edits."
  (interactive)
  (let* ((buffer (ps/org-save--buffer-at-point))
         (name (file-name-nondirectory (buffer-file-name buffer))))
    (when (yes-or-no-p
           (format "Discard your unsaved edits to %s and reload it? " name))
      (with-current-buffer buffer (revert-buffer t t))
      (ps/org-save-refresh))))

;;; Saving on focus loss

(defvar ps/org-save--last-command-time (float-time)
  "`float-time' of the most recent command Emacs processed.
Compared against `ps/org-save-on-focus-loss-idle-threshold' by
`ps/org-save-on-focus-loss' -- more direct than `current-idle-time', which
is not reliably meaningful from inside a focus-change callback.")

(defun ps/org-save--note-command ()
  "Record that a command has just run."
  (setq ps/org-save--last-command-time (float-time)))

(defun ps/org-save--any-org-buffer-modified-p ()
  "Non-nil if any live Org buffer has unsaved changes."
  (seq-some (lambda (b)
              (and (buffer-modified-p b)
                   (with-current-buffer b (derived-mode-p 'org-mode))))
            (buffer-list)))

(defun ps/org-save-on-focus-loss ()
  "Save all Org buffers when Emacs loses focus.
Skipped outright when nothing is modified, or when Emacs has been idle
longer than `ps/org-save-on-focus-loss-idle-threshold' (see its docstring).
The save itself is bounded by a timeout and never lets an error escape, so a
slow or failing save cannot take the rest of `after-focus-change-function'
-- e.g. `ps/scrollbar--on-focus-change' -- down with it."
  (unless (frame-focus-state)
    (when (and (ps/org-save--any-org-buffer-modified-p)
               (< (- (float-time) ps/org-save--last-command-time)
                  ps/org-save-on-focus-loss-idle-threshold))
      (with-timeout (5 (message "ps/org-save-on-focus-loss: timed out, skipped"))
        (condition-case err
            (ps/org-save-all-org-buffers-quietly)
          (error (message "ps/org-save-on-focus-loss: %S" err)))))))

(defun ps/org-save--before-kill-emacs (&rest _)
  "Save Org buffers on the way out.
A diverged buffer is skipped and stays modified, so `save-buffers-kill-emacs'
asks about it itself -- the one save path where a question is welcome."
  (ps/org-save-all-org-buffers-quietly))

;;; Setup

(defun ps/org-save-setup ()
  "Wire up automatic saving of Org buffers.
Obsidian-like persistence: the idle save keeps files current while editing,
and the other two close the remaining gaps -- a save the moment Emacs loses
focus (which is also how Claude Code picks up the latest file content), and
one just before exit, so quitting never prompts \"Save buffer?\"."
  (add-hook 'post-command-hook #'ps/org-save--note-command)
  (add-hook 'auto-save-hook #'ps/org-save-all-org-buffers-quietly)
  (add-function :after after-focus-change-function #'ps/org-save-on-focus-loss)
  (advice-add 'save-buffers-kill-emacs :before #'ps/org-save--before-kill-emacs))

(provide 'ps-org-save)
;;; ps-org-save.el ends here
