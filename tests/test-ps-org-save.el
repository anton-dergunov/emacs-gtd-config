;;; test-ps-org-save.el --- ERT tests for ps-org-save -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path "lisp")
(require 'ps-org-save)
(require 'org)

;;; Test helpers

(defmacro ps/org-save-test--with-file (var contents &rest body)
  "Run BODY with VAR bound to a buffer visiting a temp Org file holding CONTENTS."
  (declare (indent 2))
  `(let* ((file (make-temp-file "ps-org-save-test-" nil ".org" ,contents))
          (,var (find-file-noselect file)))
     (unwind-protect
         (progn ,@body)
       (when (buffer-live-p ,var)
         (with-current-buffer ,var (set-buffer-modified-p nil))
         (kill-buffer ,var))
       (delete-file file))))

(defun ps/org-save-test--edit (buffer text)
  "Type TEXT into BUFFER without saving."
  (with-current-buffer buffer
    (goto-char (point-max))
    (insert text)))

(defun ps/org-save-test--change-on-disk (buffer text)
  "Append TEXT to BUFFER's file behind Emacs's back, as Dropbox would.
The modification time is pushed a second into the future as well: the file
may well be written inside the same second Emacs read it, and then the
modtime comparison cannot see the change at all."
  (let ((file (buffer-file-name buffer)))
    (write-region text nil file t 'quiet)
    (set-file-times file (time-add (current-time) 1))))

(defmacro ps/org-save-test--quietly (&rest body)
  "Run BODY with the report suppressed and messages collected in `msgs'."
  (declare (indent 0))
  `(let ((msgs nil))
     (cl-letf (((symbol-function 'ps/org-save-show-stale) (lambda () nil))
               ((symbol-function 'message)
                (lambda (fmt &rest args) (push (apply #'format fmt args) msgs))))
       ,@body)))

;;; -------------------------------------------------------
;;; Divergence detection
;;; -------------------------------------------------------

(ert-deftest ps/org-save--fresh-edits-are-not-stale ()
  "The ordinary case: you typed, nothing else touched the file."
  (ps/org-save-test--with-file buffer "* One\n"
    (ps/org-save-test--edit buffer "* Two\n")
    (should (buffer-modified-p buffer))
    (should-not (ps/org-save--stale-p buffer))))

(ert-deftest ps/org-save--edits-plus-a-changed-file-are-stale ()
  "Both sides moved: this is the divergence that must never be saved over."
  (ps/org-save-test--with-file buffer "* One\n"
    (ps/org-save-test--edit buffer "* Mine\n")
    (ps/org-save-test--change-on-disk buffer "* Theirs\n")
    (should (ps/org-save--stale-p buffer))
    (should (member buffer (ps/org-save-stale-buffers)))))

(ert-deftest ps/org-save--an-unmodified-buffer-is-never-stale ()
  "A file that changed under an unmodified buffer is auto-revert's business."
  (ps/org-save-test--with-file buffer "* One\n"
    (ps/org-save-test--change-on-disk buffer "* Theirs\n")
    (should-not (buffer-modified-p buffer))
    (should-not (ps/org-save--stale-p buffer))))

(ert-deftest ps/org-save--a-deleted-file-is-not-stale ()
  "Nothing newer exists to lose, so writing the buffer back is the useful act.
This mirrors `basic-save-buffer', which asks its \"changed since visited\"
question only when the file is still there."
  (ps/org-save-test--with-file buffer "* One\n"
    (ps/org-save-test--edit buffer "* Mine\n")
    (let ((file (buffer-file-name buffer)))
      (delete-file file)
      (should-not (ps/org-save--stale-p buffer))
      ;; Put it back so the fixture's cleanup has something to delete.
      (write-region "" nil file nil 'quiet))))

;;; -------------------------------------------------------
;;; Saving
;;; -------------------------------------------------------

(ert-deftest ps/org-save--quiet-save-writes-a-fresh-buffer ()
  "An ordinary modified buffer is saved, as it always was."
  (ps/org-save-test--with-file buffer "* One\n"
    (ps/org-save-test--edit buffer "* Two\n")
    (ps/org-save-test--quietly (ps/org-save-all-org-buffers-quietly))
    (should-not (buffer-modified-p buffer))
    (should (equal (with-temp-buffer
                     (insert-file-contents (buffer-file-name buffer))
                     (buffer-string))
                   "* One\n* Two\n"))))

(ert-deftest ps/org-save--quiet-save-leaves-a-diverged-file-alone ()
  "The whole point: the newer file survives and the edits survive with it."
  (ps/org-save-test--with-file buffer "* One\n"
    (ps/org-save-test--edit buffer "* Mine\n")
    (ps/org-save-test--change-on-disk buffer "* Theirs\n")
    (let (stale)
      (ps/org-save-test--quietly
        (setq stale (ps/org-save-all-org-buffers-quietly)))
      (should (equal stale (list buffer))))
    ;; The buffer keeps its edits...
    (should (buffer-modified-p buffer))
    (should (string-match-p "Mine" (with-current-buffer buffer (buffer-string))))
    ;; ...and the file keeps what arrived from the other machine.
    (should (equal (with-temp-buffer
                     (insert-file-contents (buffer-file-name buffer))
                     (buffer-string))
                   "* One\n* Theirs\n"))))

(ert-deftest ps/org-save--a-diverged-buffer-does-not-block-the-others ()
  "One file out of sync must not stop the rest of the vault being saved."
  (ps/org-save-test--with-file diverged "* One\n"
    (ps/org-save-test--with-file ordinary "* One\n"
      (ps/org-save-test--edit diverged "* Mine\n")
      (ps/org-save-test--change-on-disk diverged "* Theirs\n")
      (ps/org-save-test--edit ordinary "* Two\n")
      (ps/org-save-test--quietly (ps/org-save-all-org-buffers-quietly))
      (should (buffer-modified-p diverged))
      (should-not (buffer-modified-p ordinary)))))

;;; -------------------------------------------------------
;;; Announcing
;;; -------------------------------------------------------

(ert-deftest ps/org-save--divergence-is-announced-once ()
  "The idle timer fires every few seconds; the report is raised once per set."
  (ps/org-save-test--with-file buffer "* One\n"
    (let ((ps/org-save--reported nil)
          (shown 0))
      (cl-letf (((symbol-function 'ps/org-save-show-stale)
                 (lambda () (setq shown (1+ shown))))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (ps/org-save-test--edit buffer "* Mine\n")
        (ps/org-save-test--change-on-disk buffer "* Theirs\n")
        (ps/org-save-all-org-buffers-quietly)
        (should (= shown 1))
        (ps/org-save-all-org-buffers-quietly)
        (should (= shown 1))))))

(ert-deftest ps/org-save--a-resolved-divergence-can-be-announced-again ()
  "Once the set empties, the next divergence is news again."
  (ps/org-save-test--with-file buffer "* One\n"
    (let ((ps/org-save--reported '("/some/old/file.org")))
      (ps/org-save-test--quietly (ps/org-save--announce nil))
      (should-not ps/org-save--reported))))

;;; -------------------------------------------------------
;;; The report
;;; -------------------------------------------------------

(ert-deftest ps/org-save--report-names-the-files ()
  "Each diverged file is a row, with both timestamps."
  (ps/org-save-test--with-file buffer "* One\n"
    (ps/org-save-test--edit buffer "* Mine\n")
    (ps/org-save-test--change-on-disk buffer "* Theirs\n")
    (let ((text (ps/org-save--render (list buffer))))
      (should (string-match-p
               (regexp-quote (file-name-nondirectory (buffer-file-name buffer)))
               text))
      (should (string-match-p "your copy from" text))
      (should (string-match-p "disk changed" text))
      (should (string-match-p "NOT saved" text)))))

(ert-deftest ps/org-save--report-is-honest-when-empty ()
  "An empty report says so rather than showing a bare heading."
  (should (string-match-p "No Org file has diverged" (ps/org-save--render nil))))

(ert-deftest ps/org-save--report-rows-carry-their-buffer ()
  "A row's command must find its file from the row, not from point's line
number -- a mouse click does not move point."
  (ps/org-save-test--with-file buffer "* One\n"
    (ps/org-save-test--edit buffer "* Mine\n")
    (ps/org-save-test--change-on-disk buffer "* Theirs\n")
    (with-temp-buffer
      (insert (ps/org-save--render (list buffer)))
      (goto-char (point-max))
      (forward-line -1)
      ;; Anywhere on the row, including the leading indent, resolves to it.
      (beginning-of-line)
      (should (eq (ps/org-save--buffer-at-point) buffer))
      (end-of-line)
      (should (eq (ps/org-save--buffer-at-point) buffer)))))

(ert-deftest ps/org-save--report-commands-are-interactive ()
  "Every key the report advertises is a real command."
  (dolist (command '(ps/org-save-show-stale ps/org-save-refresh
                     ps/org-save-compare ps/org-save-keep-mine
                     ps/org-save-take-disk))
    (should (commandp command))))

;;; -------------------------------------------------------
;;; Resolving
;;; -------------------------------------------------------

(ert-deftest ps/org-save--keep-mine-overwrites-the-file ()
  "\"Keep mine\" is the one path that discards the newer disk content."
  (ps/org-save-test--with-file buffer "* One\n"
    (ps/org-save-test--edit buffer "* Mine\n")
    (ps/org-save-test--change-on-disk buffer "* Theirs\n")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (with-temp-buffer
        (insert (ps/org-save--render (list buffer)))
        (goto-char (point-max))
        (forward-line -1)
        (ps/org-save-keep-mine)))
    (should-not (buffer-modified-p buffer))
    (should (equal (with-temp-buffer
                     (insert-file-contents (buffer-file-name buffer))
                     (buffer-string))
                   "* One\n* Mine\n"))))

(ert-deftest ps/org-save--take-disk-reloads-the-buffer ()
  "\"Take disk\" discards the unsaved edits and re-reads the file."
  (ps/org-save-test--with-file buffer "* One\n"
    (ps/org-save-test--edit buffer "* Mine\n")
    (ps/org-save-test--change-on-disk buffer "* Theirs\n")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (with-temp-buffer
        (insert (ps/org-save--render (list buffer)))
        (goto-char (point-max))
        (forward-line -1)
        (ps/org-save-take-disk)))
    (should-not (buffer-modified-p buffer))
    (should (equal (with-current-buffer buffer (buffer-string))
                   "* One\n* Theirs\n"))
    (should-not (ps/org-save--stale-p buffer))))

;;; -------------------------------------------------------
;;; Focus-loss save
;;; -------------------------------------------------------

(ert-deftest ps/org-save--focus-loss-threshold-default ()
  "The idle threshold defcustom has the documented default."
  (should (= ps/org-save-on-focus-loss-idle-threshold 300)))

(ert-deftest ps/org-save--focus-loss-skips-a-long-idle-emacs ()
  "A focus event long after the last command is not a hand-off to another app."
  (ps/org-save-test--with-file buffer "* One\n"
    (ps/org-save-test--edit buffer "* Two\n")
    (let ((ps/org-save--last-command-time (- (float-time) 100000))
          (saved nil))
      (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) nil))
                ((symbol-function 'ps/org-save-all-org-buffers-quietly)
                 (lambda () (setq saved t))))
        (ps/org-save-on-focus-loss)
        (should-not saved)))))

(ert-deftest ps/org-save--focus-loss-saves-after-a-recent-command ()
  "Switching to Claude Code straight after typing does save."
  (ps/org-save-test--with-file buffer "* One\n"
    (ps/org-save-test--edit buffer "* Two\n")
    (let ((ps/org-save--last-command-time (float-time))
          (saved nil))
      (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) nil))
                ((symbol-function 'ps/org-save-all-org-buffers-quietly)
                 (lambda () (setq saved t))))
        (ps/org-save-on-focus-loss)
        (should saved)))))

(provide 'test-ps-org-save)
;;; test-ps-org-save.el ends here
