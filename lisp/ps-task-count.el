;;; ps-task-count.el --- Per-buffer open-task count for the mode line -*- lexical-binding: t; -*-

;;; Commentary:

;; Counts a buffer's headings by TODO state and feeds the result into the
;; mode-line variables declared in lisp/ps-mode-line.el
;; (`ps/mode-line--task-count-open', `ps/mode-line--task-count-tooltip',
;; `ps/mode-line--task-count-gen') -- the same cross-module ownership split
;; already used for the agenda's conflict count (lisp/ps-conflicts.el).
;;
;; Only installed for buffers `ps/org-files-in-scope-p' accepts (the same
;; corpus the agenda scans), and recomputed on actual edits only, debounced
;; onto idle time via a single-shot timer -- mirroring the DONE-fade
;; overlay rebuild in lisp/ps-done.el.  There is no periodic polling: a
;; buffer that is never edited never has a timer running for it.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'ps-org-files)
(require 'ps-mode-line)

(declare-function ps/org-files-in-scope-p "ps-org-files" (&optional file root))

;; Populated by Org from `org-todo-keywords' after mode init.
(defvar org-todo-keywords-1)
(defvar org-done-keywords)

;;; Customization

(defgroup ps-task-count nil
  "Per-buffer open-task count shown in the mode line."
  :group 'ps)

(defcustom ps/task-count-idle-delay 0.5
  "Idle seconds to wait after an edit before recomputing the task count.
Debounces the rebuild onto idle time so it does not run on every keystroke;
it fires once typing pauses for this long."
  :type 'number
  :group 'ps-task-count)

;;; Pure counting

(defun ps/task-count--tally ()
  "Return an alist of (STATE . COUNT) for the current Org buffer.
Ordered by `org-todo-keywords-1' (the flattened, canonical keyword list --
never hardcoded, per this config's single-source-of-truth convention for
TODO keywords).  Headings with no recognized TODO state are ignored, so
plain section headings never contribute to any count.

Scans with `org-with-wide-buffer' plus SCOPE nil rather than SCOPE `file':
`org-map-entries' silently scans nothing for `file' when the buffer has no
`buffer-file-name' (it resolves the scope to a file list via
`buffer-file-name' and iterates that), which would make this always report
zero for a non-file-visiting buffer -- including in tests.  SCOPE nil scans
the current buffer directly, respecting any active restriction, hence the
explicit widen so a narrowed subtree doesn't undercount the file."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (kw org-todo-keywords-1)
      (puthash kw 0 counts))
    (org-with-wide-buffer
     (org-map-entries
      (lambda ()
        (let ((state (org-get-todo-state)))
          (when (and state (gethash state counts))
            (cl-incf (gethash state counts)))))
      nil nil))
    (mapcar (lambda (kw) (cons kw (gethash kw counts))) org-todo-keywords-1)))

(defun ps/task-count--total-open (tally)
  "Return the sum of TALLY's counts for states not in `org-done-keywords'."
  (apply #'+ (mapcar #'cdr (seq-remove (lambda (pair) (member (car pair) org-done-keywords))
                                       tally))))

(defun ps/task-count--total-all (tally)
  "Return the sum of every count in TALLY, DONE states included."
  (apply #'+ (mapcar #'cdr tally)))

(defun ps/task-count--tooltip (tally)
  "Return a per-state breakdown string for TALLY, one \"STATE: N\" per line.
Every state in `org-todo-keywords-1' is listed, including DONE and any
state with a zero count."
  (mapconcat (lambda (pair) (format "%s: %d" (car pair) (cdr pair))) tally "\n"))

;;; Live tracking

(defvar-local ps/task-count--timer nil
  "Pending idle timer that will recompute this buffer's task count, or nil.")

(defun ps/task-count--recompute (buffer)
  "Recompute BUFFER's task count and refresh its mode line.
The debounced worker scheduled by `ps/task-count--schedule'.  Sets
`ps/mode-line--task-count-open' to nil when the buffer has no heading with
a recognized TODO state at all -- 0 is a distinct, valid value meaning
every such heading is DONE."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ps/task-count--timer nil)
      (let* ((tally (ps/task-count--tally))
             (total-all (ps/task-count--total-all tally)))
        (setq-local ps/mode-line--task-count-open
                    (and (> total-all 0) (ps/task-count--total-open tally)))
        (setq-local ps/mode-line--task-count-tooltip
                    (and (> total-all 0) (ps/task-count--tooltip tally))))
      (cl-incf ps/mode-line--task-count-gen)
      ;; ALL=t: the buffer may be visible in more than one window, each with
      ;; its own per-window render cache (see ps-mode-line.el).
      (force-mode-line-update t))))

(defun ps/task-count--schedule (&rest _)
  "Debounce a task-count recompute onto idle time.
Added to buffer-local `after-change-functions' (its BEG/END/LEN args are
ignored), so it runs on actual edits -- including TODO-state cycling, which
rewrites the keyword text -- not on cursor movement.  Arming only when no
timer is already pending is what keeps this purely event-driven: nothing
runs, and no timer exists, for a buffer that sits unedited."
  (unless ps/task-count--timer
    (setq ps/task-count--timer
          (run-with-idle-timer ps/task-count-idle-delay nil
                               #'ps/task-count--recompute (current-buffer)))))

(defun ps/task-count--org-setup ()
  "Install task-count tracking in the current buffer, if it is in scope.
Buffers outside `ps/org-files-in-scope-p' (docs, config.org, unsaved
buffers, files outside the Org base directory) install nothing and are
never scanned."
  (when (ps/org-files-in-scope-p buffer-file-name)
    (add-hook 'after-change-functions #'ps/task-count--schedule nil t)
    (ps/task-count--recompute (current-buffer))))

;;;###autoload
(defun ps/task-count-setup ()
  "Enable the per-buffer task count feeding the mode line."
  (add-hook 'org-mode-hook #'ps/task-count--org-setup))

(provide 'ps-task-count)
;;; ps-task-count.el ends here
