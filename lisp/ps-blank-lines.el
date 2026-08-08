;;; ps-blank-lines.el --- Recover blank lines lost to mobile Org editors -*- lexical-binding: t; -*-

;;; Commentary:

;; Org files edited in Beorg or Orgzly come back with their blank lines gone:
;; both apps parse a heading into (heading line, content string) and trim that
;; string's leading and trailing whitespace on write.  Across the strip commits
;; in the author's history that cost 111 blank lines between a heading and its
;; first body line, and 104 between a body line and the next heading.
;;
;; Those blank lines are deliberate and not describable by a rule, so they are
;; recovered from the version of the file that still has them — git already
;; holds it, because the periodic sync commits whatever Dropbox delivers.
;;
;; This module is the driver: it scans the Org directory, picks each file's
;; healthiest ancestor (`ps-blank-lines-git.el'), fits the rule over the
;; healthiest known version of every file, proposes per file
;; (`ps-blank-lines-engine.el'), and reports.  The model lives in
;; `ps-blank-lines-tree.el'.
;;
;; The scan itself writes nothing — it is also the detector, and the only one
;; that catches Beorg, whose own reinsertion leaves level-1 conformance looking
;; perfect while body and level-2 gaps are gone.  Files are written only
;; through `ps-blank-lines-review.el', one Ediff session at a time, and only
;; for what the user accepts there.
;;
;; See `design-docs/blank-line-recovery.md'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ps-blank-lines-tree)
(require 'ps-blank-lines-engine)
(require 'ps-blank-lines-git)
(require 'ps-blank-lines-review)
(require 'ps-org-files)
(require 'ps-file-tree)
(require 'ps-window)
(require 'ps-mode-line)

;;; Data structures

(cl-defstruct (ps/blank-lines-result (:constructor ps/blank-lines--make-result))
  "What a dry run found for one file."
  file relpath                ; absolute path, and path relative to the git root
  sha time candidates         ; the chosen ancestor, and how many were considered
  restored removed            ; blank lines this proposal would add / take away
  changes                     ; list of `ps/blank-lines-change'
  whitespace-only             ; whitespace-only lines that would lose their spaces
  scanned                     ; the file's text as the scan read it
  proposed                    ; that text with the recovered blank lines
  applied                     ; nil, blank lines written, `rejected', or a reason string
  error)                      ; non-nil when the file was skipped, with the reason

;;; Scanning

(defun ps/blank-lines--read (file)
  "Return FILE's contents as a string, or nil when it cannot be read."
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun ps/blank-lines--files ()
  "Return the Org files to scan, honouring the active file-tree set."
  (ps/file-tree-filter-files (ps/org-files-in-directory (ps/org-files-root))))

(defun ps/blank-lines--fit-rule (entries)
  "Fit the rule over the healthiest known version of every file in ENTRIES.

ENTRIES is an alist of (FILE WORKING-TEXT . ANCESTOR-PLIST).  A file with no
better ancestor contributes its working-tree text, because that is then the
healthiest version known — dropping it would throw away most of the sample
and leave thinly observed cells that the rule cannot use."
  (let ((rule (ps/blank-lines-rule-empty)))
    (pcase-dolist (`(,_file ,wtext . ,pick) entries)
      (when-let* ((text (or (and pick (plist-get pick :text)) wtext))
                  (file (ps/blank-lines-parse text)))
        (ps/blank-lines-rule-observe rule file)))
    rule))

(defun ps/blank-lines-scan ()
  "Scan the Org directory and return a list of `ps/blank-lines-result'.

Two passes, because the rule and the ancestors depend on each other: pass one
picks each file's ancestor with a rule-free score, so candidates compete on
what they remember rather than on what a corpus-wide rule would have guessed
for all of them equally; pass two fits the rule and proposes."
  (let* ((root (ps/blank-lines-git-root (ps/org-files-root)))
         (files (ps/blank-lines--files))
         (entries '())
         (results '()))
    (unless root
      (user-error "%s is not in a git repository, so there is no history to recover from"
                  (ps/org-files-root)))
    ;; Pass 1 — ancestors.
    (let ((reporter (make-progress-reporter "Looking for healthy versions..."
                                            0 (length files)))
          (i 0))
      (dolist (file files)
        (progress-reporter-update reporter (setq i (1+ i)))
        (when-let* ((wtext (ps/blank-lines--read file)))
          (push (cons file (cons wtext (ps/blank-lines-select-ancestor root file wtext)))
                entries)))
      (progress-reporter-done reporter))
    (setq entries (nreverse entries))
    ;; Pass 2 — fit, then propose.
    (let ((rule (ps/blank-lines--fit-rule entries)))
      (pcase-dolist (`(,file ,wtext . ,pick) entries)
        (let* ((relpath (ps/blank-lines-git-relpath root file))
               (atext (and pick (plist-get pick :text)))
               (result (and atext (ps/blank-lines-propose
                                   wtext atext :rule rule
                                   :strategy ps/blank-lines-new-edge-strategy))))
          (push (ps/blank-lines--make-result
                 :file file
                 :relpath relpath
                 :sha (and pick (plist-get pick :sha))
                 :time (and pick (plist-get pick :time))
                 :candidates (and pick (plist-get pick :candidates))
                 :restored (or (and result (plist-get result :restored)) 0)
                 :removed (or (and result (plist-get result :removed)) 0)
                 :changes (and result (plist-get result :changes))
                 :whitespace-only (ps/blank-lines-count-whitespace-only wtext)
                 :scanned wtext
                 :proposed (and result (plist-get result :ok) (plist-get result :text))
                 :error (and result (plist-get result :error)))
                results)))
      (cons rule (nreverse results)))))

;;; Summary buffer

(defvar-local ps/blank-lines--results nil
  "Results of the current *Org Blank Lines* session.")
(defvar-local ps/blank-lines--rule nil
  "The rule fitted during the current session.")

(defvar ps-blank-lines-mode-map (make-sparse-keymap))

(define-derived-mode ps-blank-lines-mode special-mode "Blank Lines"
  "Major mode for the *Org Blank Lines* buffer.

\\{ps-blank-lines-mode-map}"
  ;; The report sits in a narrow window beside a diff, so a long row must be
  ;; cut off rather than folded onto the next line — wrapping turns a table
  ;; into a wall.  Full provenance is on the line's tooltip.
  (setq-local truncate-lines t)
  (setq-local mode-line-format
              '((:eval (ps/mode-line--simple-view-render "Blank Lines")))))

(let ((map ps-blank-lines-mode-map))
  (define-key map (kbd "g")   #'ps/blank-lines-recover)
  (define-key map (kbd "r")   #'ps/blank-lines-recover)
  (define-key map (kbd "s")   #'ps/blank-lines-toggle-strategy)
  (define-key map (kbd "1")   #'ps/blank-lines-accept-this-file)
  (define-key map (kbd "2")   #'ps/blank-lines-reject-this-file)
  (define-key map (kbd "A")   #'ps/blank-lines-accept-all-files)
  (define-key map (kbd "d")   #'ps/blank-lines-review-this-file)
  (define-key map (kbd "n")   #'ps/blank-lines-next-file)
  (define-key map (kbd "p")   #'ps/blank-lines-previous-file)
  (define-key map (kbd "q")   #'ps/blank-lines-close)
  (define-key map (kbd "RET") #'ps/blank-lines--visit-at-point))

(defun ps/blank-lines--visit-at-point ()
  "Open the diff for the row under point."
  (interactive)
  (call-interactively #'ps/blank-lines-review-this-file))

(defun ps/blank-lines-close ()
  "Close the report, and any diff it opened."
  (interactive)
  (ps/blank-lines-review-cancel)
  (quit-window))

(defun ps/blank-lines--result-at-point ()
  "Return the `ps/blank-lines-result' whose row point is on, if any."
  (get-text-property (point) 'ps/blank-lines-result))

(defun ps/blank-lines--pending-p (result)
  "Return non-nil when RESULT still has something waiting to be applied."
  (and (null (ps/blank-lines-result-applied result))
       (null (ps/blank-lines-result-error result))
       (or (> (ps/blank-lines-result-restored result) 0)
           (> (ps/blank-lines-result-removed result) 0))))

;;;; Moving between rows

(defun ps/blank-lines--goto-row (result)
  "Put point on RESULT's row, if it is on screen."
  (let ((position (point-min)))
    (while (and position
                (not (eq result (get-text-property position 'ps/blank-lines-result))))
      (setq position (next-single-property-change
                      position 'ps/blank-lines-result)))
    (when position (goto-char position))))

(defun ps/blank-lines--move-row (direction)
  "Move point to the previous or next file row, per DIRECTION (-1 or 1)."
  (let ((here (ps/blank-lines--result-at-point))
        (step (if (> direction 0) #'next-single-property-change
                #'previous-single-property-change))
        (position (point)))
    ;; Step over property changes until a *different* result shows up, since
    ;; one row spans several lines of provenance.
    (catch 'done
      (while (setq position (funcall step position 'ps/blank-lines-result))
        (let ((there (get-text-property position 'ps/blank-lines-result)))
          (when (and there (not (eq there here)))
            (goto-char position)
            (throw 'done t))))
      (message "No further file"))))

(defun ps/blank-lines-next-file ()
  "Move point to the next file row."
  (interactive)
  (ps/blank-lines--move-row 1))

(defun ps/blank-lines-previous-file ()
  "Move point to the previous file row."
  (interactive)
  (ps/blank-lines--move-row -1))

;;;; Accepting and rejecting from the report

(defun ps/blank-lines--refresh (&optional focus)
  "Re-render the report in place, leaving point on FOCUS's row if given."
  (let ((line (line-number-at-pos)))
    (ps/blank-lines--render)
    (if (and focus (ps/blank-lines--goto-row focus))
        nil
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun ps/blank-lines--accept (result)
  "Apply RESULT and record the outcome on it.  Return non-nil when written."
  (pcase-let ((`(,ok . ,detail) (ps/blank-lines-review-apply result)))
    (setf (ps/blank-lines-result-applied result) (if ok detail detail))
    (if ok
        (message "%s: %d blank line(s) restored"
                 (ps/blank-lines-result-relpath result) detail)
      (message "%s: not written — %s"
               (ps/blank-lines-result-relpath result) detail))
    ok))

(defun ps/blank-lines--decide (result decide)
  "Settle RESULT with DECIDE, having closed any diff still on screen.

A diff open beside the report asks a question that deciding here has just
answered; leaving it up would show a comparison that no longer describes
anything.  So the review is cancelled first — it writes nothing, and under
this model it never touched the file anyway."
  (ps/blank-lines-review-cancel)
  (funcall decide result)
  (ps/blank-lines--refresh result)
  (ps/blank-lines--move-row 1))

(defun ps/blank-lines-accept-this-file ()
  "Restore every blank line proposed for the file on this line, and save it."
  (interactive)
  (let ((result (ps/blank-lines--result-at-point)))
    (cond
     ((null result) (user-error "Point is not on a file row"))
     ((not (ps/blank-lines--pending-p result))
      (user-error "Nothing left to apply for %s"
                  (ps/blank-lines-result-relpath result)))
     (t (ps/blank-lines--decide result #'ps/blank-lines--accept)))))

(defun ps/blank-lines-reject-this-file ()
  "Dismiss the proposal for the file on this line, leaving the file alone."
  (interactive)
  (let ((result (ps/blank-lines--result-at-point)))
    (cond
     ((null result) (user-error "Point is not on a file row"))
     ((not (ps/blank-lines--pending-p result))
      (user-error "Nothing left to dismiss for %s"
                  (ps/blank-lines-result-relpath result)))
     (t (ps/blank-lines--decide
         result
         (lambda (r) (setf (ps/blank-lines-result-applied r) 'rejected)))))))

(defun ps/blank-lines-accept-all-files ()
  "Restore every blank line in the report, in every file, and save them."
  (interactive)
  (let ((pending (seq-filter #'ps/blank-lines--pending-p ps/blank-lines--results)))
    (unless pending (user-error "Nothing left to apply"))
    (when (yes-or-no-p
           (format "Restore %d blank line(s) across %d file(s)? "
                   (apply #'+ 0 (mapcar #'ps/blank-lines-result-restored pending))
                   (length pending)))
      (let ((written 0) (lines 0))
        (dolist (result pending)
          (when (ps/blank-lines--accept result)
            (setq written (1+ written)
                  lines (+ lines (ps/blank-lines-result-applied result)))))
        (ps/blank-lines--refresh)
        (message "Recovered %d blank line(s) in %d file(s)%s"
                 lines written
                 (if (< written (length pending))
                     (format "; %d not written" (- (length pending) written))
                   ""))))))

;;;; Reviewing with Ediff

(defun ps/blank-lines-review-this-file ()
  "Open the diff for the file whose row point is on.

One file at a time and only on request: a queue that walked every file was
more machinery than the report needs, since the report is where files are
chosen and where the answer goes back."
  (interactive)
  (if-let* ((result (ps/blank-lines--result-at-point)))
      (ps/blank-lines--review (list result))
    (user-error "Point is not on a file row")))

(defun ps/blank-lines--review (results)
  "Start an Ediff review of RESULTS, folding the outcome back into the report.

The report is updated in place rather than re-scanned: a scan costs seconds
per file, and re-running it would throw away the view — and point in it — that
the user is working through.  Press \\<ps-blank-lines-mode-map>\\[ps/blank-lines-recover] for the authoritative state."
  ;; Opening a diff for another file replaces the one on screen rather than
  ;; refusing: the report is a list you move around in.
  (ps/blank-lines-review-cancel)
  (unless results (user-error "Nothing left to review"))
  (let ((buffer (current-buffer)))
    (ps/blank-lines-review-start
     results
     (lambda (applied skipped)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (dolist (result results)
             (let ((relpath (ps/blank-lines-result-relpath result)))
               (cond
                ((assoc relpath applied)
                 (setf (ps/blank-lines-result-applied result)
                       (cdr (assoc relpath applied))))
                ((assoc relpath skipped)
                 (setf (ps/blank-lines-result-applied result)
                       (cdr (assoc relpath skipped)))))))
           (ps/blank-lines--refresh))))
     buffer)))

(defun ps/blank-lines-toggle-strategy ()
  "Switch between predicting new seams from the rule and never guessing."
  (interactive)
  (setq ps/blank-lines-new-edge-strategy
        (if (eq ps/blank-lines-new-edge-strategy 'learned) 'zero 'learned))
  (message "New-edge strategy: %s" ps/blank-lines-new-edge-strategy)
  (ps/blank-lines-recover))

(defconst ps/blank-lines--source-labels
  '((exact-edge . "edge")
    (half-edge  . "half")
    (rule       . "rule")
    (keep       . "kept")
    (unmatched  . "new")
    (verbatim   . "same")
    (body-swap  . "body"))
  "Short column labels for `ps/blank-lines-change' sources.
The full sentence stays reachable as the line's tooltip; the report has to fit
a narrow window beside a diff, and a column of sentences does not.")

(defun ps/blank-lines--format-change (change)
  "Return one indented provenance line for CHANGE, tooltipped with the detail."
  (propertize
   (format "    %-4s %-24s %d→%-2d %s"
           (ps/blank-lines-change-slot change)
           (truncate-string-to-width (ps/blank-lines-change-title change) 24)
           (ps/blank-lines-change-from change)
           (ps/blank-lines-change-to change)
           (or (cdr (assq (ps/blank-lines-change-source change)
                          ps/blank-lines--source-labels))
               (ps/blank-lines-change-source change)))
   'help-echo (ps/blank-lines-change-detail change)))

(defun ps/blank-lines--insert-row (result)
  "Insert the summary row for RESULT, plus its provenance lines.
The whole row carries RESULT, so `d' can review just this file."
  (let ((start (point)))
    (ps/blank-lines--insert-row-1 result)
    (put-text-property start (point) 'ps/blank-lines-result result)))

(defun ps/blank-lines--display-path (result)
  "Return RESULT's file relative to the Org directory.
Shorter than the git-root-relative path the engine keys on, and the part it
drops is the same for every row."
  (let ((file (ps/blank-lines-result-file result)))
    (or (ignore-errors (file-relative-name file (ps/org-files-root)))
        (ps/blank-lines-result-relpath result))))

(defun ps/blank-lines--insert-row-1 (result)
  "Insert the summary row for RESULT, plus its provenance lines."
  (let ((file (ps/blank-lines-result-file result))
        (path (ps/blank-lines--display-path result))
        (restored (ps/blank-lines-result-restored result))
        (removed (ps/blank-lines-result-removed result)))
    (insert " ")
    (insert-button (format "%-26s" (truncate-string-to-width path 26))
                   ;; Bold: the row is a block of lines and the file name is
                   ;; what starts it, so it has to be findable at a glance.
                   'face 'bold
                   'mouse-face 'highlight
                   'follow-link t
                   'help-echo (ps/blank-lines-result-relpath result)
                   'action (lambda (_b) (ps/blank-lines-review-this-file)))
    (cond
     ((ps/blank-lines-result-error result)
      (insert (format "— skipped: %s\n" (ps/blank-lines-result-error result))))
     ((integerp (ps/blank-lines-result-applied result))
      (insert (format "✓ %d blank line(s) restored\n"
                      (ps/blank-lines-result-applied result))))
     ((eq (ps/blank-lines-result-applied result) 'rejected)
      (insert "— dismissed\n"))
     ((stringp (ps/blank-lines-result-applied result))
      (insert (format "— not written: %s\n" (ps/blank-lines-result-applied result))))
     ((and (zerop restored) (zerop removed))
      (insert "— no changes\n"))
     (t
      (insert (propertize
               (format "+%-3d -%-3d %s %s\n"
                       restored removed
                       (substring (ps/blank-lines-result-sha result) 0 7)
                       (substring (ps/blank-lines-result-time result) 0 10))
               'help-echo (format "from %s, chosen from %d candidate(s)"
                                  (substring (ps/blank-lines-result-sha result) 0 7)
                                  (ps/blank-lines-result-candidates result))))
      (dolist (change (ps/blank-lines-result-changes result))
        (insert (ps/blank-lines--format-change change) "\n"))
      (when (> (ps/blank-lines-result-whitespace-only result) 0)
        (insert (format "    note: %d whitespace-only line(s) lose their spaces\n"
                        (ps/blank-lines-result-whitespace-only result))))))))

(defun ps/blank-lines--render ()
  "Render the summary buffer from its buffer-local session state."
  (let* ((inhibit-read-only t)
         (results ps/blank-lines--results)
         (pending (seq-filter #'ps/blank-lines--pending-p results))
         (done (seq-filter (lambda (r) (integerp (ps/blank-lines-result-applied r)))
                           results)))
    (erase-buffer)
    (insert (format " %d files · %d to go · +%d -%d%s\n"
                    (length results)
                    (length pending)
                    (apply #'+ 0 (mapcar #'ps/blank-lines-result-restored pending))
                    (apply #'+ 0 (mapcar #'ps/blank-lines-result-removed pending))
                    (if done
                        (format " · ✓ %d in %d"
                                (apply #'+ 0 (mapcar #'ps/blank-lines-result-applied done))
                                (length done))
                      "")))
    (insert " 1 accept · 2 dismiss · A accept all\n")
    (insert " d/RET diff · n/p move · q close\n")
    (insert (format " g rescan · s seams: %s\n" ps/blank-lines-new-edge-strategy))
    (insert (make-string 46 ?─) "\n")
    (dolist (result results)
      (unless (and (zerop (ps/blank-lines-result-restored result))
                   (zerop (ps/blank-lines-result-removed result))
                   (null (ps/blank-lines-result-error result)))
        (ps/blank-lines--insert-row result)))
    (when (seq-every-p (lambda (r) (and (zerop (ps/blank-lines-result-restored r))
                                        (zerop (ps/blank-lines-result-removed r))))
                       results)
      (insert " No blank lines to recover.\n"))
    (insert (make-string 46 ?─) "\n")
    (insert " Spacing worked out from your own files:\n")
    (dolist (row (ps/blank-lines-rule-report ps/blank-lines--rule))
      (insert (format "   %-21s → %s  (%d/%d)\n"
                      (nth 0 row) (nth 1 row) (nth 2 row) (nth 3 row))))
    (goto-char (point-min))))

;;;###autoload
(defun ps/blank-lines-recover ()
  "Report the blank lines that could be recovered from git history.

Scans every Org file, finds the version of each that still remembers its
blank lines, and shows what would be restored — with where each gap came
from.  The scan itself writes nothing; in the report,
\\<ps-blank-lines-mode-map>\\[ps/blank-lines-accept-this-file] accepts a file and \\[ps/blank-lines-review-this-file] opens it side by side first."
  (interactive)
  (pcase-let* ((`(,rule . ,results) (ps/blank-lines-scan))
               (buffer (get-buffer-create "*Org Blank Lines*")))
    (with-current-buffer buffer
      (unless (eq major-mode 'ps-blank-lines-mode) (ps-blank-lines-mode))
      (setq ps/blank-lines--results results
            ps/blank-lines--rule rule)
      (ps/blank-lines--render))
    (ps/window-show-here buffer)))

(provide 'ps-blank-lines)
;;; ps-blank-lines.el ends here
