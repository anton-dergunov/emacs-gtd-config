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
;; It writes nothing.  Reviewing and applying proposals is the next step; until
;; then this is the detection half of the design, which is also how the feature
;; finds which files were damaged at all — a dry run *is* the detector, and it
;; is the only one that catches Beorg, whose own reinsertion leaves level-1
;; conformance looking perfect while body and level-2 gaps are gone.
;;
;; See `design-docs/blank-line-recovery.md'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ps-blank-lines-tree)
(require 'ps-blank-lines-engine)
(require 'ps-blank-lines-git)
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
  (setq-local mode-line-format
              '((:eval (ps/mode-line--simple-view-render "Blank Lines")))))

(let ((map ps-blank-lines-mode-map))
  (define-key map (kbd "g")   #'ps/blank-lines-recover)
  (define-key map (kbd "r")   #'ps/blank-lines-recover)
  (define-key map (kbd "s")   #'ps/blank-lines-toggle-strategy)
  (define-key map (kbd "RET") #'ps/blank-lines--visit-at-point))

(defun ps/blank-lines--visit-at-point ()
  "Visit the file named by the row under point."
  (interactive)
  (when-let* ((button (button-at (point))))
    (button-activate button)))

(defun ps/blank-lines-toggle-strategy ()
  "Switch between predicting new seams from the rule and never guessing."
  (interactive)
  (setq ps/blank-lines-new-edge-strategy
        (if (eq ps/blank-lines-new-edge-strategy 'learned) 'zero 'learned))
  (message "New-edge strategy: %s" ps/blank-lines-new-edge-strategy)
  (ps/blank-lines-recover))

(defun ps/blank-lines--format-change (change)
  "Return one indented provenance line for CHANGE."
  (format "        %-4s %-34s %d → %-3d %s"
          (ps/blank-lines-change-slot change)
          (truncate-string-to-width (ps/blank-lines-change-title change) 34)
          (ps/blank-lines-change-from change)
          (ps/blank-lines-change-to change)
          (ps/blank-lines-change-detail change)))

(defun ps/blank-lines--insert-row (result)
  "Insert the summary row for RESULT, plus its provenance lines."
  (let ((file (ps/blank-lines-result-file result))
        (relpath (ps/blank-lines-result-relpath result))
        (restored (ps/blank-lines-result-restored result))
        (removed (ps/blank-lines-result-removed result)))
    (insert "  ")
    (insert-button (format "%-34s" (truncate-string-to-width relpath 34))
                   'face 'default
                   'mouse-face 'highlight
                   'follow-link t
                   'action (lambda (_b) (find-file-other-window file)))
    (cond
     ((ps/blank-lines-result-error result)
      (insert (format "— skipped: %s\n" (ps/blank-lines-result-error result))))
     ((and (zerop restored) (zerop removed))
      (insert "— no changes\n"))
     (t
      (insert (format "+%-4d -%-4d  from %s (%s, %d candidates)\n"
                      restored removed
                      (substring (ps/blank-lines-result-sha result) 0 7)
                      (substring (ps/blank-lines-result-time result) 0 10)
                      (ps/blank-lines-result-candidates result)))
      (dolist (change (ps/blank-lines-result-changes result))
        (insert (ps/blank-lines--format-change change) "\n"))
      (when (> (ps/blank-lines-result-whitespace-only result) 0)
        (insert (format "        note: %d whitespace-only line(s) would lose their spaces\n"
                        (ps/blank-lines-result-whitespace-only result))))))))

(defun ps/blank-lines--render ()
  "Render the summary buffer from its buffer-local session state."
  (let ((inhibit-read-only t)
        (results ps/blank-lines--results))
    (erase-buffer)
    (insert "Blank lines recoverable from git history\n\n")
    (insert (format "  %d file(s) scanned   %d with changes   +%d restored   -%d removed\n"
                    (length results)
                    (seq-count (lambda (r) (or (> (ps/blank-lines-result-restored r) 0)
                                               (> (ps/blank-lines-result-removed r) 0)))
                               results)
                    (apply #'+ 0 (mapcar #'ps/blank-lines-result-restored results))
                    (apply #'+ 0 (mapcar #'ps/blank-lines-result-removed results))))
    (insert (format "  new seams: %s      g refresh    s toggle strategy    RET open file\n"
                    ps/blank-lines-new-edge-strategy))
    (insert "\n  Nothing is written.  This run only reports what could be restored.\n\n")
    (insert (make-string 78 ?─) "\n")
    (dolist (result results)
      (unless (and (zerop (ps/blank-lines-result-restored result))
                   (zerop (ps/blank-lines-result-removed result))
                   (null (ps/blank-lines-result-error result)))
        (ps/blank-lines--insert-row result)))
    (when (seq-every-p (lambda (r) (and (zerop (ps/blank-lines-result-restored r))
                                        (zerop (ps/blank-lines-result-removed r))))
                       results)
      (insert "  No blank lines to recover.\n"))
    (insert (make-string 78 ?─) "\n\n")
    (insert "  Rule fitted from the healthiest known version of every file:\n")
    (dolist (row (ps/blank-lines-rule-report ps/blank-lines--rule))
      (insert (format "    %-22s → %s   (%d/%d)\n"
                      (nth 0 row) (nth 1 row) (nth 2 row) (nth 3 row))))
    (goto-char (point-min))))

;;;###autoload
(defun ps/blank-lines-recover ()
  "Report the blank lines that could be recovered from git history.

Scans every Org file, finds the version of each that still remembers its
blank lines, and shows what would be restored — with where each gap came
from.  Nothing is written; git is only read."
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
