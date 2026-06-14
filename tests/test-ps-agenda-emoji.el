;;; test-ps-agenda-emoji.el --- ERT tests for ps-agenda-emoji -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'seq)
(add-to-list 'load-path "lisp")
(require 'ps-agenda-emoji)

;; Declare special so a `let' binding below is dynamic (org-agenda may be
;; unloaded in batch, leaving the symbol otherwise lexical here).
(defvar org-agenda-finalize-hook)

;;; Helpers

(defmacro ps/agenda-emoji-test--with-lines (lines &rest body)
  "Insert LINES, then run BODY with `ps/agenda-emoji--line-title' stubbed.
The stub treats every non-blank line as a task whose title is the trimmed
line text, sidestepping the need for a live org-agenda buffer."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,lines)
     (cl-letf (((symbol-function 'ps/agenda-emoji--line-title)
                (lambda ()
                  (let ((s (string-trim
                            (buffer-substring-no-properties
                             (line-beginning-position) (line-end-position)))))
                    (if (string= s "") nil s)))))
       ,@body)))

;;; -------------------------------------------------------
;;; defcustom / API
;;; -------------------------------------------------------

(ert-deftest ps/agenda-emoji--matcher-path-is-file ()
  "The matcher path defcustom points at org_emoji_matcher.py."
  (should (stringp ps/agenda-emoji-matcher-path))
  (should (string-suffix-p "org_emoji_matcher.py" ps/agenda-emoji-matcher-path)))

(ert-deftest ps/agenda-emoji--setup-adds-hook ()
  "setup registers the append function on org-agenda-finalize-hook."
  (let ((org-agenda-finalize-hook nil))
    (ps/agenda-emoji-setup)
    (should (memq 'ps/agenda-emoji--append org-agenda-finalize-hook))))

(ert-deftest ps/agenda-emoji--append-noop-when-disabled ()
  "With the feature disabled, the finalize hook schedules no work."
  (let ((ps/agenda-emoji-enabled nil)
        (ps/agenda-emoji--timer nil))
    (ps/agenda-emoji--append)
    (should (null ps/agenda-emoji--timer))))

(ert-deftest ps/agenda-emoji--append-schedules-when-enabled ()
  "With the feature enabled, the finalize hook arms the debounce timer."
  (let ((ps/agenda-emoji-enabled t)
        (ps/agenda-emoji--timer nil))
    (unwind-protect
        (progn
          (ps/agenda-emoji--append)
          (should (timerp ps/agenda-emoji--timer)))
      (when ps/agenda-emoji--timer
        (cancel-timer ps/agenda-emoji--timer)
        (setq ps/agenda-emoji--timer nil)))))

;;; -------------------------------------------------------
;;; map-get
;;; -------------------------------------------------------

(ert-deftest ps/agenda-emoji--map-get-hash-and-alist ()
  "map-get works for both hash tables and string-keyed alists."
  (let ((h (make-hash-table :test 'equal)))
    (puthash "A" '("x") h)
    (should (equal (ps/agenda-emoji--map-get h "A") '("x")))
    (should (null (ps/agenda-emoji--map-get h "Z")))
    (should (equal (ps/agenda-emoji--map-get '(("B" . ("y"))) "B") '("y")))))

;;; -------------------------------------------------------
;;; collect-titles
;;; -------------------------------------------------------

(ert-deftest ps/agenda-emoji--collect-titles-skips-blanks ()
  "Only non-blank task lines contribute titles, in order."
  (ps/agenda-emoji-test--with-lines "Write the report\n\nBuy groceries\n"
    (should (equal (ps/agenda-emoji--collect-titles)
                   '("Write the report" "Buy groceries")))))

;;; -------------------------------------------------------
;;; cache: partition / round-trip / invalidation
;;; -------------------------------------------------------

(ert-deftest ps/agenda-emoji--partition-splits-hit-miss ()
  "Cached titles (even empty ones) go to CACHED; unseen titles to MISSING."
  (let ((ps/agenda-emoji--cache (make-hash-table :test 'equal))
        (ps/agenda-emoji--cache-loaded-tag ps/agenda-emoji-cache-tag))
    (puthash "Known" '("✅") ps/agenda-emoji--cache)
    (puthash "Empty" '() ps/agenda-emoji--cache)
    (let* ((part (ps/agenda-emoji--partition '("Known" "Empty" "Fresh")))
           (cached (car part))
           (missing (cdr part)))
      (should (equal (alist-get "Known" cached nil nil #'equal) '("✅")))
      (should (assoc "Empty" cached))             ; present, even though empty
      (should (member "Fresh" missing))
      (should-not (member "Known" missing)))))

(ert-deftest ps/agenda-emoji--cache-roundtrip ()
  "Saving then reloading preserves entries, including empty-list ones."
  (let* ((tmp (make-temp-file "emoji-cache" nil ".json"))
         (ps/agenda-emoji-cache-file tmp)
         (ps/agenda-emoji--cache (make-hash-table :test 'equal))
         (ps/agenda-emoji--cache-loaded-tag ps/agenda-emoji-cache-tag))
    (unwind-protect
        (progn
          (ps/agenda-emoji--cache-put "Task one" '("📌"))
          (ps/agenda-emoji--cache-put "Weak task" '())
          (ps/agenda-emoji--cache-save)
          ;; Simulate a fresh session: drop in-memory state, reload from disk.
          (setq ps/agenda-emoji--cache nil
                ps/agenda-emoji--cache-loaded-tag nil)
          (let ((cache (ps/agenda-emoji--cache-load)))
            (should (equal (gethash "Task one" cache) '("📌")))
            ;; present but empty (not the 'miss sentinel)
            (should (eq (gethash "Weak task" cache 'miss) nil))
            (should (eq (gethash "Absent" cache 'miss) 'miss))))
      (delete-file tmp))))

(ert-deftest ps/agenda-emoji--cache-tag-invalidates ()
  "A changed cache tag discards the on-disk cache."
  (let* ((tmp (make-temp-file "emoji-cache" nil ".json"))
         (ps/agenda-emoji-cache-file tmp)
         (ps/agenda-emoji-cache-tag "tagA")
         (ps/agenda-emoji--cache (make-hash-table :test 'equal))
         (ps/agenda-emoji--cache-loaded-tag "tagA"))
    (unwind-protect
        (progn
          (ps/agenda-emoji--cache-put "Task" '("📌"))
          (ps/agenda-emoji--cache-save)
          (setq ps/agenda-emoji--cache nil
                ps/agenda-emoji--cache-loaded-tag nil
                ps/agenda-emoji-cache-tag "tagB")
          (should (= (hash-table-count (ps/agenda-emoji--cache-load)) 0)))
      (delete-file tmp))))

;;; -------------------------------------------------------
;;; lookup (consumed by ps-agenda-layout)
;;; -------------------------------------------------------

(ert-deftest ps/agenda-emoji--lookup-returns-first-cached ()
  "lookup returns the first cached emoji for a title."
  (let ((ps/agenda-emoji-enabled t)
        (ps/agenda-emoji--cache (make-hash-table :test 'equal))
        (ps/agenda-emoji--cache-loaded-tag ps/agenda-emoji-cache-tag))
    (puthash "Known" '("✅" "📌") ps/agenda-emoji--cache)
    (should (equal (ps/agenda-emoji-lookup "Known") "✅"))))

(ert-deftest ps/agenda-emoji--lookup-nil-for-absent-or-empty ()
  "lookup returns nil for an unknown title or an empty cached list."
  (let ((ps/agenda-emoji-enabled t)
        (ps/agenda-emoji--cache (make-hash-table :test 'equal))
        (ps/agenda-emoji--cache-loaded-tag ps/agenda-emoji-cache-tag))
    (puthash "Empty" '() ps/agenda-emoji--cache)
    (should (null (ps/agenda-emoji-lookup "Empty")))
    (should (null (ps/agenda-emoji-lookup "Absent")))))

(ert-deftest ps/agenda-emoji--lookup-nil-when-disabled ()
  "lookup returns nil when the feature is disabled."
  (let ((ps/agenda-emoji-enabled nil)
        (ps/agenda-emoji--cache (make-hash-table :test 'equal))
        (ps/agenda-emoji--cache-loaded-tag ps/agenda-emoji-cache-tag))
    (puthash "Known" '("✅") ps/agenda-emoji--cache)
    (should (null (ps/agenda-emoji-lookup "Known")))))

;;; test-ps-agenda-emoji.el ends here
