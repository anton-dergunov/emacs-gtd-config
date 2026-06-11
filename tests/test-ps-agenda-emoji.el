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

(defun ps/agenda-emoji-test--overlays ()
  "Return the emoji overlays in the current buffer."
  (seq-filter (lambda (o) (overlay-get o 'ps/agenda-emoji))
              (overlays-in (point-min) (point-max))))

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
;;; apply (right-aligned overlays)
;;; -------------------------------------------------------

(ert-deftest ps/agenda-emoji--apply-places-overlay ()
  "A mapped task gets exactly one emoji overlay carrying the glyph."
  (ps/agenda-emoji-test--with-lines "Write the report\nBuy groceries\n"
    (ps/agenda-emoji--apply '(("Write the report" . ("X"))))
    (let ((ovs (ps/agenda-emoji-test--overlays)))
      (should (= (length ovs) 1))
      (should (string-match-p "X" (overlay-get (car ovs) 'after-string))))))

(ert-deftest ps/agenda-emoji--apply-skips-unmapped ()
  "A task absent from the map gets no overlay."
  (ps/agenda-emoji-test--with-lines "Write the report\n"
    (ps/agenda-emoji--apply '(("Nonexistent" . ("Z"))))
    (should-not (ps/agenda-emoji-test--overlays))))

(ert-deftest ps/agenda-emoji--apply-is-idempotent ()
  "Re-applying clears prior overlays instead of stacking them."
  (ps/agenda-emoji-test--with-lines "Write the report\n"
    (ps/agenda-emoji--apply '(("Write the report" . ("X"))))
    (ps/agenda-emoji--apply '(("Write the report" . ("X"))))
    (should (= (length (ps/agenda-emoji-test--overlays)) 1))))

(ert-deftest ps/agenda-emoji--apply-emoji-has-face ()
  "The appended glyph carries `ps/agenda-emoji-face'."
  (ps/agenda-emoji-test--with-lines "Write the report\n"
    (ps/agenda-emoji--apply '(("Write the report" . ("X"))))
    (let* ((ov (car (ps/agenda-emoji-test--overlays)))
           (s (overlay-get ov 'after-string))
           (idx (string-match "X" s)))
      (should (equal (get-text-property idx 'face s) ps/agenda-emoji-face)))))

;;; test-ps-agenda-emoji.el ends here
