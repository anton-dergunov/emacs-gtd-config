;;; test-ps-agenda-fold.el --- ERT tests for ps-agenda-fold -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path "lisp")
(require 'ps-agenda-fold)

;; Faithful stand-in for org's `org-get-at-bol' so the tests don't need org
;; loaded in batch.
(defmacro ps/agenda-fold-test--with-agenda (&rest body)
  "Run BODY in a temp buffer with a small agenda and `org-get-at-bol' stubbed.
Layout: header \"Overdue:\" + 2 items, header \"In progress:\" + 1 item, with a
leading date header that must NOT be treated as foldable."
  (declare (indent 0))
  `(with-temp-buffer
     (cl-letf (((symbol-function 'org-get-at-bol)
                (lambda (prop)
                  (get-text-property (line-beginning-position) prop))))
       (insert (propertize "Sunday 14 June 2026\n" 'org-agenda-date-header t))
       (insert (propertize "Overdue:\n" 'org-super-agenda-header t))
       (insert "  item one\n")
       (insert "  item two\n")
       (insert (propertize "In progress:\n" 'org-agenda-structural-header t))
       (insert "  item three\n")
       (goto-char (point-min))
       ,@body)))

(defmacro ps/agenda-fold-test--with-agenda-blank-line (&rest body)
  "Like `ps/agenda-fold-test--with-agenda', but with a blank separator line
between \"Overdue:\"'s items and the \"In progress:\" header."
  (declare (indent 0))
  `(with-temp-buffer
     (cl-letf (((symbol-function 'org-get-at-bol)
                (lambda (prop)
                  (get-text-property (line-beginning-position) prop))))
       (insert (propertize "Sunday 14 June 2026\n" 'org-agenda-date-header t))
       (insert (propertize "Overdue:\n" 'org-super-agenda-header t))
       (insert "  item one\n")
       (insert "  item two\n")
       (insert "\n")
       (insert (propertize "In progress:\n" 'org-agenda-structural-header t))
       (insert "  item three\n")
       (goto-char (point-min))
       ,@body)))

(defun ps/agenda-fold-test--goto (substr)
  "Move point to the start of the line containing SUBSTR."
  (goto-char (point-min))
  (search-forward substr)
  (beginning-of-line))

(defun ps/agenda-fold-test--overlays (prop)
  "Return overlays carrying PROP in the current buffer."
  (cl-remove-if-not (lambda (o) (overlay-get o prop))
                    (overlays-in (point-min) (point-max))))

;;; -------------------------------------------------------
;;; header detection
;;; -------------------------------------------------------

(ert-deftest ps/agenda-fold--header-p-recognizes-both-kinds ()
  (ps/agenda-fold-test--with-agenda
    (ps/agenda-fold-test--goto "Overdue:")
    (should (ps/agenda-fold--header-p))
    (ps/agenda-fold-test--goto "In progress:")
    (should (ps/agenda-fold--header-p))))

(ert-deftest ps/agenda-fold--header-p-excludes-date-and-items ()
  (ps/agenda-fold-test--with-agenda
    (ps/agenda-fold-test--goto "Sunday")
    (should-not (ps/agenda-fold--header-p))
    (ps/agenda-fold-test--goto "item one")
    (should-not (ps/agenda-fold--header-p))))

(ert-deftest ps/agenda-fold--header-label-is-trimmed ()
  (ps/agenda-fold-test--with-agenda
    (ps/agenda-fold-test--goto "Overdue:")
    (should (equal (ps/agenda-fold--header-label) "Overdue:"))))

(ert-deftest ps/agenda-fold--all-labels-in-order ()
  (ps/agenda-fold-test--with-agenda
    (should (equal (ps/agenda-fold--all-labels) '("Overdue:" "In progress:")))))

;;; -------------------------------------------------------
;;; body region
;;; -------------------------------------------------------

(ert-deftest ps/agenda-fold--body-region-spans-to-next-header ()
  (ps/agenda-fold-test--with-agenda
    (ps/agenda-fold-test--goto "Overdue:")
    (let ((region (ps/agenda-fold--body-region)))
      (should region)
      (should (string-match-p "item one"
                              (buffer-substring-no-properties (car region) (cdr region))))
      (should (string-match-p "item two"
                              (buffer-substring-no-properties (car region) (cdr region))))
      ;; stops before the next header
      (should-not (string-match-p "In progress"
                                  (buffer-substring-no-properties (car region) (cdr region)))))))

(ert-deftest ps/agenda-fold--body-region-excludes-trailing-blank-line ()
  "A blank separator line before the next header is excluded from the body."
  (ps/agenda-fold-test--with-agenda-blank-line
    (ps/agenda-fold-test--goto "Overdue:")
    (let ((region (ps/agenda-fold--body-region)))
      (should region)
      (should (string-match-p "item two"
                              (buffer-substring-no-properties (car region) (cdr region))))
      ;; ends at the start of the blank line, not at "In progress:"'s bol
      (save-excursion
        (goto-char (cdr region))
        (should (= (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (should (looking-at-p "In progress:"))))))

;;; -------------------------------------------------------
;;; collapse / indicator overlays
;;; -------------------------------------------------------

(ert-deftest ps/agenda-fold--collapse-here-hides-body ()
  (ps/agenda-fold-test--with-agenda
    (ps/agenda-fold-test--goto "Overdue:")
    (ps/agenda-fold--collapse-here)
    (let ((ovs (ps/agenda-fold-test--overlays 'ps/agenda-fold)))
      (should (= (length ovs) 1))
      (should (equal (overlay-get (car ovs) 'display) "")))))

(ert-deftest ps/agenda-fold--collapse-here-keeps-trailing-blank-line-visible ()
  (ps/agenda-fold-test--with-agenda-blank-line
    (ps/agenda-fold-test--goto "Overdue:")
    (ps/agenda-fold--collapse-here)
    (let ((item-two-pos (progn (ps/agenda-fold-test--goto "item two")
                                (point)))
          (blank-pos (progn (ps/agenda-fold-test--goto "In progress:")
                             (forward-line -1)
                             (point))))
      (should (cl-find-if (lambda (o) (overlay-get o 'ps/agenda-fold))
                          (overlays-at item-two-pos)))
      (should-not (cl-find-if (lambda (o) (overlay-get o 'ps/agenda-fold))
                              (overlays-at blank-pos))))))

(ert-deftest ps/agenda-fold--indicator-shows-arrow ()
  (ps/agenda-fold-test--with-agenda
    (ps/agenda-fold-test--goto "Overdue:")
    (ps/agenda-fold--set-indicator (line-beginning-position) nil)
    (let ((ov (car (ps/agenda-fold-test--overlays 'ps/agenda-fold-indicator))))
      (should (string-match-p "▾" (overlay-get ov 'before-string))))
    (remove-overlays (point-min) (point-max) 'ps/agenda-fold-indicator t)
    (ps/agenda-fold--set-indicator (line-beginning-position) t)
    (let ((ov (car (ps/agenda-fold-test--overlays 'ps/agenda-fold-indicator))))
      (should (string-match-p "▸" (overlay-get ov 'before-string))))))

;;; -------------------------------------------------------
;;; toggle bookkeeping
;;; -------------------------------------------------------

(ert-deftest ps/agenda-fold--toggle-tracks-collapsed-set ()
  "On a header, toggle flips the label's membership in the collapsed set."
  (ps/agenda-fold-test--with-agenda
    (cl-letf (((symbol-function 'ps/agenda-fold--apply) #'ignore))
      (setq ps/agenda-fold--collapsed nil)
      (ps/agenda-fold-test--goto "Overdue:")
      (ps/agenda-fold-toggle)
      (should (member "Overdue:" ps/agenda-fold--collapsed))
      (ps/agenda-fold-toggle)
      (should-not (member "Overdue:" ps/agenda-fold--collapsed)))))

(ert-deftest ps/agenda-fold--collapse-expand-all ()
  (ps/agenda-fold-test--with-agenda
    (cl-letf (((symbol-function 'ps/agenda-fold--apply) #'ignore))
      (ps/agenda-fold-collapse-all)
      (should (equal ps/agenda-fold--collapsed '("Overdue:" "In progress:")))
      (ps/agenda-fold-expand-all)
      (should (null ps/agenda-fold--collapsed)))))

;;; test-ps-agenda-fold.el ends here
