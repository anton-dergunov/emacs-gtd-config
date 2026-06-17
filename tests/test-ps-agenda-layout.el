;;; test-ps-agenda-layout.el --- ERT tests for ps-agenda-layout -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path "lisp")
(require 'ps-agenda-layout)

;;; -------------------------------------------------------
;;; relative-date wording
;;; -------------------------------------------------------

(ert-deftest ps/agenda-layout--reldate-today-and-days ()
  (should (equal (ps/agenda-layout--reldate-string 0) "today"))
  (should (equal (ps/agenda-layout--reldate-string 5) "in 5d"))
  (should (equal (ps/agenda-layout--reldate-string -11) "11d ago"))
  (should (equal (ps/agenda-layout--reldate-string -1) "1d ago")))

(ert-deftest ps/agenda-layout--reldate-rolls-to-weeks ()
  (should (equal (ps/agenda-layout--reldate-string 14) "in 2w"))
  (should (equal (ps/agenda-layout--reldate-string -21) "3w ago")))

(ert-deftest ps/agenda-layout--reldate-tint-by-sign ()
  (should (eq (ps/agenda-layout--reldate-tint -3) 'overdue))
  (should (eq (ps/agenda-layout--reldate-tint 0) 'today))
  (should (eq (ps/agenda-layout--reldate-tint 4) 'future)))

(ert-deftest ps/agenda-layout--reldate-glyph-by-type ()
  (let ((ps/agenda-layout-reldate-glyphs '(("deadline" . "⚑") ("scheduled" . "⏱"))))
    (should (equal (ps/agenda-layout--reldate-glyph "deadline") "⚑"))
    (should (equal (ps/agenda-layout--reldate-glyph "scheduled") "⏱"))
    (should (null (ps/agenda-layout--reldate-glyph "timestamp")))))

(ert-deftest ps/agenda-layout--reldate-glyph-disabled-when-nil ()
  (let ((ps/agenda-layout-reldate-glyphs nil))
    (should (null (ps/agenda-layout--reldate-glyph "deadline")))))

;;; -------------------------------------------------------
;;; time formatting
;;; -------------------------------------------------------

(ert-deftest ps/agenda-layout--fmt-tod-pads-minutes ()
  (should (equal (ps/agenda-layout--fmt-tod 930) "9:30"))
  (should (equal (ps/agenda-layout--fmt-tod 1500) "15:00"))
  (should (equal (ps/agenda-layout--fmt-tod 800) "8:00")))

(ert-deftest ps/agenda-layout--time-range-adds-duration ()
  (should (equal (ps/agenda-layout--time-range 1500 60) "15:00–16:00"))
  (should (equal (ps/agenda-layout--time-range 1330 90) "13:30–15:00")))

(ert-deftest ps/agenda-layout--time-range-without-duration ()
  (should (equal (ps/agenda-layout--time-range 1500 nil) "15:00"))
  (should (equal (ps/agenda-layout--time-range 1500 0) "15:00")))

;;; -------------------------------------------------------
;;; truncation
;;; -------------------------------------------------------

(ert-deftest ps/agenda-layout--truncate-keeps-short ()
  (let ((ps/agenda-layout-ellipsis "…"))
    (should (equal (ps/agenda-layout--truncate "Short title" 40) "Short title"))))

(ert-deftest ps/agenda-layout--truncate-shortens-long ()
  (let ((ps/agenda-layout-ellipsis "…"))
    (let ((out (ps/agenda-layout--truncate "A very long title that overflows" 10)))
      (should (<= (string-width out) 10))
      (should (string-suffix-p "…" out)))))

;;; -------------------------------------------------------
;;; state labels
;;; -------------------------------------------------------

(ert-deftest ps/agenda-layout--state-label-default-verbatim ()
  (let ((ps/agenda-layout-state-labels nil))
    (should (equal (ps/agenda-layout--state-label "INPR") "INPR"))))

(ert-deftest ps/agenda-layout--state-label-remapped ()
  (let ((ps/agenda-layout-state-labels '(("INPR" . "WIP"))))
    (should (equal (ps/agenda-layout--state-label "INPR") "WIP"))
    (should (equal (ps/agenda-layout--state-label "TODO") "TODO"))))

;;; -------------------------------------------------------
;;; column geometry
;;; -------------------------------------------------------

(ert-deftest ps/agenda-layout--columns-icon-mode ()
  "Column starts follow the configured widths with an icon category."
  (let ((ps/agenda-layout-left-margin-cols 1)
        (ps/agenda-layout-gap-cols 1)
        (ps/agenda-layout-category-display 'icon)
        (ps/agenda-layout-category-cols 3)
        (ps/agenda-layout-state-cols 13)
        (ps/agenda-layout-priority-cols 4)
        (ps/agenda-layout-emoji-cols 2))
    (let ((c (ps/agenda-layout--columns)))
      (should (= (plist-get c :cat) 1))
      (should (= (plist-get c :state) 5))    ; 1 + 3 + 1
      (should (= (plist-get c :pri) 19))     ; 5 + 13 + 1
      (should (= (plist-get c :emoji) 24))   ; 19 + 4 + 1
      (should (= (plist-get c :title) 27))))) ; 24 + 2 + 1

(ert-deftest ps/agenda-layout--columns-no-category ()
  "With no category column there is no leading category gap."
  (let ((ps/agenda-layout-left-margin-cols 1)
        (ps/agenda-layout-gap-cols 1)
        (ps/agenda-layout-category-display 'none)
        (ps/agenda-layout-state-cols 13)
        (ps/agenda-layout-priority-cols 4)
        (ps/agenda-layout-emoji-cols 2))
    (let ((c (ps/agenda-layout--columns)))
      (should (= (plist-get c :state) 1))    ; left, no category gap
      (should (= (plist-get c :title) 23))))) ; 1 +13+1 +4+1 +2+1

;;; -------------------------------------------------------
;;; schedule-group detection
;;; -------------------------------------------------------

(ert-deftest ps/agenda-layout--header-schedule-detection ()
  (let ((ps/agenda-layout-schedule-group "Schedule"))
    (should (ps/agenda-layout--header-schedule-p "Schedule:"))
    (should-not (ps/agenda-layout--header-schedule-p "Overdue:"))
    (should-not (ps/agenda-layout--header-schedule-p nil))))

(ert-deftest ps/agenda-layout--header-schedule-does-not-match-scheduled-earlier ()
  "\"Scheduled earlier:\" merely contains \"Schedule\" but names a different group."
  (let ((ps/agenda-layout-schedule-group "Schedule"))
    (should-not (ps/agenda-layout--header-schedule-p "Scheduled earlier:"))))

;;; -------------------------------------------------------
;;; tags string
;;; -------------------------------------------------------

(ert-deftest ps/agenda-layout--tags-string-format ()
  (should (equal (ps/agenda-layout--tags-string nil) ""))
  (let ((s (ps/agenda-layout--tags-string '("work" "home"))))
    (should (string-match-p ":work:home:" s))
    ;; carries org-modern-tag face on the tag text, matching org-buffer style
    (should (eq (get-text-property (1+ (string-match ":" s)) 'face s)
                'org-modern-tag))))

;;; -------------------------------------------------------
;;; buffer-text replacement
;;; -------------------------------------------------------

(ert-deftest ps/agenda-layout--strip-display-props-drops-display-face-help-echo ()
  (should (equal (ps/agenda-layout--strip-display-props
                  '(display "x" face bold help-echo "y" org-marker mark todo-state "TODO"
                    org-not-done-regexp "TODO" org-todo-regexp "TODO\\|DONE"))
                 '(org-marker mark todo-state "TODO"))))

(ert-deftest ps/agenda-layout--replace-line-renders-display-and-keeps-nav-props ()
  "The replacement string's own `display'/`face' render on real buffer text,
while the original line's navigation properties (e.g. `org-marker') survive
on the replacement so RET/TAB and `org-get-at-bol' keep working."
  (with-temp-buffer
    (insert (propertize "old text" 'org-marker 'MARK 'face 'bold))
    (insert "\n")
    (goto-char (point-min))
    (ps/agenda-layout--replace-line
     (point-min) (line-end-position)
     (propertize "new" 'display '(space :align-to 5) 'face 'shadow))
    (goto-char (point-min))
    (should (equal (buffer-substring-no-properties (point-min) (line-end-position)) "new"))
    ;; the replacement's own display/face render directly on buffer text
    (should (equal (get-text-property (point-min) 'display) '(space :align-to 5)))
    (should (eq (get-text-property (point-min) 'face) 'shadow))
    ;; the original line's navigation property is preserved
    (should (eq (get-text-property (point-min) 'org-marker) 'MARK))))

;;; -------------------------------------------------------
;;; state-cols auto-compute
;;; -------------------------------------------------------

(ert-deftest ps/agenda-layout--effective-state-cols-integer-override ()
  (let ((ps/agenda-layout-state-cols 10))
    (should (= (ps/agenda-layout--effective-state-cols) 10))))

(ert-deftest ps/agenda-layout--effective-state-cols-auto ()
  "Auto-compute returns max keyword length + 2 padding spaces."
  (let ((ps/agenda-layout-state-cols nil)
        (org-todo-all-keywords '("TODO" "NEXT" "DONE")))
    (should (= (ps/agenda-layout--effective-state-cols) 6))))

;;; -------------------------------------------------------
;;; badge text functions
;;; -------------------------------------------------------

(ert-deftest ps/agenda-layout--state-text-active ()
  "State badge uses display-property padding like org-modern--todo, not literal spaces."
  (let ((ps/agenda-layout-state-labels nil)
        (org-done-keywords nil))
    (let ((s (ps/agenda-layout--state-text "INPR")))
      ;; actual string content is just the label, no surrounding spaces
      (should (equal s "INPR"))
      ;; first char has leading space via display property
      (should (equal (get-text-property 0 'display s) " I"))
      ;; last char has trailing space via display property
      (should (equal (get-text-property 3 'display s) "R "))
      (should (eq (get-text-property 0 'face s) 'org-modern-todo)))))

(ert-deftest ps/agenda-layout--state-text-done ()
  (let ((ps/agenda-layout-state-labels nil)
        (org-done-keywords '("DONE")))
    (let ((s (ps/agenda-layout--state-text "DONE")))
      (should (equal s "DONE"))
      (should (equal (get-text-property 0 'display s) " D"))
      (should (equal (get-text-property 3 'display s) "E "))
      (should (eq (get-text-property 0 'face s) 'org-modern-done)))))

(ert-deftest ps/agenda-layout--priority-text-face ()
  "Priority badge is the bare letter, padded to \" A \" via a display property."
  (let ((s (ps/agenda-layout--priority-text ?A)))
    (should (equal s "A"))
    (should (equal (get-text-property 0 'display s) " A "))
    (should (eq (get-text-property 0 'face s) 'org-modern-priority))))

(ert-deftest ps/agenda-layout--priority-text-nil ()
  (should (null (ps/agenda-layout--priority-text nil))))

(ert-deftest ps/agenda-layout--effective-priority-cols-collapsed ()
  "Priority column collapses to zero width when no task in scope is prioritised."
  (let ((ps/agenda-layout-priority-cols 3))
    (let ((ps/agenda-layout--reserve-priority nil))
      (should (= 0 (ps/agenda-layout--effective-priority-cols))))
    ;; With the flag set, the batch fallback is the configured width.
    (let ((ps/agenda-layout--reserve-priority t))
      (should (= 3 (ps/agenda-layout--effective-priority-cols))))))

(ert-deftest ps/agenda-layout--reldate-text-tints ()
  (should (eq (get-text-property 0 'face (ps/agenda-layout--reldate-text "3d ago" 'overdue))
              'ps/agenda-layout-reldate-overdue))
  (should (eq (get-text-property 0 'face (ps/agenda-layout--reldate-text "today" 'today))
              'ps/agenda-layout-reldate-today))
  (should (eq (get-text-property 0 'face (ps/agenda-layout--reldate-text "in 5d" 'future))
              'ps/agenda-layout-reldate-future))
  (should (eq (get-text-property 0 'face (ps/agenda-layout--reldate-text "9:00" 'time))
              'ps/agenda-layout-reldate-time)))

(ert-deftest ps/agenda-layout--date-dotted-inserts-dot ()
  (should (equal (ps/agenda-layout--date-dotted "Wednesday  17 June 2026")
                 "Wednesday · 17 June 2026"))
  (should (equal (ps/agenda-layout--date-dotted "Tuesday   16 June 2026")
                 "Tuesday · 16 June 2026")))

(ert-deftest ps/agenda-layout--date-dotted-trims-and-noops ()
  ;; Leading/trailing space trimmed; a single-spaced string is left as-is.
  (should (equal (ps/agenda-layout--date-dotted "  Friday  1 May 2026  ")
                 "Friday · 1 May 2026"))
  (should (equal (ps/agenda-layout--date-dotted "Monday")
                 "Monday")))

(ert-deftest ps/agenda-layout--reldate-text-content ()
  (should (equal (ps/agenda-layout--reldate-text "in 5d" 'future) " in 5d")))

;;; test-ps-agenda-layout.el ends here
