;;; test-ps-mode-line.el --- ERT tests for ps-mode-line -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path "lisp")
(require 'ps-file-tree)   ; provides ps/file-tree--strip-org-extension
(require 'ps-mode-line)
(require 'org)

;;; -------------------------------------------------------
;;; ps/mode-line--buffer-name / frame title
;;; -------------------------------------------------------

(ert-deftest ps/mode-line--buffer-name-strips-org ()
  (with-temp-buffer
    (rename-buffer "Photo.org" t)
    (should (equal (ps/mode-line--buffer-name) "Photo"))
    (should (equal (ps/mode-line--frame-title) "Photo"))))

(ert-deftest ps/mode-line--buffer-name-keeps-non-org ()
  (with-temp-buffer
    (rename-buffer "scratch" t)
    (should (equal (ps/mode-line--buffer-name) "scratch"))))

;;; -------------------------------------------------------
;;; ps/mode-line--escape
;;; -------------------------------------------------------

(ert-deftest ps/mode-line--escape-doubles-percent ()
  "A literal % is doubled so it survives mode-line %-construct expansion."
  (should (equal (ps/mode-line--escape "57%") "57%%"))
  (should (equal (ps/mode-line--escape "a%b%c") "a%%b%%c")))

(ert-deftest ps/mode-line--escape-noop-without-percent ()
  (should (equal (ps/mode-line--escape "Photo") "Photo")))

;;; -------------------------------------------------------
;;; ps/mode-line--percent
;;; -------------------------------------------------------

(ert-deftest ps/mode-line--percent-empty-buffer-is-zero ()
  (with-temp-buffer
    (should (equal (ps/mode-line--percent) "0%"))))

(ert-deftest ps/mode-line--percent-end-is-100 ()
  (with-temp-buffer
    (insert (make-string 100 ?x))
    (goto-char (point-max))
    (should (equal (ps/mode-line--percent) "100%"))))

(ert-deftest ps/mode-line--percent-start-is-0 ()
  (with-temp-buffer
    (insert (make-string 100 ?x))
    (goto-char (point-min))
    (should (equal (ps/mode-line--percent) "0%"))))

;;; -------------------------------------------------------
;;; ps/mode-line--join-titles
;;; -------------------------------------------------------

(ert-deftest ps/mode-line--join-titles-uses-arrow ()
  (should (equal (ps/mode-line--join-titles '("A" "B" "C")) "A > B > C"))
  (should (equal (ps/mode-line--join-titles '("Only")) "Only"))
  (should (equal (ps/mode-line--join-titles nil) "")))

;;; -------------------------------------------------------
;;; ps/mode-line--outline-titles (clean, title-only)
;;; -------------------------------------------------------

(ert-deftest ps/mode-line--outline-titles-strips-todo-priority-tags ()
  (with-temp-buffer
    (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
      (org-mode)
      (insert "* Search Ranking\n"
              "** TODO [#A] Dataset Cleanup :work:urgent:\n"
              "Body line\n")
      (goto-char (point-max))
      (should (equal (ps/mode-line--outline-titles)
                     '("Search Ranking" "Dataset Cleanup"))))))

(ert-deftest ps/mode-line--outline-titles-plain-sections ()
  "A non-task file (plain sections) yields clean section titles."
  (with-temp-buffer
    (org-mode)
    (insert "* Package Management\n** Load local modules\nstuff\n")
    (goto-char (point-max))
    (should (equal (ps/mode-line--outline-titles)
                   '("Package Management" "Load local modules")))))

(ert-deftest ps/mode-line--outline-titles-before-first-heading-is-nil ()
  (with-temp-buffer
    (org-mode)
    (insert "Preamble text before any heading\n")
    (goto-char (point-min))
    (should (null (ps/mode-line--outline-titles)))))

(ert-deftest ps/mode-line--outline-titles-strips-link-markup ()
  "A heading whose title is an Org link is shown as plain text."
  (with-temp-buffer
    (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
      (org-mode)
      (insert "* Interviews\n"
              "** TODO [[obsidian:Foo Corp - Lead - 2026-05-14]]\n"
              "Body line\n")
      (goto-char (point-max))
      (should (equal (ps/mode-line--outline-titles)
                     '("Interviews" "Foo Corp - Lead - 2026-05-14"))))))

;;; -------------------------------------------------------
;;; ps/mode-line--clean-markup
;;; -------------------------------------------------------

(ert-deftest ps/mode-line--clean-markup-obsidian-link ()
  (should (equal (ps/mode-line--clean-markup
                  "[[obsidian:Linklaters - Data Science Team Lead - 2026-05-14]]")
                 "Linklaters - Data Science Team Lead - 2026-05-14")))

(ert-deftest ps/mode-line--clean-markup-link-with-description ()
  (should (equal (ps/mode-line--clean-markup "[[https://example.com][Example]]")
                 "Example")))

(ert-deftest ps/mode-line--clean-markup-bare-link ()
  (should (equal (ps/mode-line--clean-markup "[[https://example.com]]")
                 "https://example.com")))

(ert-deftest ps/mode-line--clean-markup-emphasis ()
  (should (equal (ps/mode-line--clean-markup "*bold* and /italic/ and =code=")
                 "bold and italic and code")))

(ert-deftest ps/mode-line--clean-markup-leaves-plain-text ()
  (should (equal (ps/mode-line--clean-markup "Just a plain title") "Just a plain title"))
  (should (equal (ps/mode-line--clean-markup "A * lone asterisk") "A * lone asterisk")))

;;; -------------------------------------------------------
;;; per-segment truncation
;;; -------------------------------------------------------

(ert-deftest ps/mode-line--seg-trimmable-and-trim ()
  (should (ps/mode-line--seg-trimmable-p "Dataset"))
  (should-not (ps/mode-line--seg-trimmable-p "A…"))
  (should-not (ps/mode-line--seg-trimmable-p "A"))
  (should (equal (ps/mode-line--seg-trim "Dataset") "Datase…"))
  (should (equal (ps/mode-line--seg-trim "Da…") "D…")))

(ert-deftest ps/mode-line--truncate-no-op-when-fits ()
  (should (equal (ps/mode-line--truncate-segments '("Search Ranking" "Dataset Cleanup") 100)
                 "Search Ranking > Dataset Cleanup")))

(ert-deftest ps/mode-line--truncate-zero-width-returns-full ()
  (should (equal (ps/mode-line--truncate-segments '("A" "B") 0) "A > B")))

(ert-deftest ps/mode-line--truncate-shrinks-longest-first ()
  "The widest segment is ellipsized before the shorter one."
  (let ((out (ps/mode-line--truncate-segments '("Short" "Dataset Cleanup Pipeline") 24)))
    (should (<= (string-width out) 24))
    ;; The shorter "Short" stays intact while the much longer second
    ;; segment absorbs the trimming.
    (should (string-prefix-p "Short > " out))
    (should (string-suffix-p "…" out))))

(ert-deftest ps/mode-line--truncate-shrinks-to-fit ()
  (let* ((titles '("Search Ranking" "Dataset Cleanup"))
         (full (ps/mode-line--join-titles titles))
         (out (ps/mode-line--truncate-segments titles 24)))
    (should (< (string-width out) (string-width full)))
    (should (<= (string-width out) 24))
    (should (string-match-p "…" out))))

(ert-deftest ps/mode-line--truncate-fits-within-width ()
  (let ((out (ps/mode-line--truncate-segments '("Search Ranking" "Dataset Cleanup") 18)))
    (should (<= (string-width out) 18))))

(ert-deftest ps/mode-line--truncate-empty-list ()
  (should (equal (ps/mode-line--truncate-segments nil 10) "")))

;;; -------------------------------------------------------
;;; destructive mouse clicks disabled
;;; -------------------------------------------------------

(ert-deftest ps/mode-line--disable-destructive-mouse-binds-ignore ()
  "mouse-2 (delete-other-windows) and mouse-3 (delete-window) become no-ops."
  (let ((global-map (make-sparse-keymap)))
    ;; Seed the destructive defaults, then confirm they are neutralized.
    (define-key global-map [mode-line mouse-2] #'mouse-delete-other-windows)
    (define-key global-map [mode-line mouse-3] #'mouse-delete-window)
    (ps/mode-line--disable-destructive-mouse)
    (should (eq (lookup-key global-map [mode-line mouse-2]) #'ignore))
    (should (eq (lookup-key global-map [mode-line mouse-3]) #'ignore))))

;;; -------------------------------------------------------
;;; ps/mode-line--update-now (cache gating)
;;; -------------------------------------------------------

(ert-deftest ps/mode-line--update-now-gates-on-line ()
  "Same-line calls (e.g. each keystroke while typing) leave the cached
string untouched and never force a redraw; a line change recomputes and
forces one.  This is the synchronous worker the idle debounce drives."
  (with-temp-buffer
    (insert "line one\nline two\n")
    (goto-char (point-min))
    (let ((ps/mode-line--last-bol nil)
          (ps/mode-line--cached-string nil)
          (render-calls 0)
          (force-calls 0))
      (cl-letf (((symbol-function 'ps/mode-line--render)
                 (lambda () (cl-incf render-calls) (format "render-%d" render-calls)))
                ((symbol-function 'force-mode-line-update)
                 (lambda (&optional _all) (cl-incf force-calls))))
        ;; Initial call on line 1 establishes the cache.
        (ps/mode-line--update-now (current-buffer))
        (should (= render-calls 1))
        (should (= force-calls 1))
        (should (equal ps/mode-line--cached-string "render-1"))
        ;; Same line (simulated keystrokes): no recompute, no forced redraw.
        (forward-char 3)
        (ps/mode-line--update-now (current-buffer))
        (forward-char 2)
        (ps/mode-line--update-now (current-buffer))
        (should (= render-calls 1))
        (should (= force-calls 1))
        (should (equal ps/mode-line--cached-string "render-1"))
        ;; Moving to a different line recomputes and forces a redraw.
        (goto-char (point-min))
        (forward-line 1)
        (ps/mode-line--update-now (current-buffer))
        (should (= render-calls 2))
        (should (= force-calls 2))
        (should (equal ps/mode-line--cached-string "render-2"))))))

;;; -------------------------------------------------------
;;; ps/mode-line--agenda-render (view-switcher)
;;; -------------------------------------------------------

(ert-deftest ps/mode-line--agenda-render-title-is-clickable ()
  "The title segment carries the ▾ marker and a mouse-1 view-switch binding."
  (with-temp-buffer
    (let ((ps/mode-line--agenda-title "Agenda")
          (ps/mode-line--agenda-show-position nil))
      (let* ((s (ps/mode-line--agenda-render))
             (map (get-text-property 1 'local-map s)))
        (should (string-match-p "Agenda ▾" s))
        (should (eq (lookup-key map [mode-line mouse-1]) #'ps/mode-line--agenda-click))))))

(ert-deftest ps/mode-line--agenda-render-keeps-position-suffix ()
  "The Tasks view's percentage suffix is preserved after the clickable title."
  (with-temp-buffer
    (insert "abcdefghij")
    (goto-char (point-min))
    (let ((ps/mode-line--agenda-title "Tasks")
          (ps/mode-line--agenda-show-position t))
      (should (string-match-p "Tasks ▾ · 0%%" (ps/mode-line--agenda-render))))))

(ert-deftest ps/mode-line--agenda-render-shows-plain-clickable-conflict-count ()
  "A positive conflict count renders calmly (no face) and is clickable."
  (with-temp-buffer
    (let ((ps/mode-line--agenda-title "Agenda")
          (ps/mode-line--agenda-show-position nil)
          (ps/mode-line--agenda-conflict-count 2))
      (let* ((s (ps/mode-line--agenda-render))
             (marker-pos (string-match "⚠ 2 conflicts" s)))
        (should marker-pos)
        (should-not (get-text-property marker-pos 'face s))
        (should (eq (lookup-key (get-text-property marker-pos 'local-map s)
                                 [mode-line mouse-1])
                    #'ps/mode-line--agenda-conflicts-click))))))

(ert-deftest ps/mode-line--agenda-render-omits-conflict-count-when-zero-or-nil ()
  "No conflict segment is shown when the count is nil or zero."
  (with-temp-buffer
    (let ((ps/mode-line--agenda-title "Agenda")
          (ps/mode-line--agenda-show-position nil))
      (let ((ps/mode-line--agenda-conflict-count nil))
        (should-not (string-match-p "conflict" (ps/mode-line--agenda-render))))
      (let ((ps/mode-line--agenda-conflict-count 0))
        (should-not (string-match-p "conflict" (ps/mode-line--agenda-render)))))))

(ert-deftest ps/mode-line--agenda-conflicts-click-opens-conflicts-buffer ()
  "Clicking the conflict segment opens the dedicated Conflicts buffer."
  (let ((called nil))
    (cl-letf (((symbol-function 'ps/show-conflicts)
               (lambda () (interactive) (setq called t))))
      (ps/mode-line--agenda-conflicts-click 'fake-event)
      (should called))))

(ert-deftest ps/mode-line--agenda-click-calls-chosen-view ()
  "Selecting a popup entry calls its command."
  (let ((called nil))
    (cl-letf (((symbol-function 'x-popup-menu)
               (lambda (&rest _) #'ps/show-tasks))
              ((symbol-function 'ps/show-tasks)
               (lambda () (interactive) (setq called t))))
      (ps/mode-line--agenda-click 'fake-event)
      (should called))))

(ert-deftest ps/mode-line--agenda-click-noop-when-menu-dismissed ()
  "Dismissing the popup (nil choice) calls nothing."
  (cl-letf (((symbol-function 'x-popup-menu) (lambda (&rest _) nil)))
    (should-not (ps/mode-line--agenda-click 'fake-event))))

(provide 'test-ps-mode-line)
;;; test-ps-mode-line.el ends here
