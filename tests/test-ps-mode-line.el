;;; test-ps-mode-line.el --- ERT tests for ps-mode-line -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path "lisp")
(require 'ps-file-tree)   ; provides ps/file-tree--normalize-display-name
(require 'ps-claude)      ; provides ps/claude--session-buffer-p
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

(ert-deftest ps/mode-line--buffer-name-replaces-underscores ()
  (with-temp-buffer
    (rename-buffer "Deep_Learning.org" t)
    (should (equal (ps/mode-line--buffer-name) "Deep Learning"))
    (should (equal (ps/mode-line--frame-title) "Deep Learning"))))

(ert-deftest ps/mode-line--frame-title-labels-claude-session ()
  (with-temp-buffer
    (rename-buffer "*claude-code[org]*" t)
    (should (equal (ps/mode-line--frame-title) "Claude Code"))))

;;; -------------------------------------------------------
;;; ps/mode-line--display-name (uniquified buffer names)
;;; -------------------------------------------------------

(ert-deftest ps/mode-line--display-name-plain ()
  "Without a uniquify qualifier the name just loses its extension."
  (should (equal (ps/mode-line--display-name "Work.org") "Work"))
  (should (equal (ps/mode-line--display-name "scratch") "scratch")))

(ert-deftest ps/mode-line--display-name-moves-qualifier-to-front ()
  "`uniquify''s trailing <dir> becomes a leading dir/ and the extension goes."
  (should (equal (ps/mode-line--display-name "Work.org<mydir>") "mydir/Work"))
  (should (equal (ps/mode-line--display-name "Inbox.org<Personal>")
                 "Personal/Inbox")))

(ert-deftest ps/mode-line--display-name-nested-qualifier ()
  "A deeper qualifier already reads as a path and needs no special handling."
  (should (equal (ps/mode-line--display-name "Work.org<a/b>") "a/b/Work")))

(ert-deftest ps/mode-line--display-name-keeps-numeric-suffix ()
  "Emacs's own <2> fallback is not a directory, so it stays put."
  (should (equal (ps/mode-line--display-name "Work.org<2>") "Work<2>")))

(ert-deftest ps/mode-line--display-name-non-org-file ()
  "The reformatting is not Org-specific."
  (should (equal (ps/mode-line--display-name "init.el<emacs>") "emacs/init.el")))

(ert-deftest ps/mode-line--display-name-leaves-bracketed-filename ()
  "A file whose own name contains <> is not a uniquified name."
  (should (equal (ps/mode-line--display-name "<draft>.org") "<draft>"))
  (should (equal (ps/mode-line--display-name "*claude-code[org]*")
                 "*claude-code[org]*")))

(ert-deftest ps/mode-line--buffer-name-uses-display-name ()
  "The mode line and frame title both go through the reformatting."
  (with-temp-buffer
    (rename-buffer "Photo.org<Trips>" t)
    (should (equal (ps/mode-line--buffer-name) "Trips/Photo"))
    (should (equal (ps/mode-line--frame-title) "Trips/Photo"))))

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
;;; ps/mode-line--task-count-segment
;;; -------------------------------------------------------

(ert-deftest ps/mode-line--task-count-segment-nil-when-no-count ()
  (with-temp-buffer
    (let ((ps/mode-line--task-count-open nil))
      (should-not (ps/mode-line--task-count-segment)))))

(ert-deftest ps/mode-line--task-count-segment-shows-count-and-tooltip ()
  (with-temp-buffer
    (let ((ps/mode-line--task-count-open 3)
          (ps/mode-line--task-count-tooltip "TODO: 3\nDONE: 1"))
      (let ((s (ps/mode-line--task-count-segment)))
        (should (equal s "3"))
        (should (equal (get-text-property 0 'help-echo s) "TODO: 3\nDONE: 1"))))))

(ert-deftest ps/mode-line--task-count-segment-shows-zero-not-absent ()
  "0 is a valid value distinct from nil and must still render."
  (with-temp-buffer
    (let ((ps/mode-line--task-count-open 0)
          (ps/mode-line--task-count-tooltip "DONE: 1"))
      (should (equal (ps/mode-line--task-count-segment) "0")))))

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
;;; ps/mode-line--render (task-count segment placement)
;;; -------------------------------------------------------

(ert-deftest ps/mode-line--render-includes-task-count-between-name-and-percent ()
  (with-temp-buffer
    (rename-buffer "Inbox.org" t)
    (org-mode)
    (let ((ps/mode-line--task-count-open 2))
      ;; The trailing "%" is doubled by `ps/mode-line--escape' so it survives
      ;; mode-line %-construct expansion.
      (should (equal (ps/mode-line--render) " Inbox · 2 · 0%%")))))

(ert-deftest ps/mode-line--render-omits-task-count-when-nil ()
  "Unchanged from before this feature existed when there is no count."
  (with-temp-buffer
    (rename-buffer "Inbox.org" t)
    (org-mode)
    (let ((ps/mode-line--task-count-open nil))
      (should (equal (ps/mode-line--render) " Inbox · 0%%")))))

(ert-deftest ps/mode-line--render-accounts-for-task-count-width-in-truncation ()
  "The task-count segment's width is subtracted from the truncation budget
passed to `ps/mode-line--truncate-segments' -- otherwise the breadcrumb
would overflow window-body-width by the segment's width once it's added."
  (with-temp-buffer
    (rename-buffer "Inbox.org" t)
    (org-mode)
    (insert "* Search Ranking\n** Dataset Cleanup Pipeline\n")
    (goto-char (point-max))
    (let (captured-avail)
      (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) 40))
                ((symbol-function 'ps/mode-line--truncate-segments)
                 (lambda (_titles avail) (setq captured-avail avail) "stub")))
        (let ((ps/mode-line--task-count-open nil)) (ps/mode-line--render))
        (let ((avail-without-count captured-avail))
          (let ((ps/mode-line--task-count-open 12)) (ps/mode-line--render))
          ;; The "12" segment plus its separator (" · 12", 5 columns) shrinks
          ;; the truncation budget by exactly that much.
          (should (= (- avail-without-count captured-avail) 5)))))))

;;; -------------------------------------------------------
;;; ps/mode-line--cache-valid-p / ps/mode-line--render-window-cached
;;; (per-window cache gating)
;;; -------------------------------------------------------

(ert-deftest ps/mode-line--cache-valid-p-checks-task-count-generation ()
  "A generation bump alone (bol/name unchanged) invalidates the cache.
The direct regression guard for the async-update wrinkle: the task count
recomputes off an idle timer, with neither point nor the buffer name
necessarily changing at the same time."
  (with-temp-buffer
    (rename-buffer "Inbox.org" t)
    (set-window-parameter nil 'ps-ml-bol (pos-bol))
    (set-window-parameter nil 'ps-ml-name (buffer-name))
    (set-window-parameter nil 'ps-ml-task-gen ps/mode-line--task-count-gen)
    (should (ps/mode-line--cache-valid-p))
    (setq-local ps/mode-line--task-count-gen (1+ ps/mode-line--task-count-gen))
    (should-not (ps/mode-line--cache-valid-p))))

;;; -------------------------------------------------------
;;; ps/mode-line--render-window-cached (per-window cache gating)
;;; -------------------------------------------------------

(ert-deftest ps/mode-line--render-window-cached-gates-on-line ()
  "Same-line calls return the cached string without recomputing; a line
change recomputes and updates the window-parameter cache."
  (with-temp-buffer
    (insert "line one\nline two\n")
    (goto-char (point-min))
    (let ((render-calls 0))
      (cl-letf (((symbol-function 'ps/mode-line--render)
                 (lambda () (cl-incf render-calls) (format "render-%d" render-calls))))
        ;; Seed the cache for the selected window on line 1.
        (set-window-parameter nil 'ps-ml-bol nil)
        (set-window-parameter nil 'ps-ml-str nil)
        (let ((result1 (ps/mode-line--render-window-cached)))
          (should (= render-calls 1))
          (should (equal result1 "render-1"))
          (should (equal (window-parameter nil 'ps-ml-str) "render-1")))
        ;; Same line: cache hit, no recompute.
        (forward-char 3)
        (let ((result2 (ps/mode-line--render-window-cached)))
          (should (= render-calls 1))
          (should (equal result2 "render-1")))
        ;; New line: cache miss, recompute.
        (forward-line 1)
        (let ((result3 (ps/mode-line--render-window-cached)))
          (should (= render-calls 2))
          (should (equal result3 "render-2"))
          (should (equal (window-parameter nil 'ps-ml-str) "render-2")))))))

(ert-deftest ps/mode-line--render-window-cached-recomputes-on-task-count-bump ()
  "An async task-count update (generation bump, no point movement) is not
masked by the per-window cache -- the direct regression guard for the
wrinkle this feature introduces (see `ps/mode-line--cache-valid-p')."
  (with-temp-buffer
    (insert "line one\n")
    (goto-char (point-min))
    (let ((render-calls 0))
      (cl-letf (((symbol-function 'ps/mode-line--render)
                 (lambda () (cl-incf render-calls) (format "render-%d" render-calls))))
        (set-window-parameter nil 'ps-ml-bol nil)
        (set-window-parameter nil 'ps-ml-str nil)
        (set-window-parameter nil 'ps-ml-task-gen nil)
        (ps/mode-line--render-window-cached)
        (should (= render-calls 1))
        ;; Same line, same generation: cache hit, no recompute.
        (ps/mode-line--render-window-cached)
        (should (= render-calls 1))
        ;; Bump the generation without moving point: must still recompute.
        (setq-local ps/mode-line--task-count-gen (1+ ps/mode-line--task-count-gen))
        (let ((result (ps/mode-line--render-window-cached)))
          (should (= render-calls 2))
          (should (equal result "render-2")))))))

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
        (should (eq (lookup-key map [mode-line mouse-1]) #'ps/mode-line--view-click))))))

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

(ert-deftest ps/mode-line--view-click-calls-chosen-view ()
  "Selecting a popup entry runs the command the menu keymap binds it to."
  (let ((called nil))
    (cl-letf (((symbol-function 'x-popup-menu)
               (lambda (&rest _) '(menu-bar tasks)))
              ((symbol-function 'lookup-key)
               (lambda (&rest _) #'ps/show-tasks))
              ((symbol-function 'ps/show-tasks)
               (lambda () (interactive) (setq called t))))
      (ps/mode-line--view-click 'fake-event)
      (should called))))

(ert-deftest ps/mode-line--view-click-noop-when-menu-dismissed ()
  "Dismissing the popup (nil choice) calls nothing."
  (cl-letf (((symbol-function 'x-popup-menu) (lambda (&rest _) nil)))
    (should-not (ps/mode-line--view-click 'fake-event))))

(ert-deftest ps/mode-line-view-menu-items-lists-every-view ()
  "The shared list carries all the views, with Calendar nested as a submenu."
  (let* ((items (ps/mode-line-view-menu-items))
         (labels (mapcar (lambda (i) (if (vectorp i) (aref i 0) (car i))) items)))
    (should (equal labels
                   '("Agenda" "Calendar" "Situations"
                     "Tasks" "Availability" "Conflicts")))
    ;; Calendar is a real submenu, not a flattened "Calendar: Week" entry.
    (let ((cal (seq-find (lambda (i) (equal (car-safe i) "Calendar")) items)))
      (should (listp cal))
      (should (equal (mapcar (lambda (v) (aref v 0)) (cdr cal))
                     '("Open (Day)" "Day" "Week" "Month" "Year"))))
    ;; Situations is generated on open, so the menu can never go stale.
    (let ((sit (seq-find (lambda (i) (equal (car-safe i) "Situations")) items)))
      (should (eq (nth 1 sit) :filter)))))

(ert-deftest ps/mode-line-view-menu-items-omits-situations-when-unavailable ()
  "Without the situations module the submenu is simply absent, not broken."
  ;; A nil function cell reads as unbound, and `cl-letf' restores the original.
  (cl-letf (((symbol-function 'ps/situations--menu-filter) nil))
    (should-not (fboundp 'ps/situations--menu-filter))
    (let ((labels (mapcar (lambda (i) (if (vectorp i) (aref i 0) (car i)))
                          (ps/mode-line-view-menu-items))))
      (should-not (member "Situations" labels)))))

;;; -------------------------------------------------------
;;; ps/mode-line--view-title / ps/mode-line--simple-view-render
;;; -------------------------------------------------------

(ert-deftest ps/mode-line--view-title-is-clickable ()
  "The shared title helper carries the ▾ marker and a mouse-1 view-switch
binding, reused by Availability/Conflicts as well as the agenda views."
  (let* ((s (ps/mode-line--view-title "Availability"))
         (map (get-text-property 0 'local-map s)))
    (should (string-match-p "Availability ▾" s))
    (should (eq (lookup-key map [mode-line mouse-1]) #'ps/mode-line--view-click))))

(ert-deftest ps/mode-line--simple-view-render-shows-label ()
  "The simple render wraps the clickable title with a leading space, for
buffers (Availability, Conflicts) that need nothing more in the mode line."
  (should (string-match-p "Conflicts ▾" (ps/mode-line--simple-view-render "Conflicts"))))

;;; -------------------------------------------------------
;;; Everything that is not a plan file
;;; -------------------------------------------------------

(ert-deftest ps/mode-line--shorten-path-drops-directories-before-the-name ()
  "Left-truncation, the way a shell prompt does it: the file name is the part
that identifies the buffer, so it is the last thing to go."
  (let ((path "~/info-triage-inbox/2026-08-16_167/index.md"))
    (should (equal (ps/mode-line--shorten-path path 100) path))
    ;; Zero width is no room, not a reason to show nothing.
    (should (equal (ps/mode-line--shorten-path path 0) path))
    (should (equal (ps/mode-line--shorten-path path 30)
                   "…/2026-08-16_167/index.md"))
    (should (equal (ps/mode-line--shorten-path path 15) "…/index.md"))))

(ert-deftest ps/mode-line--shorten-path-cuts-the-name-only-as-a-last-resort ()
  "And keeps its tail, where the extension is."
  (should (equal (ps/mode-line--shorten-path "/a/very-long-file-name.md" 8)
                 "…name.md"))
  (should (<= (string-width (ps/mode-line--shorten-path "/a/very-long-name.md" 8)) 8))
  (should (equal (ps/mode-line--shorten-path "/a/b.md" 1) "…")))

(ert-deftest ps/mode-line--file-url-path-recovers-a-local-page ()
  "A page read as a rendering must call itself what the same file calls itself
read as source, and eww visits a local file through a `file://' URL."
  (should (equal (ps/mode-line--file-url-path "file:///Users/a/page.html")
                 "/Users/a/page.html"))
  (should (equal (ps/mode-line--file-url-path "file:///Users/a/my%20page.html")
                 "/Users/a/my page.html"))
  (should-not (ps/mode-line--file-url-path "https://example.com/"))
  (should-not (ps/mode-line--file-url-path nil)))

(ert-deftest ps/mode-line--eww-label-names-the-page-it-is-showing ()
  (with-temp-buffer
    (setq-local eww-current-url "file:///Users/a/page.html")
    (setq-local eww-data nil)
    (should (equal (ps/mode-line--eww-label)
                   (abbreviate-file-name "/Users/a/page.html"))))
  (with-temp-buffer
    (setq-local eww-current-url "https://example.com/a/b")
    (setq-local eww-data '(:title "A page"))
    (should (equal (ps/mode-line--eww-label) "A page")))
  (with-temp-buffer
    ;; A page that reported no title falls back to its address, minus the
    ;; scheme -- the part of a URL nobody reads.
    (setq-local eww-current-url "https://example.com/a/b")
    (setq-local eww-data '(:title ""))
    (should (equal (ps/mode-line--eww-label) "example.com/a/b"))))

(ert-deftest ps/mode-line--generic-render-names-the-file-and-the-position ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/notes/index.md")
    (insert "hello")
    (goto-char (point-min))
    (let ((rendered (ps/mode-line--generic-render)))
      (should (string-match-p "index\\.md" rendered))
      ;; A `:eval' result is re-scanned for %-constructs, so the percentage has
      ;; to arrive doubled.
      (should (string-match-p "0%%" rendered)))))

(ert-deftest ps/mode-line--generic-render-escapes-a-percent-in-the-name ()
  "A path is user data.  An unescaped % swallows itself and the character
after it, so the file would be named something it is not."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/100%done.md")
    (should (string-match-p "100%%done" (ps/mode-line--generic-render)))))

(ert-deftest ps/mode-line--generic-render-gives-an-image-no-position ()
  "A picture has no reading position, and the number Emacs shows for one is an
animation's frame counter -- for a photo, always 1 and always noise."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/photo.jpg")
    (setq major-mode 'image-mode)
    (let ((rendered (ps/mode-line--generic-render)))
      (should (string-match-p "photo\\.jpg" rendered))
      (should-not (string-match-p "%" rendered))
      (should-not (string-match-p ps/mode-line-separator rendered)))))

(ert-deftest ps/mode-line--generic-render-falls-back-to-the-buffer-name ()
  "This one is drawn in every window in the frame, so an error inside it is a
configuration that looks broken everywhere rather than in one buffer."
  (with-temp-buffer
    (rename-buffer "*a buffer*" t)
    (cl-letf (((symbol-function 'ps/mode-line--identity)
               (lambda () (error "boom"))))
      (should (string-match-p "a buffer" (ps/mode-line--generic-render))))))

(ert-deftest ps/mode-line--identity-names-a-bufferless-buffer-by-its-name ()
  (with-temp-buffer
    (rename-buffer "*Messages*" t)
    (should (equal (ps/mode-line--identity) (buffer-name)))))

(ert-deftest ps/mode-line--modified-marker-only-where-nothing-auto-saves ()
  (with-temp-buffer
    (should (equal (ps/mode-line--modified-marker) ""))   ; no file
    (setq buffer-file-name "/tmp/notes.md")
    (set-buffer-modified-p nil)
    (should (equal (ps/mode-line--modified-marker) ""))
    (insert "x")
    (should (equal (ps/mode-line--modified-marker) ps/mode-line-modified-indicator))
    ;; A read-only buffer cannot have been edited here, whatever the flag says.
    (setq buffer-read-only t)
    (should (equal (ps/mode-line--modified-marker) ""))))

(ert-deftest ps/mode-line--buffer-file-sees-through-an-indirect-buffer ()
  "`org-capture' edits a plan file through an indirect buffer, whose own
`buffer-file-name' is nil -- without this it would lose its heading path."
  (let* ((base (get-buffer-create " ps-ml-base"))
         (indirect nil))
    (unwind-protect
        (progn
          (with-current-buffer base (setq buffer-file-name "/tmp/plan.org"))
          (setq indirect (make-indirect-buffer base " ps-ml-indirect"))
          (with-current-buffer indirect
            (should (equal (ps/mode-line--buffer-file) "/tmp/plan.org"))))
      (when indirect (kill-buffer indirect))
      (kill-buffer base))))

(ert-deftest ps/mode-line--org-setup-leaves-a-non-plan-org-buffer-alone ()
  "The capture queue, the docs and config.org are all Org and none of them has
a task count or a heading path worth reading; they take the generic line."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/triage.org")
    (cl-letf (((symbol-function 'ps/org-files-in-scope-p) (lambda (&rest _) nil)))
      (ps/mode-line--org-setup)
      (should-not (local-variable-p 'mode-line-format)))
    (cl-letf (((symbol-function 'ps/org-files-in-scope-p) (lambda (&rest _) t)))
      (ps/mode-line--org-setup)
      (should (local-variable-p 'mode-line-format)))))

(ert-deftest ps/mode-line-generic-format-keeps-the-nav-buttons-outside-the-render ()
  "The arrows must be a sibling of the `:eval', never inside a string it
returns -- and they must survive whichever of the two setups runs second."
  (let ((format (ps/mode-line--with-nav ps/mode-line-generic-format)))
    (should (member '(:eval (ps/mode-line--generic-render)) format))
    (when (fboundp 'ps/nav-mode-line-add)
      (should (equal format (ps/nav-mode-line-add format))))))

(provide 'test-ps-mode-line)
;;; test-ps-mode-line.el ends here
