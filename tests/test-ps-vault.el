;;; test-ps-vault.el --- ERT tests for ps-vault -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path "lisp")
(require 'ps-vault)

;; `(defvar my-org-base-directory)' with no value, as in ps-vault.el, only marks
;; the symbol special within that file -- repeat it here so the `let' bindings
;; below are dynamic rather than lexical.
(defvar my-org-base-directory)

;; Same reason: these belong to modules this test does not load, and
;; `ps/vault-state-apply' reaches them through `set', which only a dynamic
;; binding makes visible.
(defvar ps/file-tree-current-set)
(defvar ps/file-tree-set-applies-to-agenda)
(defvar ps/vault-test--scoped)
(defvar ps/vault-test--unbound)

(defmacro ps/vault-test--with-dir (&rest body)
  "Bind `dir' to a fresh temp directory, run BODY, then clean up."
  (declare (indent 0))
  `(let ((dir (file-name-as-directory (make-temp-file "ps-vault-" t))))
     (unwind-protect (progn ,@body)
       (delete-directory dir t))))

(defmacro ps/vault-test--with-home (&rest body)
  "Run BODY with `user-emacs-directory' pointing at a fresh temp directory.
Binds `home' to it, so registry reads and writes never touch the real one."
  (declare (indent 0))
  `(let* ((home (file-name-as-directory (make-temp-file "ps-vault-home-" t)))
          (user-emacs-directory home)
          (ps/vault--pinned nil))
     (unwind-protect (progn ,@body)
       (delete-directory home t))))

(defun ps/vault-test--registry (&rest paths)
  "Return a registry containing PATHS, the last of them current."
  (let ((registry (ps/vault--empty-registry)))
    (dolist (path paths registry)
      (setq registry (ps/vault--registry-set-current registry path)))))

;;; -------------------------------------------------------
;;; Paths
;;; -------------------------------------------------------

(ert-deftest ps/vault-test-normalize-adds-trailing-slash ()
  "A vault path always ends in a slash, so `concat'-style joins are safe."
  (should (equal (ps/vault--normalize-path "/tmp/notes") "/tmp/notes/"))
  (should (equal (ps/vault--normalize-path "/tmp/notes/") "/tmp/notes/")))

(ert-deftest ps/vault-test-normalize-expands ()
  "Relative and `~' paths become absolute."
  (should (file-name-absolute-p (ps/vault--normalize-path "~/notes")))
  (should (file-name-absolute-p (ps/vault--normalize-path "notes"))))

(ert-deftest ps/vault-test-normalize-rejects-empty ()
  "A missing or blank setting stays missing rather than becoming the home dir."
  (should-not (ps/vault--normalize-path nil))
  (should-not (ps/vault--normalize-path ""))
  (should-not (ps/vault--normalize-path "   ")))

(ert-deftest ps/vault-test-path-under-p ()
  "Membership is by directory component, not by string prefix."
  (should (ps/vault-path-under-p "/base/Work/Career.org" "/base/"))
  (should (ps/vault-path-under-p "/base" "/base/"))
  (should-not (ps/vault-path-under-p "/base-other/file.org" "/base/"))
  (should-not (ps/vault-path-under-p "/elsewhere/file.org" "/base/"))
  (should-not (ps/vault-path-under-p nil "/base/")))

;;; -------------------------------------------------------
;;; Registry manipulation
;;; -------------------------------------------------------

(ert-deftest ps/vault-test-registry-add-appends ()
  "Vaults keep insertion order, which is the order the menu shows."
  (let ((registry (ps/vault-test--registry "/a/one" "/a/two" "/a/three")))
    (should (equal (mapcar #'ps/vault--entry-path
                           (ps/vault--registry-vaults registry))
                   '("/a/one/" "/a/two/" "/a/three/")))))

(ert-deftest ps/vault-test-registry-add-is-idempotent ()
  "Adding a known vault again does not duplicate it or move it."
  (let* ((registry (ps/vault-test--registry "/a/one" "/a/two"))
         (again (ps/vault--registry-add registry "/a/one/")))
    (should (equal (mapcar #'ps/vault--entry-path
                           (ps/vault--registry-vaults again))
                   '("/a/one/" "/a/two/")))))

(ert-deftest ps/vault-test-registry-add-updates-name-in-place ()
  "Re-adding with a name renames without reordering."
  (let* ((registry (ps/vault-test--registry "/a/one" "/a/two"))
         (named (ps/vault--registry-add registry "/a/one" "First")))
    (should (equal (ps/vault-entry-name (ps/vault--registry-entry named "/a/one/"))
                   "First"))
    (should (equal (mapcar #'ps/vault--entry-path
                           (ps/vault--registry-vaults named))
                   '("/a/one/" "/a/two/")))))

(ert-deftest ps/vault-test-registry-remove-drops-the-entry ()
  "Removing a vault takes it out of the list."
  (let ((registry (ps/vault--registry-remove
                   (ps/vault-test--registry "/a/one" "/a/two") "/a/one")))
    (should (equal (mapcar #'ps/vault--entry-path
                           (ps/vault--registry-vaults registry))
                   '("/a/two/")))))

(ert-deftest ps/vault-test-registry-remove-clears-current ()
  "Removing the open vault leaves no current one for the caller to fall back from."
  (let ((registry (ps/vault--registry-remove
                   (ps/vault-test--registry "/a/one" "/a/two") "/a/two")))
    (should-not (ps/vault--registry-current registry))))

(ert-deftest ps/vault-test-registry-remove-keeps-other-current ()
  "Removing a vault that is not open leaves the current one alone."
  (let ((registry (ps/vault--registry-remove
                   (ps/vault-test--registry "/a/one" "/a/two") "/a/one")))
    (should (equal (ps/vault--registry-current registry) "/a/two/"))))

(ert-deftest ps/vault-test-registry-fallback ()
  "The fallback is the current vault, else the first known one, else nothing."
  (should (equal (ps/vault--registry-fallback
                  (ps/vault-test--registry "/a/one" "/a/two"))
                 "/a/two/"))
  (should (equal (ps/vault--registry-fallback
                  (ps/vault--registry-remove
                   (ps/vault-test--registry "/a/one" "/a/two") "/a/two"))
                 "/a/one/"))
  (should-not (ps/vault--registry-fallback (ps/vault--empty-registry))))

(ert-deftest ps/vault-test-registry-rename-blank-clears-the-name ()
  "A blank name falls back to the directory's own name rather than sticking."
  (let ((registry (ps/vault--registry-rename
                   (ps/vault--registry-add (ps/vault--empty-registry) "/a/notes" "Mine")
                   "/a/notes" "")))
    (should (equal (ps/vault-entry-name
                    (ps/vault--registry-entry registry "/a/notes"))
                   "notes"))))

(ert-deftest ps/vault-test-registry-set-current-adds-unknown-vault ()
  "Switching to a folder that is not in the list yet puts it there."
  (let ((registry (ps/vault--registry-set-current (ps/vault--empty-registry) "/a/new")))
    (should (equal (ps/vault--registry-current registry) "/a/new/"))
    (should (equal (mapcar #'ps/vault--entry-path
                           (ps/vault--registry-vaults registry))
                   '("/a/new/")))))

;;; -------------------------------------------------------
;;; Registry serialization
;;; -------------------------------------------------------

(ert-deftest ps/vault-test-serialize-round-trips ()
  "`ps/vault--deserialize' undoes `ps/vault--serialize' exactly."
  (let ((registry (ps/vault--registry-add
                   (ps/vault-test--registry "/a/one" "/a/two") "/a/one" "First")))
    (should (equal (ps/vault--deserialize (ps/vault--serialize registry))
                   registry))))

(ert-deftest ps/vault-test-serialize-round-trips-empty ()
  "The empty registry survives a round trip too."
  (should (equal (ps/vault--deserialize
                  (ps/vault--serialize (ps/vault--empty-registry)))
                 (ps/vault--empty-registry))))

(ert-deftest ps/vault-test-deserialize-tolerates-garbage ()
  "A corrupt registry costs the vault list, never a working Emacs."
  (should (equal (ps/vault--deserialize "") (ps/vault--empty-registry)))
  (should (equal (ps/vault--deserialize "(:version 1 :vaults") (ps/vault--empty-registry)))
  (should (equal (ps/vault--deserialize "not a plist") (ps/vault--empty-registry)))
  (should (equal (ps/vault--deserialize "(1 2 3)") (ps/vault--empty-registry))))

(ert-deftest ps/vault-test-deserialize-drops-pathless-entries ()
  "An entry with no usable path is dropped rather than poisoning the list."
  (let ((registry (ps/vault--deserialize
                   "(:version 1 :current nil :vaults ((:name \"Broken\") (:path \"/a/one\")))")))
    (should (equal (mapcar #'ps/vault--entry-path
                           (ps/vault--registry-vaults registry))
                   '("/a/one/")))))

(ert-deftest ps/vault-test-deserialize-preserves-unknown-keys ()
  "An older Emacs does not eat settings a newer one wrote."
  (let* ((registry (ps/vault--deserialize
                    "(:version 2 :current nil :vaults ((:path \"/a/one\" :colour \"red\")) :future t)"))
         (entry (car (ps/vault--registry-vaults registry))))
    (should (equal (plist-get registry :future) t))
    (should (equal (plist-get registry :version) 2))
    (should (equal (plist-get entry :colour) "red"))))

;;; -------------------------------------------------------
;;; Labels
;;; -------------------------------------------------------

(ert-deftest ps/vault-test-chip-label-with-no-vault ()
  "With no vault open the chip says so rather than rendering empty."
  (should (equal (ps/vault-chip-label nil) "No vault")))

(ert-deftest ps/vault-test-chip-label-derives-name-from-directory ()
  "A vault with no explicit name is labelled by its folder."
  (should (equal (ps/vault-chip-label "/home/me/Notes/") "Notes"))
  (should (equal (ps/vault-chip-label '(:path "/home/me/Notes/")) "Notes")))

(ert-deftest ps/vault-test-chip-label-prefers-explicit-name ()
  "An explicit name wins over the folder name."
  (should (equal (ps/vault-chip-label '(:path "/home/me/org-2024/" :name "Work"))
                 "Work")))

(ert-deftest ps/vault-test-chip-label-ignores-blank-name ()
  "A blank name is not a name."
  (should (equal (ps/vault-chip-label '(:path "/home/me/Notes/" :name "  "))
                 "Notes")))

(ert-deftest ps/vault-test-chip-label-truncates ()
  "An overlong name is cut to WIDTH columns with an ellipsis."
  (let ((label (ps/vault-chip-label '(:path "/a/b/" :name "Personal Knowledge Base") 12)))
    (should (equal (string-width label) 12))
    (should (string-suffix-p "…" label))))

(ert-deftest ps/vault-test-chip-label-truncates-by-width-not-length ()
  "Truncation counts columns, because a wide glyph takes two of them."
  (let ((label (ps/vault-chip-label '(:path "/a/b/" :name "日本語のノート") 8)))
    (should (<= (string-width label) 8))))

(ert-deftest ps/vault-test-chip-label-leaves-short-names-alone ()
  "A name that fits is untouched -- no stray ellipsis at the limit."
  (should (equal (ps/vault-chip-label '(:path "/a/b/" :name "Notes") 5) "Notes")))

(ert-deftest ps/vault-test-menu-labels-disambiguate-duplicates ()
  "Two vaults both called `notes' are told apart by their parent folder."
  (should (equal (ps/vault-menu-labels
                  '((:path "/home/me/work/notes/")
                    (:path "/home/me/personal/notes/")
                    (:path "/home/me/archive/")))
                 '("notes (work)" "notes (personal)" "archive"))))

;;; -------------------------------------------------------
;;; Per-vault state
;;; -------------------------------------------------------

(ert-deftest ps/vault-test-state-round-trips ()
  "State survives serialization unchanged."
  (let ((state '(:version 1 :file-tree-current-set "Work" :git-sync nil)))
    (should (equal (ps/vault--state-deserialize (ps/vault--state-serialize state))
                   state))))

(ert-deftest ps/vault-test-state-deserialize-tolerates-garbage ()
  "A corrupt state file falls back to defaults instead of signalling."
  (should (equal (ps/vault--state-deserialize "((((") '(:version 1)))
  (should (equal (ps/vault--state-deserialize "") '(:version 1))))

(ert-deftest ps/vault-test-state-get-distinguishes-absent-from-nil ()
  "\"The vault did not say\" is not the same as \"the vault said never\"."
  (should (eq (ps/vault--state-get '(:version 1) :git-sync) :unset))
  (should (eq (ps/vault--state-get '(:version 1 :git-sync nil) :git-sync) nil)))

(ert-deftest ps/vault-test-state-save-and-apply-round-trip ()
  "Saving in one vault and applying restores exactly what was saved."
  (ps/vault-test--with-dir
    (let ((ps/file-tree-current-set "Work")
          (ps/file-tree-set-applies-to-agenda t))
      (ps/vault-state-save dir))
    (let ((ps/file-tree-current-set "All")
          (ps/file-tree-set-applies-to-agenda nil))
      (ps/vault-state-apply dir)
      (should (equal ps/file-tree-current-set "Work"))
      (should (eq ps/file-tree-set-applies-to-agenda t)))))

(ert-deftest ps/vault-test-state-save-preserves-unknown-keys ()
  "Keys this Emacs does not know about survive a save."
  (ps/vault-test--with-dir
    (let ((file (ps/vault-state-path dir)))
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (insert (ps/vault--state-serialize '(:version 1 :future "keep me"))))
      (let ((ps/file-tree-current-set "Work"))
        (ps/vault-state-save dir))
      (should (equal (plist-get (ps/vault-state-load dir) :future) "keep me")))))

(ert-deftest ps/vault-test-state-apply-leaves-unmentioned-settings-alone ()
  "A state file that says nothing about a setting does not reset it."
  (ps/vault-test--with-dir
    (let ((ps/file-tree-current-set "Work"))
      (ps/vault-state-apply dir)
      (should (equal ps/file-tree-current-set "Work")))))

;;; -------------------------------------------------------
;;; Git sync resolution
;;; -------------------------------------------------------

(ert-deftest ps/vault-test-git-repo-p-detects-a-repo ()
  "A directory holding .git is a vault that syncs."
  (ps/vault-test--with-dir
    (should-not (ps/vault-git-repo-p dir))
    (make-directory (expand-file-name ".git" dir))
    (should (ps/vault-git-repo-p dir))))

(ert-deftest ps/vault-test-git-repo-p-accepts-a-git-file ()
  "A worktree or submodule, where .git is a file, counts as a repo."
  (ps/vault-test--with-dir
    (with-temp-file (expand-file-name ".git" dir) (insert "gitdir: /elsewhere\n"))
    (should (ps/vault-git-repo-p dir))))

(ert-deftest ps/vault-test-git-repo-p-rejects-a-subdirectory-of-a-repo ()
  "A vault kept inside a larger checkout must not sync that outer repository.
This is the whole reason detection is a filesystem test rather than
`git rev-parse --show-toplevel', which climbs to the enclosing repo."
  (ps/vault-test--with-dir
    (make-directory (expand-file-name ".git" dir))
    (let ((nested (expand-file-name "notes/" dir)))
      (make-directory nested)
      (should-not (ps/vault-git-repo-p nested)))))

(ert-deftest ps/vault-test-git-sync-auto-follows-detection ()
  "Under `auto' a vault syncs exactly when it is a repo."
  (should (equal (ps/vault-git-sync-setting :unset :unset t 60) 60))
  (should-not (ps/vault-git-sync-setting :unset :unset nil 60)))

(ert-deftest ps/vault-test-git-sync-explicit-nil-beats-a-repo ()
  "A vault can refuse to sync even though it is a git working tree."
  (should-not (ps/vault-git-sync-setting nil :unset t 60))
  (should-not (ps/vault-git-sync-setting :unset nil t 60)))

(ert-deftest ps/vault-test-git-sync-integer-forces-it-on ()
  "An explicit interval syncs regardless of detection."
  (should (equal (ps/vault-git-sync-setting :unset 300 nil 60) 300))
  (should (equal (ps/vault-git-sync-setting 120 :unset nil 60) 120)))

(ert-deftest ps/vault-test-git-sync-state-beats-workspace ()
  "The state file wins over workspace.org, since it is what the UI writes."
  (should-not (ps/vault-git-sync-setting nil 300 t 60))
  (should (equal (ps/vault-git-sync-setting 120 nil t 60) 120)))

;;; -------------------------------------------------------
;;; Vault-scoped defaults
;;; -------------------------------------------------------

(ert-deftest ps/vault-test-every-scoped-variable-has-a-default ()
  "Every vault-scoped variable is captured, or it leaks silently on a switch.
This is the guard against adding a variable to the list and forgetting the
half that makes resetting it work.  Capturing here happens with none of the
`ps/*' modules loaded, so only entries carrying their own default survive --
which is exactly the case the (SYMBOL . DEFAULT) form exists for."
  (let ((ps/vault--defaults nil))
    (ps/vault-capture-defaults)
    (should (assq 'org-agenda-category-icon-alist ps/vault--defaults))))

(ert-deftest ps/vault-test-missing-defaults-accepts-declared-defaults ()
  "An entry that declares its own default is never reported as missing."
  (should-not (ps/vault--missing-defaults '((a . 1)) '((a . 1))))
  (should (equal (ps/vault--missing-defaults '(a (b . 2)) '((b . 2))) '(a))))

(ert-deftest ps/vault-test-capture-uses-a-declared-default-when-unbound ()
  "A variable whose package has not loaded yet still gets a default to reset to."
  (let ((ps/vault-scoped-variables '((ps/vault-test--unbound . "fallback")))
        (ps/vault--defaults nil))
    (ps/vault-capture-defaults)
    (should (equal (alist-get 'ps/vault-test--unbound ps/vault--defaults)
                   "fallback"))))

(ert-deftest ps/vault-test-capture-prefers-the-live-value-to-the-declared-one ()
  "Once the variable is bound, its real `* Settings' value wins."
  (let ((ps/vault-scoped-variables '((ps/vault-test--scoped . "fallback")))
        (ps/vault--defaults nil)
        (ps/vault-test--scoped "live"))
    (ps/vault-capture-defaults)
    (should (equal (alist-get 'ps/vault-test--scoped ps/vault--defaults) "live"))))

(ert-deftest ps/vault-test-missing-defaults-are-reported ()
  "A variable with no captured default is named rather than quietly skipped."
  (should (equal (ps/vault--missing-defaults '(a b c) '((a . 1) (c . 3))) '(b))))

(ert-deftest ps/vault-test-reset-plan-skips-uncaptured-variables ()
  "Guessing at a default is how a switch destroys a setting, so it is skipped."
  (should (equal (ps/vault--reset-plan '(a b c) '((a . 1) (c . 3)))
                 '((a . 1) (c . 3)))))

(ert-deftest ps/vault-test-capture-and-restore-defaults ()
  "A captured default survives being overwritten by a vault."
  (let* ((ps/vault-scoped-variables '(ps/vault-test--scoped))
         (ps/vault--defaults nil)
         (ps/vault-test--scoped '("default")))
    (ps/vault-capture-defaults)
    (setq ps/vault-test--scoped '("from the vault"))
    (ps/vault-restore-defaults)
    (should (equal ps/vault-test--scoped '("default")))))

(ert-deftest ps/vault-test-restore-defaults-does-not-share-structure ()
  "Restoring hands out a copy, so a vault mutating a list cannot poison the default."
  (let* ((ps/vault-scoped-variables '(ps/vault-test--scoped))
         (ps/vault--defaults nil)
         (ps/vault-test--scoped (list "a")))
    (ps/vault-capture-defaults)
    (ps/vault-restore-defaults)
    (setcar ps/vault-test--scoped "mutated")
    (ps/vault-restore-defaults)
    (should (equal ps/vault-test--scoped '("a")))))

;;; -------------------------------------------------------
;;; Validation and scaffolding
;;; -------------------------------------------------------

(ert-deftest ps/vault-test-validate-accepts-an-empty-directory ()
  "A folder with no .org files in it is a perfectly good new vault."
  (ps/vault-test--with-dir
    (should-not (ps/vault-validate dir))))

(ert-deftest ps/vault-test-validate-rejects-a-file ()
  "A regular file is not a vault."
  (ps/vault-test--with-dir
    (let ((file (expand-file-name "notes.org" dir)))
      (with-temp-file file (insert ""))
      (should (ps/vault-validate file)))))

(ert-deftest ps/vault-test-validate-rejects-a-missing-directory ()
  "A folder that is not there yet is reported rather than silently created."
  (should (ps/vault-validate "/nonexistent/ps-vault-test/"))
  (should (ps/vault-validate nil)))

(ert-deftest ps/vault-test-scaffold-writes-the-starter-files ()
  "A new vault gets a workspace.org, an Inbox.org and a state file."
  (ps/vault-test--with-dir
    (ps/vault-scaffold dir "Notes")
    (should (file-exists-p (expand-file-name "workspace.org" dir)))
    (should (file-exists-p (expand-file-name "Inbox.org" dir)))
    (should (file-exists-p (ps/vault-state-path dir)))))

(ert-deftest ps/vault-test-scaffold-writes-nothing-else ()
  "Scaffolding never runs git init, and never generates AGENTS.md or .claude/.
Whether a vault syncs is decided by the user's own `git init'; AGENTS.md is
hand-written and .claude/ is generated by the AI context sync."
  (ps/vault-test--with-dir
    (ps/vault-scaffold dir "Notes")
    (should-not (file-exists-p (expand-file-name ".git" dir)))
    (should-not (file-exists-p (expand-file-name "AGENTS.md" dir)))
    (should-not (file-exists-p (expand-file-name ".claude" dir)))))

(ert-deftest ps/vault-test-scaffold-does-not-overwrite ()
  "Scaffolding an existing folder leaves the notes already in it alone."
  (ps/vault-test--with-dir
    (with-temp-file (expand-file-name "Inbox.org" dir) (insert "mine\n"))
    (ps/vault-scaffold dir "Notes")
    (with-temp-buffer
      (insert-file-contents (expand-file-name "Inbox.org" dir))
      (should (equal (buffer-string) "mine\n")))))

(ert-deftest ps/vault-test-templates-are-readable-elisp ()
  "Every src block in the workspace template reads cleanly.
A typo here would break every new vault at its first load, silently."
  (let ((text (ps/vault--workspace-template "Notes")))
    (should (string-match-p "^#\\+TITLE:" text))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "^#\\+begin_src emacs-lisp\n\\(\\(?:.\\|\n\\)*?\\)#\\+end_src" nil t)
        (let ((body (match-string 1)))
          (with-temp-buffer
            (insert body)
            (goto-char (point-min))
            ;; `read' over the whole block: unbalanced parens signal here.
            (while (ignore-errors (progn (read (current-buffer)) t)))
            (should (looking-at-p "[[:space:]]*\\'"))))))))

(ert-deftest ps/vault-test-starter-template-has-a-subtitle ()
  "The starter file carries the #+SUBTITLE: the AI context index reads."
  (let ((text (ps/vault--starter-template)))
    (should (string-match-p "^#\\+TITLE:" text))
    (should (string-match-p "^#\\+SUBTITLE:" text))))

(ert-deftest ps/vault-test-starter-template-schedules-for-the-given-day ()
  "The starter task is scheduled, so a new vault opens onto a non-empty agenda."
  (let ((text (ps/vault--starter-template (encode-time 0 0 12 15 8 2026))))
    (should (string-match-p "SCHEDULED: <2026-08-15" text))))

;;; -------------------------------------------------------
;;; Registry on disk and bootstrap
;;; -------------------------------------------------------

(ert-deftest ps/vault-test-registry-save-and-load-round-trip ()
  "The registry survives a trip through the filesystem."
  (ps/vault-test--with-home
    (let ((registry (ps/vault-test--registry "/a/one" "/a/two")))
      (ps/vault-registry-save registry)
      (should (equal (ps/vault-registry-load) registry)))))

(ert-deftest ps/vault-test-registry-load-without-a-file ()
  "A first run, with no registry yet, is an empty registry rather than an error."
  (ps/vault-test--with-home
    (should (equal (ps/vault-registry-load) (ps/vault--empty-registry)))))

(ert-deftest ps/vault-test-registry-save-is-suppressed-when-pinned ()
  "A PS_ORG_BASE session must not rewrite the real vault list."
  (ps/vault-test--with-home
    (let ((ps/vault--pinned t))
      (ps/vault-registry-save (ps/vault-test--registry "/a/one")))
    (should-not (file-exists-p (ps/vault-registry-path)))))

(ert-deftest ps/vault-test-bootstrap-opens-the-current-vault ()
  "Startup reopens whichever vault was last in use."
  (ps/vault-test--with-home
    (ps/vault-test--with-dir
      (ps/vault-registry-save (ps/vault-test--registry dir))
      (let (my-org-base-directory)
        (ps/vault-bootstrap)
        (should (equal my-org-base-directory (ps/vault--normalize-path dir)))
        (should (ps/vault-configured-p))))))

(ert-deftest ps/vault-test-bootstrap-skips-a-vanished-vault ()
  "A vault whose folder has gone leaves no vault open rather than a broken one."
  (ps/vault-test--with-home
    (ps/vault-registry-save (ps/vault-test--registry "/nonexistent/ps-vault/"))
    (let (my-org-base-directory)
      (ps/vault-bootstrap)
      (should-not my-org-base-directory)
      (should-not (ps/vault-configured-p))
      (should ps/vault--needs-welcome))))

(ert-deftest ps/vault-test-bootstrap-always-leaves-a-usable-default-directory ()
  "`default-directory' is never nil, whatever happens to the vault."
  (ps/vault-test--with-home
    (let (my-org-base-directory (default-directory "/"))
      (ps/vault-bootstrap)
      (should (stringp default-directory))
      (should (file-directory-p default-directory)))))

(ert-deftest ps/vault-test-bootstrap-migrates-from-local-el ()
  "An existing install migrates its local.el vault into the registry once."
  (ps/vault-test--with-home
    (ps/vault-test--with-dir
      (with-temp-file (expand-file-name "local.el" home)
        (insert (format "(setq my-org-base-directory %S)\n" dir)))
      (let (my-org-base-directory)
        (ps/vault-bootstrap)
        (should (equal my-org-base-directory (ps/vault--normalize-path dir)))
        (should (equal (ps/vault--registry-current (ps/vault-registry-load))
                       (ps/vault--normalize-path dir)))))))

(ert-deftest ps/vault-test-bootstrap-prefers-the-registry-over-local-el ()
  "Once the registry exists, local.el no longer decides which vault opens."
  (ps/vault-test--with-home
    (ps/vault-test--with-dir
      (with-temp-file (expand-file-name "local.el" home)
        (insert "(setq my-org-base-directory \"/nonexistent/from-local/\")\n"))
      (ps/vault-registry-save (ps/vault-test--registry dir))
      (let (my-org-base-directory)
        (ps/vault-bootstrap)
        (should (equal my-org-base-directory (ps/vault--normalize-path dir)))))))

(ert-deftest ps/vault-test-bootstrap-honours-ps-org-base ()
  "PS_ORG_BASE pins the session and leaves the registry untouched."
  (ps/vault-test--with-home
    (ps/vault-test--with-dir
      (let ((process-environment (cons (format "PS_ORG_BASE=%s" dir)
                                       process-environment))
            my-org-base-directory)
        (ps/vault-bootstrap)
        (should (equal my-org-base-directory (ps/vault--normalize-path dir)))
        (should ps/vault--pinned)
        (should-not (file-exists-p (ps/vault-registry-path)))))))

(ert-deftest ps/vault-test-bootstrap-survives-a-corrupt-registry ()
  "A corrupt registry costs the vault list, never a bootable Emacs."
  (ps/vault-test--with-home
    (with-temp-file (ps/vault-registry-path) (insert "(:vaults ((((\n"))
    (let (my-org-base-directory)
      (ps/vault-bootstrap)
      (should-not my-org-base-directory)
      (should (stringp default-directory)))))

(ert-deftest ps/vault-test-bootstrap-normalizes-the-trailing-slash ()
  "The vault path always ends in a slash, so `concat'-style joins stay correct."
  (ps/vault-test--with-home
    (ps/vault-test--with-dir
      (ps/vault-registry-save
       (ps/vault--registry-set-current (ps/vault--empty-registry)
                                       (directory-file-name dir)))
      (let (my-org-base-directory)
        (ps/vault-bootstrap)
        (should (string-suffix-p "/" my-org-base-directory))))))

(provide 'test-ps-vault)
;;; test-ps-vault.el ends here
