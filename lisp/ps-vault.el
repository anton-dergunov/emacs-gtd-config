;;; ps-vault.el --- Vault registry, per-vault state and scaffolding -*- lexical-binding: t; -*-

;;; Commentary:

;; A *vault* is one Org folder: the directory `my-org-base-directory' points at.
;; Everything the system shows -- agenda, file tree, journal, situations -- comes
;; from exactly one vault at a time.  This module owns the list of known vaults,
;; the per-vault machine state, and the scaffolding for a brand new one.  The
;; orchestration of an actual switch lives in `ps-vault-switch'.
;;
;; It is deliberately dependency-free (cl-lib/seq/subr-x only), because
;; `config.org' requires it at the very top of Bootstrap -- before packages are
;; initialised and before the other 37 modules are on `load-path'.  That is also
;; what keeps it testable under `emacs -Q --batch'.
;;
;; Two files hold state, and both are read as *data* with `read', never `load'ed:
;;
;;   <user-emacs-directory>/vaults.eld   the registry -- known vaults + current
;;   <vault>/.ps/state.el                per-vault machine state
;;
;; `load'ing a machine-written file would make it arbitrary eval; parsing a plist
;; can only ever produce data.  Both parsers are total: a truncated or garbage
;; file costs the settings it held, never a working Emacs.  Both preserve keys
;; they do not recognise, so an older Emacs does not eat a newer one's settings.
;;
;; The vault/system settings boundary this module draws:
;;
;;   system      config.org's `* Settings' -- fonts, theme, layout: how things look
;;   vault       <vault>/workspace.org -- icons, file sets, situations: hand-authored
;;   vault       <vault>/.ps/state.el -- what the UI toggles, machine-written
;;
;; `ps/vault-scoped-variables' names the globals a switch must *reset*, not merely
;; re-set.  This matters because `workspace.org' loading is additive and partial:
;; `ps/material-icons-add' merges rather than replaces, and the plain `setq's only
;; take effect if the incoming vault happens to set the same variable.  Without a
;; reset, vault A's categories and situations leak into vault B.  The defaults are
;; *captured* at load time (`ps/vault-capture-defaults', called from config.org
;; immediately before the first `ps/load-workspace-config') rather than restated
;; here, so they cannot drift from `* Settings'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'pp)

(defvar my-org-base-directory)
(defvar desktop-dirname)
(defvar desktop-base-file-name)
(defvar desktop-base-lock-name)

(defgroup ps-vault nil
  "Switching between Org folders (vaults)."
  :group 'ps)

;;; Settings

(defcustom ps/vault-registry-file "vaults.eld"
  "Registry file holding the known vaults, relative to `user-emacs-directory'.
Machine state, not configuration: it is rewritten whenever a vault is added,
removed, renamed or switched to.  Kept out of any vault deliberately -- it is
the one thing that must be readable before a vault is chosen."
  :type 'string
  :group 'ps-vault)

(defcustom ps/vault-state-file ".ps/state.el"
  "Per-vault machine state, relative to the vault directory.
Holds what the UI toggles (the active file set, the git-sync override), as
opposed to the hand-authored settings in the vault's workspace.org.  The
leading dot keeps it out of both the Org scan and the file tree."
  :type 'string
  :group 'ps-vault)

(defcustom ps/vault-chip-max-width 14
  "Width in columns the vault name is truncated to in the file tree header.
The chip is a label, not a path display -- keep it short enough not to
distract from the tree itself."
  :type 'integer
  :group 'ps-vault)

(defcustom ps/vault-git-sync 'auto
  "Whether background git sync runs, as the system-wide default.
`auto' means sync whenever the vault is itself a git working tree -- run
`git init' in a vault to enable it, and set up any remote yourself.  nil
forces it off; an integer forces it on with that interval in seconds.
A vault overrides this in its workspace.org or its state file."
  :type '(choice (const :tag "Automatic (sync if the vault is a git repo)" auto)
                 (const :tag "Never" nil)
                 (integer :tag "Always, with this interval (seconds)"))
  :group 'ps-vault)

(defvar ps/vault-scoped-variables
  '(ps/material-icons-category-map
    ps/material-icons-folder-map
    ps/material-icons-folder-contents-map
    ps/file-tree-file-sets
    ps/file-tree-order
    ps/file-tree-current-set
    ps/file-tree-set-applies-to-agenda
    ps/context-tags
    ps/situations
    org-tag-alist
    (org-agenda-category-icon-alist . nil))
  "Globals a vault switch must reset to their `* Settings' defaults.
Every one of these is either merged into (rather than replaced) by
workspace.org, or set by it only conditionally -- so leaving the old value in
place lets one vault's categories, file sets, tags or situations leak into the
next.

An entry is a SYMBOL, or (SYMBOL . DEFAULT) for one that is not yet bound when
the defaults are captured: `org-agenda-category-icon-alist' is declared by
org-agenda, which has not loaded that early.  A plain symbol that turns out to
be unbound is reported by `ps/vault--missing-defaults' rather than guessed at.")

;;; State

(defvar ps/vault--pinned nil
  "Non-nil when the vault came from the PS_ORG_BASE environment variable.
Registry writes are suppressed while pinned, so a development or test session
cannot rewrite the real vault list.")

(defvar ps/vault--defaults nil
  "Alist of (SYMBOL . VALUE) captured by `ps/vault-capture-defaults'.")

(defvar ps/vault--needs-welcome nil
  "Non-nil when startup finished without a usable vault.")

;;; Paths (pure)

(defun ps/vault--normalize-path (path)
  "Return PATH as an absolute directory name with a trailing slash.
nil and blank strings return nil, so a missing setting stays missing rather
than silently becoming the home directory."
  (when (and (stringp path) (not (string-blank-p path)))
    (file-name-as-directory (expand-file-name path))))

(defun ps/vault-path-under-p (path root)
  "Return non-nil if PATH is ROOT itself or lies inside it.
Compares expanded directory names, so \"/base/\" does not match
\"/base-other/file.org\"."
  (when-let* ((root (ps/vault--normalize-path root))
              (path (and (stringp path) (expand-file-name path))))
    (string-prefix-p root (file-name-as-directory path))))

;;; Registry shape (pure)

(defun ps/vault--plistp (object)
  "Return non-nil if OBJECT is a plist with keyword keys."
  (and (listp object)
       (cl-evenp (length object))
       (cl-loop for (key _value) on object by #'cddr
                always (keywordp key))))

(defun ps/vault--plist-remove (plist key)
  "Return PLIST without KEY."
  (cl-loop for (k v) on plist by #'cddr
           unless (eq k key) append (list k v)))

(defun ps/vault--empty-registry ()
  "Return a registry with no vaults."
  (list :version 1 :current nil :vaults nil))

(defun ps/vault--entry-path (entry)
  "Return the vault path of registry ENTRY, or nil."
  (and (ps/vault--plistp entry) (ps/vault--normalize-path (plist-get entry :path))))

(defun ps/vault--registry-vaults (registry)
  "Return the list of entries in REGISTRY."
  (plist-get registry :vaults))

(defun ps/vault--registry-current (registry)
  "Return the current vault path of REGISTRY, or nil."
  (ps/vault--normalize-path (plist-get registry :current)))

(defun ps/vault--registry-entry (registry path)
  "Return the entry for PATH in REGISTRY, or nil."
  (when-let* ((path (ps/vault--normalize-path path)))
    (seq-find (lambda (entry) (equal (ps/vault--entry-path entry) path))
              (ps/vault--registry-vaults registry))))

(defun ps/vault--registry-add (registry path &optional name)
  "Return REGISTRY with a vault at PATH, named NAME, added if it is new.
An existing entry keeps its position; NAME, when given, updates it.  Order is
otherwise insertion order, which is what the menu and the popup show."
  (let ((path (ps/vault--normalize-path path))
        (registry (copy-sequence registry)))
    (if (null path)
        registry
      (let ((entries (ps/vault--registry-vaults registry)))
        (plist-put
         registry :vaults
         (if (ps/vault--registry-entry registry path)
             (mapcar (lambda (entry)
                       (if (and name (equal (ps/vault--entry-path entry) path))
                           (plist-put (copy-sequence entry) :name name)
                         entry))
                     entries)
           (append entries
                   (list (if name
                             (list :path path :name name)
                           (list :path path))))))))))

(defun ps/vault--registry-remove (registry path)
  "Return REGISTRY without the vault at PATH.
Clears `:current' when PATH was the current vault; use
`ps/vault--registry-fallback' to pick what to open instead."
  (let ((path (ps/vault--normalize-path path))
        (registry (copy-sequence registry)))
    (setq registry
          (plist-put registry :vaults
                     (seq-remove (lambda (entry)
                                   (equal (ps/vault--entry-path entry) path))
                                 (ps/vault--registry-vaults registry))))
    (if (equal (ps/vault--registry-current registry) path)
        (plist-put registry :current nil)
      registry)))

(defun ps/vault--registry-rename (registry path name)
  "Return REGISTRY with the vault at PATH renamed to NAME.
A blank NAME drops the explicit name, so the label falls back to the
directory's own name -- renaming to nothing has to mean something."
  (let ((path (ps/vault--normalize-path path))
        (name (and (stringp name) (not (string-blank-p name)) name)))
    (plist-put (copy-sequence registry) :vaults
               (mapcar (lambda (entry)
                         (if (not (equal (ps/vault--entry-path entry) path))
                             entry
                           (if name
                               (plist-put (copy-sequence entry) :name name)
                             (ps/vault--plist-remove entry :name))))
                       (ps/vault--registry-vaults registry)))))

(defun ps/vault--registry-set-current (registry path)
  "Return REGISTRY with PATH as the current vault, adding it if it is new."
  (let ((path (ps/vault--normalize-path path)))
    (plist-put (ps/vault--registry-add registry path) :current path)))

(defun ps/vault--registry-fallback (registry)
  "Return the vault REGISTRY should open: the current one, else the first."
  (or (ps/vault--registry-current registry)
      (ps/vault--entry-path (car (ps/vault--registry-vaults registry)))))

;;; Registry serialization (pure)

(defun ps/vault--normalize-entry (entry)
  "Return ENTRY with its `:path' normalized, or nil if it has no usable path.
Keys other than `:path' are passed through untouched."
  (when-let* ((path (ps/vault--entry-path entry)))
    (plist-put (copy-sequence entry) :path path)))

(defun ps/vault--serialize (registry)
  "Return REGISTRY as the text of a registry file."
  (concat ";; -*- mode: lisp-data; -*-\n"
          ";; Vaults known to this Emacs.  Written by ps-vault.el; safe to edit\n"
          ";; by hand while Emacs is closed.\n"
          (pp-to-string registry)))

(defun ps/vault--deserialize (string)
  "Parse STRING as a registry, returning a registry plist.
Never signals: unparseable or ill-shaped content yields an empty registry,
because losing the vault list is recoverable and an unbootable Emacs is not.
Entries without a usable path are dropped; other keys are preserved."
  (let ((sexp (condition-case nil
                  (car (read-from-string string))
                (error nil))))
    (if (not (and sexp (ps/vault--plistp sexp)))
        (ps/vault--empty-registry)
      (let ((registry (copy-sequence sexp)))
        (setq registry
              (plist-put registry :vaults
                         (let ((entries (plist-get registry :vaults)))
                           (delq nil (mapcar #'ps/vault--normalize-entry
                                             (and (listp entries) entries))))))
        (setq registry
              (plist-put registry :current
                         (ps/vault--normalize-path (plist-get registry :current))))
        (unless (plist-get registry :version)
          (setq registry (plist-put registry :version 1)))
        registry))))

;;; Labels (pure)

(defun ps/vault--directory-name (path)
  "Return the last component of directory PATH, e.g. \"notes\" for \"/a/notes/\"."
  (when-let* ((path (ps/vault--normalize-path path)))
    (file-name-nondirectory (directory-file-name path))))

(defun ps/vault-entry-name (entry)
  "Return the display name of ENTRY, a registry entry plist or a path string.
An explicit `:name' wins; otherwise the vault directory's own name is used."
  (let* ((plist (and (ps/vault--plistp entry) entry))
         (path (if plist (ps/vault--entry-path entry) entry))
         (name (and plist (plist-get entry :name))))
    (if (and (stringp name) (not (string-blank-p name)))
        name
      (ps/vault--directory-name path))))

(defun ps/vault-chip-label (entry &optional width)
  "Return the header-line label for ENTRY, truncated to WIDTH columns.
ENTRY is a registry entry plist or a path string; nil means no vault is open.
WIDTH defaults to `ps/vault-chip-max-width'.  Measured in columns rather than
characters, because the header line lays out in columns and a name may well
not be ASCII."
  (let ((name (or (ps/vault-entry-name entry) "No vault"))
        (width (max 2 (or width ps/vault-chip-max-width))))
    (if (<= (string-width name) width)
        name
      (truncate-string-to-width name width nil nil "…"))))

(defun ps/vault-menu-labels (vaults)
  "Return a display label for each entry of VAULTS, in order.
Names shared by several vaults are disambiguated with the vault's parent
directory -- two folders both called \"notes\" are a normal way to organise
these, and an unqualified menu of them is unusable."
  (let ((names (mapcar #'ps/vault-entry-name vaults)))
    (cl-mapcar
     (lambda (entry name)
       (if (or (null name) (< (seq-count (lambda (n) (equal n name)) names) 2))
           (or name "?")
         (let ((parent (ps/vault--directory-name
                        (file-name-directory
                         (directory-file-name (ps/vault--entry-path entry))))))
           (if parent (format "%s (%s)" name parent) name))))
     vaults names)))

;;; Per-vault state (pure)

(defun ps/vault--state-serialize (state)
  "Return STATE as the text of a per-vault state file."
  (concat ";; -*- mode: lisp-data; -*-\n"
          ";; Machine-written state for this vault.  Hand-authored settings\n"
          ";; belong in workspace.org beside it.\n"
          (pp-to-string state)))

(defun ps/vault--state-deserialize (string)
  "Parse STRING as per-vault state, returning a plist.
Like the registry parser: total, and preserving unrecognised keys."
  (let ((sexp (condition-case nil
                  (car (read-from-string string))
                (error nil))))
    (if (and sexp (ps/vault--plistp sexp))
        (copy-sequence sexp)
      (list :version 1))))

(defun ps/vault--state-get (state key)
  "Return KEY from STATE, or `:unset' if STATE does not mention it.
The distinction matters for `:git-sync', where an explicit nil means \"never
sync this vault\" and an absent key means \"decide as usual\"."
  (if (plist-member state key) (plist-get state key) :unset))

;;; Git sync resolution (pure)

(defun ps/vault-git-sync-setting (state workspace repo-p interval)
  "Return the sync interval in seconds for a vault, or nil to not sync.
STATE and WORKSPACE are the vault's overrides -- each nil (never), an integer
\(always, with that interval), the symbol `auto', or `:unset' when the vault
does not say.  The state file wins over workspace.org, since it is what the UI
writes.  Under `auto' a vault syncs exactly when REPO-P says it is a git
working tree, at INTERVAL seconds."
  (let ((choice (cond ((not (eq state :unset)) state)
                      ((not (eq workspace :unset)) workspace)
                      (t 'auto))))
    (cond ((integerp choice) choice)
          ((null choice) nil)
          (repo-p interval)
          (t nil))))

;;; Vault-scoped defaults (pure)

(defun ps/vault--scoped-symbol (entry)
  "Return the variable named by ENTRY of `ps/vault-scoped-variables'."
  (if (consp entry) (car entry) entry))

(defun ps/vault--missing-defaults (variables defaults)
  "Return the members of VARIABLES that DEFAULTS has no captured value for."
  (seq-remove (lambda (entry) (assq (ps/vault--scoped-symbol entry) defaults))
              (mapcar #'ps/vault--scoped-symbol variables)))

(defun ps/vault--reset-plan (variables defaults)
  "Return the (SYMBOL . VALUE) assignments restoring VARIABLES from DEFAULTS.
Variables with no captured default are skipped rather than reset to nil --
guessing at a default is how a switch silently destroys a setting."
  (delq nil (mapcar (lambda (entry)
                      (assq (ps/vault--scoped-symbol entry) defaults))
                    variables)))

;;; Templates (pure)

(defun ps/vault--workspace-template (name)
  "Return the text of a starter workspace.org for a vault called NAME.
Everything real is commented out: this file is hand-authored from here on, and
an empty-but-documented one is easier to extend than a guessed-at one."
  (format "#+TITLE: Workspace settings for %s

Settings that belong to *this vault* -- how its content is organised. They live
here rather than in =config.org= so they travel with the folder and never
conflict when you pull upstream changes. System-wide settings (fonts, theme,
layout) stay in =config.org= under =* Settings=.

Edit and reload from =Productivity → Config → Workspace= (=C-c p W= / =C-c p w=).

* Category icons
Each pair maps a =<Name>.org= file's basename to a Material Symbols name from
https://fonts.google.com/icons. Unmapped files show the generic file icon.
#+begin_src emacs-lisp
  ;; (ps/material-icons-add
  ;;  '((\"Inbox\" . \"drafts\")
  ;;    (\"Work\"  . \"badge\")))
#+end_src

* Whole-folder icons
#+begin_src emacs-lisp
  ;; (setq ps/material-icons-folder-map
  ;;       '((\"Current\" . \"calendar_month\")))
#+end_src

* File sets
=:include= / =:exclude= are lists of regexps matched as substrings of each file's
absolute path. Switch sets from the file tree's mode line.
#+begin_src emacs-lisp
  (setq ps/file-tree-file-sets
        '((\"All\" . (:include nil :exclude nil))))
#+end_src

* Situations
Context tags and the saved searches over them. See =docs/Situations.org=.
#+begin_src emacs-lisp
  ;; (setq ps/context-tags '((\"@home\" . \"At home\")))
  ;; (setq ps/situations '((\"Home\" :tags (\"@home\"))))
#+end_src

* Git sync
This vault syncs automatically when it is a git working tree -- run =git init=
here and add a remote to enable it. Uncomment to override that for this vault
only: =nil= never syncs, an integer syncs every that many seconds.
#+begin_src emacs-lisp
  ;; (setq ps/vault-git-sync nil)
#+end_src
" name))

(defun ps/vault--starter-template (&optional time)
  "Return the text of a starter Inbox.org, with a task scheduled for TIME.
One real task, so a brand new vault opens onto an agenda with something in it
rather than an empty buffer that looks like a failure."
  (let ((today (format-time-string "%Y-%m-%d %a" (or time (current-time)))))
    (format "#+TITLE: Inbox
#+SUBTITLE: Anything not yet sorted anywhere else

* TODO Look around the new vault
SCHEDULED: <%s>
Everything in this folder is yours to rename, move or delete.
" today)))

;;; Registry access (impure)

(defun ps/vault-registry-path ()
  "Return the absolute path of the registry file."
  (expand-file-name ps/vault-registry-file user-emacs-directory))

(defun ps/vault-registry-load ()
  "Return the registry read from disk, or an empty one."
  (let ((file (ps/vault-registry-path)))
    (if (file-readable-p file)
        (ps/vault--deserialize
         (with-temp-buffer
           (insert-file-contents file)
           (buffer-string)))
      (ps/vault--empty-registry))))

(defun ps/vault-registry-save (registry)
  "Write REGISTRY to disk and return it.
A no-op while `ps/vault--pinned', so a PS_ORG_BASE session cannot rewrite the
real vault list."
  (unless ps/vault--pinned
    (let ((file (ps/vault-registry-path)))
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (insert (ps/vault--serialize registry)))))
  registry)

(defun ps/vault-registry-update (function)
  "Apply FUNCTION to the registry, save the result, and return it."
  (ps/vault-registry-save (funcall function (ps/vault-registry-load))))

(defun ps/vault-known ()
  "Return the known vault entries, in registry order."
  (ps/vault--registry-vaults (ps/vault-registry-load)))

(defun ps/vault-current ()
  "Return the path of the vault currently open, or nil."
  (ps/vault--normalize-path (and (boundp 'my-org-base-directory)
                                 my-org-base-directory)))

(defun ps/vault-configured-p ()
  "Return non-nil when a usable vault is open.
Used as the guard everywhere a directory-dependent feature would otherwise
operate on nil.  Tests the value and the directory, not just whether the
variable is bound -- after this module runs, it is always bound."
  (when-let* ((root (ps/vault-current)))
    (file-directory-p root)))

(defun ps/vault-entry (&optional path)
  "Return the registry entry for PATH, defaulting to the current vault."
  (ps/vault--registry-entry (ps/vault-registry-load)
                            (or path (ps/vault-current))))

(defun ps/vault-name (&optional path)
  "Return the display name of the vault at PATH, defaulting to the current one."
  (let ((path (or path (ps/vault-current))))
    (and path (ps/vault-entry-name (or (ps/vault-entry path) path)))))

;;; Git detection (impure)

(defun ps/vault-git-repo-p (&optional root)
  "Return non-nil if ROOT is itself a git working tree.
Deliberately a filesystem test rather than `git rev-parse --show-toplevel':
that climbs to the *enclosing* repository, so a vault kept inside a larger
checkout would sync -- and push -- that outer repository instead.  Tests for
`.git' with `file-exists-p' rather than `file-directory-p', so worktrees and
submodules, where `.git' is a file, are recognised."
  (when-let* ((root (ps/vault--normalize-path (or root (ps/vault-current)))))
    (file-exists-p (expand-file-name ".git" root))))

;;; Per-vault state (impure)

(defun ps/vault-state-path (&optional root)
  "Return the state file path for the vault at ROOT."
  (when-let* ((root (ps/vault--normalize-path (or root (ps/vault-current)))))
    (expand-file-name ps/vault-state-file root)))

(defun ps/vault-state-load (&optional root)
  "Return the state plist stored in the vault at ROOT."
  (let ((file (ps/vault-state-path root)))
    (if (and file (file-readable-p file))
        (ps/vault--state-deserialize
         (with-temp-buffer
           (insert-file-contents file)
           (buffer-string)))
      (list :version 1))))

(defvar ps/vault-state-variables
  '((:file-tree-current-set . ps/file-tree-current-set)
    (:file-tree-set-applies-to-agenda . ps/file-tree-set-applies-to-agenda))
  "Alist mapping a state-file key to the global it restores.
These are per-vault by nature -- a file set named in one vault means nothing in
another -- which is why they moved out of savehist, which is one list shared by
every vault.")

(defun ps/vault-state-save (&optional root)
  "Write the current values of `ps/vault-state-variables' into ROOT's state file.
Keys the running Emacs does not know about are carried over from the existing
file untouched."
  (when-let* ((file (ps/vault-state-path root)))
    (let ((state (ps/vault-state-load root)))
      (pcase-dolist (`(,key . ,symbol) ps/vault-state-variables)
        (when (boundp symbol)
          (setq state (plist-put state key (symbol-value symbol)))))
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (insert (ps/vault--state-serialize state)))
      state)))

(defun ps/vault-state-apply (&optional root)
  "Set the globals named by `ps/vault-state-variables' from ROOT's state file.
Keys the file does not mention are left alone, so they keep the default that
`* Settings' gave them."
  (let ((state (ps/vault-state-load root)))
    (pcase-dolist (`(,key . ,symbol) ps/vault-state-variables)
      (let ((value (ps/vault--state-get state key)))
        (unless (eq value :unset)
          (set symbol value))))
    state))

;;;###autoload
(defun ps/vault-state-setup ()
  "Save the current vault's state when Emacs exits.
Without this the state file is only written on a vault switch, so a setting
changed and then never switched away from would be lost on quit."
  (add-hook 'kill-emacs-hook #'ps/vault-state-save-quietly))

(defun ps/vault-state-save-quietly ()
  "Save the current vault's state, ignoring any error.
Runs from `kill-emacs-hook', where signalling would block the exit."
  (ignore-errors (when (ps/vault-configured-p) (ps/vault-state-save))))

;;;###autoload
(defun ps/vault-desktop-setup (&optional root)
  "Point `desktop-save-mode' at a desktop file belonging to the vault at ROOT.
The desktop restores buffers by absolute path, so one file shared between
vaults reopens the previous vault's files after a switch.  Named by a hash of
the vault path rather than by its name, so renaming a vault or having two
called the same thing cannot collide -- and kept under `user-emacs-directory',
because machine state does not belong in a folder that is synced between
machines."
  (require 'desktop)
  (let* ((root (or root (ps/vault-current)))
         (suffix (if root (substring (secure-hash 'sha1 root) 0 10) "none")))
    (setq desktop-dirname (file-name-as-directory user-emacs-directory))
    (setq desktop-base-file-name (format ".emacs.desktop-%s" suffix))
    (setq desktop-base-lock-name (format "%s.lock" desktop-base-file-name))))

(defun ps/vault-git-sync-interval (&optional root)
  "Return the sync interval for the vault at ROOT, or nil to not sync.
Resolves the state file's override, then workspace.org's `ps/vault-git-sync',
then automatic detection."
  (let* ((state (ps/vault--state-get (ps/vault-state-load root) :git-sync))
         (workspace (if (eq ps/vault-git-sync 'auto) :unset ps/vault-git-sync))
         (interval (if (boundp 'ps/git-sync-interval)
                       (symbol-value 'ps/git-sync-interval)
                     60)))
    (ps/vault-git-sync-setting state workspace (ps/vault-git-repo-p root) interval)))

;;; Vault-scoped defaults (impure)

(defun ps/vault-capture-defaults ()
  "Snapshot the `* Settings' values of `ps/vault-scoped-variables'.
Call once from config.org, immediately before the first
`ps/load-workspace-config' -- at that point `* Settings' has run and no vault
has yet contributed anything.  Capturing rather than restating the defaults is
what stops them drifting from `* Settings'."
  (setq ps/vault--defaults
        (delq nil (mapcar
                   (lambda (entry)
                     (let ((symbol (ps/vault--scoped-symbol entry)))
                       (cond ((boundp symbol)
                              (cons symbol (copy-tree (symbol-value symbol))))
                             ;; Declared default, for a variable whose own
                             ;; package has not loaded this early.
                             ((consp entry) (cons symbol (copy-tree (cdr entry))))
                             (t nil))))
                   ps/vault-scoped-variables)))
  (when-let* ((missing (ps/vault--missing-defaults ps/vault-scoped-variables
                                                   ps/vault--defaults)))
    (message "ps-vault: no default captured for %s" missing))
  ps/vault--defaults)

(defun ps/vault-restore-defaults ()
  "Reset `ps/vault-scoped-variables' to their captured defaults.
The teardown half of a vault switch: without it the outgoing vault's icons,
file sets, tags and situations survive into the incoming one, because
workspace.org merges into some of these and sets others only conditionally."
  (pcase-dolist (`(,symbol . ,value)
                 (ps/vault--reset-plan ps/vault-scoped-variables ps/vault--defaults))
    (set symbol (copy-tree value))))

;;; Creating and validating vaults (impure)

(defun ps/vault-validate (directory)
  "Return nil if DIRECTORY can be opened as a vault, or an explanation why not.
A directory already in the registry is fine -- opening it is just a switch --
and one with no .org files in it is fine too; only what cannot work at all is
refused."
  (let ((path (ps/vault--normalize-path directory)))
    (cond
     ((null path) "No directory given")
     ((and (file-exists-p path) (not (file-directory-p path)))
      (format "%s is a file, not a directory" (directory-file-name path)))
     ((not (file-exists-p path)) (format "%s does not exist" path))
     ((not (file-writable-p (directory-file-name path)))
      (format "%s is not writable" path))
     (t nil))))

(defun ps/vault-scaffold (directory &optional name time)
  "Create the starter files for a new vault in DIRECTORY, named NAME.
Writes a workspace.org, an Inbox.org and a state file, and nothing else.  In
particular it does not run `git init' -- whether a vault syncs is decided by
whether you made it a repository yourself -- and it never writes an AGENTS.md
or a .claude directory, which are hand-written and generated respectively.
Existing files are left alone, so this is safe to run on a folder that already
holds notes.  TIME is passed to `ps/vault--starter-template'."
  (let* ((root (ps/vault--normalize-path directory))
         (name (or name (ps/vault--directory-name root)))
         (written nil))
    (make-directory root t)
    (pcase-dolist (`(,file . ,text)
                   (list (cons "workspace.org" (ps/vault--workspace-template name))
                         (cons "Inbox.org" (ps/vault--starter-template time))))
      (let ((path (expand-file-name file root)))
        (unless (file-exists-p path)
          (with-temp-file path (insert text))
          (push path written))))
    (let ((state (expand-file-name ps/vault-state-file root)))
      (unless (file-exists-p state)
        (make-directory (file-name-directory state) t)
        (with-temp-file state
          (insert (ps/vault--state-serialize (list :version 1))))
        (push state written)))
    (nreverse written)))

;;; Bootstrap (impure)

(defun ps/vault--seed-from-local-file ()
  "Return the vault named by local.el, loading it for its side effects.
The pre-vault way of naming the Org folder.  It is consulted only when no
registry exists yet, so an existing installation migrates into the registry on
its first start and local.el then stops mattering."
  (let ((file (expand-file-name "local.el" user-emacs-directory)))
    (when (file-readable-p file)
      (load file nil t)
      (and (boundp 'my-org-base-directory)
           (ps/vault--normalize-path my-org-base-directory)))))

(defun ps/vault--resolve ()
  "Return the vault to open at startup, or nil, and record how it was chosen."
  (let ((pinned (ps/vault--normalize-path (getenv "PS_ORG_BASE"))))
    (cond
     ;; A pinned session (development, tests) must not touch the real registry.
     ((and pinned (file-directory-p pinned))
      (setq ps/vault--pinned t)
      pinned)
     ((file-readable-p (ps/vault-registry-path))
      (let ((current (ps/vault--registry-fallback (ps/vault-registry-load))))
        (and current (file-directory-p current) current)))
     (t
      (when-let* ((seed (ps/vault--seed-from-local-file)))
        (when (file-directory-p seed)
          (ps/vault-registry-save
           (ps/vault--registry-set-current (ps/vault--empty-registry) seed))
          seed))))))

;;;###autoload
(defun ps/vault-bootstrap ()
  "Set `my-org-base-directory' from the registry.  Call once, from config.org.
Runs before packages and before the other modules load, so it must not fail:
any error falls back to whatever local.el names, and a vault that cannot be
resolved leaves `my-org-base-directory' nil, which every directory-dependent
feature guards for with `ps/vault-configured-p'.  Always leaves
`default-directory' pointing somewhere real, and always leaves the vault path
ending in a slash."
  (let ((root (condition-case error
                  (ps/vault--resolve)
                (error
                 (message "ps-vault: could not read the vault registry (%s)"
                          (error-message-string error))
                 (ignore-errors (ps/vault--seed-from-local-file))))))
    (setq my-org-base-directory (ps/vault--normalize-path root))
    (setq default-directory (or my-org-base-directory
                                (file-name-as-directory user-emacs-directory)))
    (setq ps/vault--needs-welcome (not (ps/vault-configured-p)))
    (when (and my-org-base-directory (not ps/vault--pinned))
      (ps/vault-registry-update
       (lambda (registry)
         (ps/vault--registry-set-current registry my-org-base-directory))))
    my-org-base-directory))

(provide 'ps-vault)
;;; ps-vault.el ends here
