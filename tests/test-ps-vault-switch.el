;;; test-ps-vault-switch.el --- ERT tests for ps-vault-switch -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path "lisp")
(require 'ps-vault-switch)

;; See the note in test-ps-vault.el: a bare `defvar' in another file only marks
;; the symbol special there, so repeat it for the `let' bindings below.
(defvar my-org-base-directory)
(defvar org-journal-dir)
(defvar ps/file-tree-current-set)
(defvar ps/file-tree-file-sets)

(defmacro ps/vault-switch-test--with-vaults (&rest body)
  "Bind `home', `one' and `two' to fresh directories with a registry in place.
`one' is the open vault.  Everything real (treemacs, git sync, the agenda) is
absent under `-Q', so the switch exercises its own steps and skips the rest
through the `fboundp' guards it already uses."
  (declare (indent 0))
  `(let* ((home (file-name-as-directory (make-temp-file "ps-vault-home-" t)))
          (one (file-name-as-directory (make-temp-file "ps-vault-one-" t)))
          (two (file-name-as-directory (make-temp-file "ps-vault-two-" t)))
          (user-emacs-directory home)
          (ps/vault--pinned nil)
          (ps/vault--defaults nil)
          (ps/vault-file-tree-init-delays nil)
          (my-org-base-directory one)
          (org-journal-dir nil)
          (default-directory one))
     (unwind-protect
         (progn
           (ps/vault-registry-save
            (ps/vault--registry-set-current (ps/vault--empty-registry) one))
           ,@body)
       (delete-directory home t)
       (delete-directory one t)
       (delete-directory two t))))

;;; -------------------------------------------------------
;;; ps/vault-apply
;;; -------------------------------------------------------

(ert-deftest ps/vault-switch-test-apply-moves-the-base-directory ()
  "Applying a vault moves every setting derived from the directory at once."
  (ps/vault-switch-test--with-vaults
    (ps/vault-apply two)
    (should (equal my-org-base-directory two))
    (should (equal default-directory two))
    (should (equal org-journal-dir (expand-file-name "Journal/" two)))))

(ert-deftest ps/vault-switch-test-apply-records-the-current-vault ()
  "The registry remembers the new vault, so the next start reopens it."
  (ps/vault-switch-test--with-vaults
    (ps/vault-apply two)
    (should (equal (ps/vault--registry-current (ps/vault-registry-load)) two))))

(ert-deftest ps/vault-switch-test-apply-adds-an-unknown-vault ()
  "Applying a folder that was not in the list puts it there."
  (ps/vault-switch-test--with-vaults
    (ps/vault-apply two)
    (should (member two (mapcar #'ps/vault--entry-path (ps/vault-known))))))

(ert-deftest ps/vault-switch-test-apply-resets-vault-scoped-settings ()
  "The outgoing vault's settings do not survive into the incoming one.
This is the leak the whole reset machinery exists for: workspace.org merges
into some of these and sets others only conditionally."
  (ps/vault-switch-test--with-vaults
    (let ((ps/vault-scoped-variables '(ps/file-tree-current-set))
          (ps/file-tree-current-set "All"))
      (ps/vault-capture-defaults)
      (setq ps/file-tree-current-set "Work")   ; as if vault one's workspace.org set it
      (ps/vault-apply two)
      (should (equal ps/file-tree-current-set "All")))))

(ert-deftest ps/vault-switch-test-apply-restores-the-new-vault-state ()
  "A vault's own state file wins over the reset default.
The set has to be one the vault actually defines: a state file naming a set
that is not there falls back to \"All\", which is the next test."
  (ps/vault-switch-test--with-vaults
    (let ((ps/vault-scoped-variables '(ps/file-tree-current-set))
          (ps/file-tree-file-sets '(("All") ("Reading")))
          (ps/file-tree-current-set "All"))
      (ps/vault-capture-defaults)
      (let ((ps/file-tree-current-set "Reading"))
        (ps/vault-state-save two))
      (ps/vault-apply two)
      (should (equal ps/file-tree-current-set "Reading")))))

(ert-deftest ps/vault-switch-test-apply-drops-a-file-set-the-vault-lacks ()
  "A set remembered from another vault falls back rather than filtering to nothing."
  (skip-unless (fboundp 'ps/file-tree--ensure-valid-set))
  (ps/vault-switch-test--with-vaults
    (let ((ps/file-tree-file-sets '(("All")))
          (ps/file-tree-current-set "All"))
      (let ((ps/file-tree-current-set "Reading"))
        (ps/vault-state-save two))
      (ps/vault-apply two)
      (should (equal ps/file-tree-current-set "All")))))

(ert-deftest ps/vault-switch-test-apply-saves-the-outgoing-state ()
  "Leaving a vault writes its state, so coming back restores it."
  (ps/vault-switch-test--with-vaults
    (let ((ps/file-tree-current-set "Work"))
      (ps/vault-apply two)
      (should (equal (plist-get (ps/vault-state-load one) :file-tree-current-set)
                     "Work")))))

(ert-deftest ps/vault-switch-test-apply-saves-modified-files ()
  "An unsaved edit is written out, never dropped with the buffer."
  (ps/vault-switch-test--with-vaults
    (let ((file (expand-file-name "Notes.org" one)))
      (with-temp-file file (insert "original\n"))
      (let ((buffer (find-file-noselect file)))
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "added\n"))
        (ps/vault-apply two)
        (should-not (buffer-live-p buffer)))
      (with-temp-buffer
        (insert-file-contents file)
        (should (equal (buffer-string) "original\nadded\n"))))))

(ert-deftest ps/vault-switch-test-apply-kills-the-old-vault-buffers ()
  "Files from the vault we left do not stay open in the one we arrived at."
  (ps/vault-switch-test--with-vaults
    (let ((file (expand-file-name "Notes.org" one)))
      (with-temp-file file (insert "x\n"))
      (let ((buffer (find-file-noselect file)))
        (ps/vault-apply two)
        (should-not (buffer-live-p buffer))))))

(ert-deftest ps/vault-switch-test-apply-leaves-other-buffers-alone ()
  "Buffers outside the vault -- scratch, config, anything else -- survive."
  (ps/vault-switch-test--with-vaults
    (let ((buffer (get-buffer-create "*ps-vault-test-scratch*")))
      (unwind-protect
          (progn
            (ps/vault-apply two)
            (should (buffer-live-p buffer)))
        (kill-buffer buffer)))))

(ert-deftest ps/vault-switch-test-apply-refuses-a-non-directory ()
  "Applying something that is not a directory fails loudly and changes nothing."
  (ps/vault-switch-test--with-vaults
    (should-error (ps/vault-apply "/nonexistent/ps-vault-switch/") :type 'user-error)
    (should (equal my-org-base-directory one))))

(ert-deftest ps/vault-switch-test-apply-survives-a-broken-step ()
  "One failing step is reported, and the rest of the switch still completes."
  (ps/vault-switch-test--with-vaults
    (cl-letf (((symbol-function 'ps/vault-restore-defaults)
               (lambda () (error "boom"))))
      (ps/vault-apply two)
      (should (equal my-org-base-directory two)))))

;;; -------------------------------------------------------
;;; ps/vault-switch and the list commands
;;; -------------------------------------------------------

(ert-deftest ps/vault-switch-test-switching-to-the-open-vault-is-a-no-op ()
  "Re-selecting the vault already open does not tear it down and rebuild it."
  (ps/vault-switch-test--with-vaults
    (let ((file (expand-file-name "Notes.org" one)))
      (with-temp-file file (insert "x\n"))
      (let ((buffer (find-file-noselect file)))
        (unwind-protect
            (progn
              (ps/vault-switch one)
              (should (buffer-live-p buffer)))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest ps/vault-switch-test-forget-falls-back-to-another-vault ()
  "Removing the open vault opens whichever one is left."
  (ps/vault-switch-test--with-vaults
    (ps/vault-apply two)                    ; now: one, two -- two open
    (ps/vault-forget two)
    (should (equal my-org-base-directory one))
    (should-not (member two (mapcar #'ps/vault--entry-path (ps/vault-known))))))

(ert-deftest ps/vault-switch-test-forget-a-vault-that-is-not-open ()
  "Removing a vault we are not in leaves the open one alone."
  (ps/vault-switch-test--with-vaults
    (ps/vault-apply two)
    (ps/vault-forget one)
    (should (equal my-org-base-directory two))))

(ert-deftest ps/vault-switch-test-create-scaffolds-and-opens ()
  "Creating a vault sets it up and switches to it in one step."
  (ps/vault-switch-test--with-vaults
    (ps/vault-create two "Second")
    (should (equal my-org-base-directory two))
    (should (file-exists-p (expand-file-name "workspace.org" two)))
    (should (equal (ps/vault-name) "Second"))))

(ert-deftest ps/vault-switch-test-rename-changes-the-label-only ()
  "Renaming relabels the vault; the folder on disk keeps its own name."
  (ps/vault-switch-test--with-vaults
    (ps/vault-rename "Personal")
    (should (equal (ps/vault-name) "Personal"))
    (should (file-directory-p one))
    (should (equal my-org-base-directory one))))

;;; -------------------------------------------------------
;;; Chip and menu
;;; -------------------------------------------------------

(ert-deftest ps/vault-switch-test-chip-is-clickable-on-the-header-line ()
  "The chip binds mouse-1 on `header-line', not `mode-line'.
Binding the wrong one leaves a chip that looks live and does nothing."
  (ps/vault-switch-test--with-vaults
    (let* ((chip (ps/vault--chip))
           (map (get-text-property 0 'local-map chip)))
      (should (eq (lookup-key map [header-line mouse-1]) #'ps/vault--chip-click))
      (should-not (commandp (lookup-key map [mode-line mouse-1]))))))

(ert-deftest ps/vault-switch-test-chip-shows-the-vault-name ()
  "The chip reads as a name with a dropdown marker."
  (ps/vault-switch-test--with-vaults
    (ps/vault-rename "Work")
    (should (string-match-p "Work ▾" (ps/vault--chip)))))

(ert-deftest ps/vault-switch-test-chip-menu-marks-the-open-vault ()
  "Exactly one row of the popup is bulleted, and it is the open vault."
  (ps/vault-switch-test--with-vaults
    (ps/vault-apply two)
    (let* ((items (ps/vault--chip-menu-items))
           (marked (seq-filter (lambda (row) (string-prefix-p "●" (car row))) items)))
      (should (= (length marked) 1))
      (should (equal (cdr (car marked)) two)))))

(ert-deftest ps/vault-switch-test-chip-menu-offers-the-list-commands ()
  "Add, open, rename and remove are always reachable from the popup."
  (ps/vault-switch-test--with-vaults
    (let ((values (mapcar #'cdr (ps/vault--chip-menu-items))))
      (dolist (key '(:create :open :rename :forget))
        (should (memq key values))))))

(ert-deftest ps/vault-switch-test-menu-filter-lists-every-vault ()
  "The menu is rebuilt from the vault list each time it opens."
  (ps/vault-switch-test--with-vaults
    (ps/vault-apply two)
    (let ((labels (mapcar (lambda (item) (and (vectorp item) (aref item 0)))
                          (ps/vault--menu-filter))))
      (should (member (ps/vault--directory-name one) labels))
      (should (member (ps/vault--directory-name two) labels)))))

(ert-deftest ps/vault-switch-test-menu-filter-with-no-vaults ()
  "An empty vault list still yields a usable menu, not an empty one."
  (let ((home (file-name-as-directory (make-temp-file "ps-vault-home-" t))))
    (unwind-protect
        (let ((user-emacs-directory home)
              (my-org-base-directory nil))
          (let ((items (ps/vault--menu-filter)))
            (should (equal (aref (car items) 0) "No vaults yet"))
            (should (member "---" items))))
      (delete-directory home t))))

;;; -------------------------------------------------------
;;; The treemacs settle ladder
;;; -------------------------------------------------------

(ert-deftest ps/vault-switch-test-file-tree-ladder-is-cancelled-on-reswitch ()
  "A second switch cancels the first one's timers, so they cannot re-root back.
Without this, the ladder from the vault we just left keeps firing for three
seconds and re-adds its project to the tree."
  (ps/vault-switch-test--with-vaults
    (let ((ps/vault-file-tree-init-delays '(30))
          (ps/vault--file-tree-timers nil))
      (ps/vault-file-tree-init-later one)
      (let ((first ps/vault--file-tree-timers))
        (should (= (length first) 1))
        (ps/vault-file-tree-init-later two)
        (should-not (memq (car first) timer-list))
        (should (= (length ps/vault--file-tree-timers) 1))
        (dolist (timer ps/vault--file-tree-timers) (cancel-timer timer))))))

(ert-deftest ps/vault-switch-test-file-tree-ladder-skips-without-a-vault ()
  "With no vault open there is nothing to re-root, and no error either."
  (let ((my-org-base-directory nil)
        (ps/vault--file-tree-timers nil))
    (ps/vault-file-tree-init-later)
    (should-not ps/vault--file-tree-timers)))

(provide 'test-ps-vault-switch)
;;; test-ps-vault-switch.el ends here
