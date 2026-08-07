;;; test-ps-situations.el --- ERT tests for ps-situations -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path "lisp")
(require 'ps-situations)

;;; -------------------------------------------------------
;;; context tags — normalization
;;; -------------------------------------------------------

(ert-deftest ps/context-tags--normalize-keeps-plist ()
  (let ((entry '(:name "audio" :key ?a :kind "affordance" :means "Ears only.")))
    (should (equal (ps/context-tags--normalize entry) entry))))

(ert-deftest ps/context-tags--normalize-accepts-cons-shorthand ()
  (let ((got (ps/context-tags--normalize '("audio" . ?a))))
    (should (equal (plist-get got :name) "audio"))
    (should (equal (plist-get got :key) ?a))))

(ert-deftest ps/context-tags--normalize-rejects-junk ()
  (should (null (ps/context-tags--normalize nil)))
  (should (null (ps/context-tags--normalize "audio")))
  (should (null (ps/context-tags--normalize '(:key ?a))))       ; no :name
  (should (null (ps/context-tags--normalize '(:name "" :key ?a)))))

(ert-deftest ps/context-tags-all-drops-unusable-entries ()
  (let ((ps/context-tags '((:name "audio" :key ?a) nil (:key ?x) ("think" . ?t))))
    (should (equal (mapcar (lambda (tg) (plist-get tg :name)) (ps/context-tags-all))
                   '("audio" "think")))))

;;; -------------------------------------------------------
;;; context tags — derived org-tag-alist
;;; -------------------------------------------------------

(ert-deftest ps/context-tags-org-tag-alist-pairs-name-and-key ()
  (should (equal (ps/context-tags-org-tag-alist
                  '((:name "audio" :key ?a :kind "affordance")
                    (:name "think" :key ?t :kind "affordance")))
                 '(("audio" . ?a) ("think" . ?t)))))

(ert-deftest ps/context-tags-org-tag-alist-breaks-lines-between-kinds ()
  "A `(:newline)' separates kinds -- and never leads the list."
  (should (equal (ps/context-tags-org-tag-alist
                  '((:name "audio" :key ?a :kind "affordance")
                    (:name "think" :key ?t :kind "affordance")
                    (:name "online" :key ?o :kind "restriction")
                    (:name "download" :key ?d :kind "lifecycle")))
                 '(("audio" . ?a) ("think" . ?t)
                   (:newline)
                   ("online" . ?o)
                   (:newline)
                   ("download" . ?d)))))

(ert-deftest ps/context-tags-org-tag-alist-omits-key-when-absent ()
  (should (equal (ps/context-tags-org-tag-alist '((:name "audio")))
                 '("audio"))))

(ert-deftest ps/context-tags-org-tag-alist-no-kinds-no-newlines ()
  (should (equal (ps/context-tags-org-tag-alist
                  '((:name "audio" :key ?a) (:name "think" :key ?t)))
                 '(("audio" . ?a) ("think" . ?t)))))

;;; -------------------------------------------------------
;;; context tags — substring lint (the Orgzly constraint)
;;; -------------------------------------------------------

(ert-deftest ps/context-tags--substring-collisions-finds-nesting ()
  (should (equal (ps/context-tags--substring-collisions '("read" "reading" "walk"))
                 '(("read" . "reading")))))

(ert-deftest ps/context-tags--substring-collisions-ignores-self ()
  (should (null (ps/context-tags--substring-collisions '("audio" "think" "phone")))))

;;; -------------------------------------------------------
;;; situations — normalization
;;; -------------------------------------------------------

(ert-deftest ps/situations--normalize-keeps-plist ()
  (let ((entry '(:key "m" :name "A spare minute" :query "think|micro")))
    (should (equal (ps/situations--normalize entry) entry))))

(ert-deftest ps/situations--normalize-accepts-list-shorthand ()
  (let ((got (ps/situations--normalize '("m" "A spare minute" "think|micro"))))
    (should (equal (plist-get got :key) "m"))
    (should (equal (plist-get got :name) "A spare minute"))
    (should (equal (plist-get got :query) "think|micro"))))

(ert-deftest ps/situations--normalize-requires-key-and-query ()
  (should (null (ps/situations--normalize '(:key "m"))))
  (should (null (ps/situations--normalize '(:query "think"))))
  (should (null (ps/situations--normalize '(:key "" :query "think"))))
  (should (null (ps/situations--normalize 42))))

(ert-deftest ps/situations--description-appends-hint ()
  (should (equal (ps/situations--description
                  '(:key "m" :name "A spare minute" :hint "a queue" :query "micro"))
                 "A spare minute (a queue)"))
  (should (equal (ps/situations--description
                  '(:key "m" :name "A spare minute" :query "micro"))
                 "A spare minute")))

(ert-deftest ps/situations-find-matches-by-key ()
  (let ((ps/situations '((:key "m" :name "Minute" :query "micro")
                         (:key "f" :name "Foot" :query "audio"))))
    (should (equal (plist-get (ps/situations-find "f") :name) "Foot"))
    (should (null (ps/situations-find "z")))))

;;; -------------------------------------------------------
;;; situations — generated agenda commands
;;; -------------------------------------------------------

(defconst ps/situations-test--two
  '((:key "m" :name "A spare minute" :hint "a queue" :query "think|micro")
    (:key "f" :name "On foot" :query "audio|think")))

(ert-deftest ps/situations--custom-commands-uses-the-prefix ()
  (let ((ps/situations-key-prefix "s"))
    (should (equal (mapcar #'car (ps/situations--custom-commands ps/situations-test--two))
                   '("sm" "sf")))))

(ert-deftest ps/situations--custom-commands-are-block-form ()
  "A one-block series, not a bare `tags-todo' -- only a series survives redo
with its settings intact (see the module Commentary)."
  (let* ((cmd (car (ps/situations--custom-commands ps/situations-test--two)))
         (blocks (nth 2 cmd)))
    (should (listp blocks))
    (should (= (length blocks) 1))
    (should (eq (car (car blocks)) 'tags-todo))
    (should (equal (nth 1 (car blocks)) "think|micro"))))

(ert-deftest ps/situations--custom-commands-tag-the-view-and-the-key ()
  (let* ((cmd (car (ps/situations--custom-commands ps/situations-test--two)))
         (gprops (nth 3 cmd)))
    (should (equal (assq 'ps/agenda-layout-view-kind gprops)
                   '(ps/agenda-layout-view-kind 'situation)))
    (should (equal (assq 'ps/situations-current gprops)
                   '(ps/situations-current "m")))))

(ert-deftest ps/situations--custom-commands-omit-overriding-header ()
  "The header line must be emitted by Org: the layout pass rewrites it into
the centred plate."
  (let* ((cmd (car (ps/situations--custom-commands ps/situations-test--two)))
         (lprops (nth 2 (car (nth 2 cmd)))))
    (should (null (assq 'org-agenda-overriding-header lprops)))))

(ert-deftest ps/situations--custom-commands-describe-with-the-hint ()
  (let ((cmd (car (ps/situations--custom-commands ps/situations-test--two))))
    (should (equal (nth 1 cmd) "A spare minute (a queue)"))))

;;; -------------------------------------------------------
;;; situations — registration into org-agenda-custom-commands
;;; -------------------------------------------------------

(ert-deftest ps/situations--register-appends-after-existing ()
  "Agenda and Calendar keep the top of the dispatcher."
  (let* ((ps/situations-key-prefix "s")
         (existing '(("c" "Agenda") ("g" "Calendar")))
         (got (ps/situations--register existing ps/situations-test--two)))
    (should (equal (mapcar #'car got) '("c" "g" "sm" "sf")))))

(ert-deftest ps/situations--register-is-idempotent ()
  "Re-registering can neither duplicate entries nor reverse their order --
this is what a plain `push' per entry got wrong."
  (let* ((ps/situations-key-prefix "s")
         (once (ps/situations--register '(("c" "Agenda")) ps/situations-test--two))
         (twice (ps/situations--register once ps/situations-test--two))
         (thrice (ps/situations--register twice ps/situations-test--two)))
    (should (equal (mapcar #'car twice) '("c" "sm" "sf")))
    (should (equal twice thrice))))

(ert-deftest ps/situations--register-drops-stale-situations ()
  "A situation removed from the declaration disappears from the dispatcher."
  (let* ((ps/situations-key-prefix "s")
         (before (ps/situations--register nil ps/situations-test--two))
         (after (ps/situations--register before '((:key "m" :name "Minute" :query "micro")))))
    (should (equal (mapcar #'car after) '("sm")))))

(ert-deftest ps/situations--register-leaves-other-prefixes-alone ()
  (let* ((ps/situations-key-prefix "s")
         (got (ps/situations--register '(("c" "Agenda") ("gg" "Custom"))
                                       ps/situations-test--two)))
    (should (member "gg" (mapcar #'car got)))))

;;; -------------------------------------------------------
;;; situations — menus
;;; -------------------------------------------------------

(ert-deftest ps/situations--menu-items-carry-the-key ()
  (should (equal (ps/situations--menu-items ps/situations-test--two)
                 '(("A spare minute (a queue)" situation . "m")
                   ("On foot" situation . "f")))))

(ert-deftest ps/situations--menu-vectors-bind-show-situation ()
  (let ((v (car (ps/situations--menu-vectors ps/situations-test--two))))
    (should (equal (aref v 0) "A spare minute (a queue)"))
    (should (equal (aref v 1) '(ps/show-situation "m")))))

(ert-deftest ps/situations--menu-filter-placeholder-when-empty ()
  (let ((ps/situations nil))
    (let ((got (ps/situations--menu-filter)))
      (should (= (length got) 1))
      (should (equal (aref (car got) 0) "No situations defined")))))

;;; -------------------------------------------------------
;;; plate label
;;; -------------------------------------------------------

(ert-deftest ps/situations-plate-label-uses-the-name ()
  (let ((ps/situations ps/situations-test--two))
    (should (equal (ps/situations-plate-label "f") "On foot"))))

(ert-deftest ps/situations-plate-label-falls-back-for-unknown-key ()
  (let ((ps/situations ps/situations-test--two))
    (should (equal (ps/situations-plate-label "zz") "Situation"))))

;;; -------------------------------------------------------
;;; apply
;;; -------------------------------------------------------

(ert-deftest ps/situations-apply-derives-org-tag-alist ()
  (let ((ps/context-tags '((:name "audio" :key ?a :kind "affordance")
                           (:name "online" :key ?o :kind "restriction")))
        (ps/situations nil)
        (org-tag-alist 'untouched))
    (ps/situations-apply)
    (should (equal org-tag-alist '(("audio" . ?a) (:newline) ("online" . ?o))))))

(ert-deftest ps/situations-apply-leaves-org-tag-alist-alone-when-undeclared ()
  "With no context tags the user may still be setting `org-tag-alist' by hand."
  (let ((ps/context-tags nil)
        (ps/situations nil)
        (org-tag-alist 'untouched))
    (ps/situations-apply)
    (should (eq org-tag-alist 'untouched))))

(ert-deftest ps/situations-apply-builds-the-keymap ()
  (let ((ps/situations ps/situations-test--two)
        (ps/context-tags nil))
    (ps/situations-apply)
    (should (keymapp ps/situations-keymap))
    (should (commandp (lookup-key ps/situations-keymap (kbd "m"))))
    (should (commandp (lookup-key ps/situations-keymap (kbd "f"))))
    (should (null (lookup-key ps/situations-keymap (kbd "z"))))))

(ert-deftest ps/situations-apply-keymap-drops-stale-keys ()
  (let ((ps/context-tags nil))
    (let ((ps/situations ps/situations-test--two))
      (ps/situations-apply))
    (let ((ps/situations '((:key "m" :name "Minute" :query "micro"))))
      (ps/situations-apply))
    (should (commandp (lookup-key ps/situations-keymap (kbd "m"))))
    (should (null (lookup-key ps/situations-keymap (kbd "f"))))))

(provide 'test-ps-situations)
;;; test-ps-situations.el ends here
