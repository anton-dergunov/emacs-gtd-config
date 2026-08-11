;;; test-ps-fonts.el --- ERT tests for ps-fonts -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path "lisp")
(require 'ps-fonts)

;;; Candidate lists

(ert-deftest ps/fonts-test-candidates-accepts-a-bare-string ()
  "A single family may be written without wrapping it in a list."
  (should (equal (ps/fonts--candidates "Monaco") '("Monaco"))))

(ert-deftest ps/fonts-test-candidates-keeps-order ()
  "Order is the preference order, so it must survive filtering."
  (should (equal (ps/fonts--candidates '("A" "B" "C")) '("A" "B" "C"))))

(ert-deftest ps/fonts-test-candidates-drops-junk ()
  "A half-edited setting degrades to \"try the rest\" rather than signalling."
  (should (equal (ps/fonts--candidates '("A" "" nil 42 "B")) '("A" "B")))
  (should (equal (ps/fonts--candidates nil) nil)))

;;; Fallback

(ert-deftest ps/fonts-test-first-available-picks-the-first-installed ()
  "The earliest candidate that exists wins; earlier missing ones are skipped."
  (should (equal (ps/fonts--first-available
                  '("Missing" "Present" "AlsoPresent")
                  (lambda (f) (member f '("Present" "AlsoPresent"))))
                 "Present")))

(ert-deftest ps/fonts-test-first-available-returns-nil-when-none-exist ()
  "No candidate installed means \"leave the face alone\", not an error."
  (should (null (ps/fonts--first-available '("Missing" "AlsoMissing")
                                           #'ignore))))

;;; Face specs

(ert-deftest ps/fonts-test-face-spec-family-and-absolute-height ()
  "An integer height is an absolute size in 1/10 pt."
  (should (equal (ps/fonts--face-spec "Monaco" 140)
                 '(:family "Monaco" :height 140))))

(ert-deftest ps/fonts-test-face-spec-family-and-relative-height ()
  "A float height is a multiplier of the inherited size."
  (should (equal (ps/fonts--face-spec "Charter" 0.95)
                 '(:family "Charter" :height 0.95))))

(ert-deftest ps/fonts-test-face-spec-omits-height ()
  "nil height means \"track the inherited size\" -- this is how `fixed-pitch'
keeps following `default' when `ps/font-size' changes."
  (should (equal (ps/fonts--face-spec "Monaco" nil) '(:family "Monaco"))))

(ert-deftest ps/fonts-test-face-spec-omits-neutral-scale ()
  "A scale of exactly 1 is dropped rather than applied, so a later relative
remapping of the same face is not stacked onto a redundant multiplier."
  (should (equal (ps/fonts--face-spec "Charter" 1.0) '(:family "Charter"))))

(ert-deftest ps/fonts-test-face-spec-without-family-is-nil ()
  "No resolved family and no height means there is nothing to set."
  (should (null (ps/fonts--face-spec nil nil)))
  (should (null (ps/fonts--face-spec "" nil))))

(ert-deftest ps/fonts-test-face-spec-height-without-family ()
  "A missing family must not swallow the size: the frame is still resized."
  (should (equal (ps/fonts--face-spec nil 140) '(:height 140))))

(ert-deftest ps/fonts-test-face-spec-rejects-nonsense-height ()
  "A zero or negative size reads as \"leave the size alone\"."
  (should (equal (ps/fonts--face-spec "Monaco" 0) '(:family "Monaco")))
  (should (equal (ps/fonts--face-spec "Monaco" -3) '(:family "Monaco"))))

;;; Point conversion

(ert-deftest ps/fonts-test-points-to-height ()
  "Points become an integer in 1/10 pt, which is what `:height' expects."
  (should (equal (ps/fonts--points-to-height 14) 140))
  (should (equal (ps/fonts--points-to-height 13.5) 135)))

(ert-deftest ps/fonts-test-points-to-height-rejects-nonsense ()
  "nil / zero / negative all read as \"leave the size alone\"."
  (should (null (ps/fonts--points-to-height nil)))
  (should (null (ps/fonts--points-to-height 0)))
  (should (null (ps/fonts--points-to-height -14))))

;;; Application

(ert-deftest ps/fonts-test-apply-never-signals-without-fonts ()
  "In batch no font is available, so `ps/fonts-apply' must be a quiet no-op.
This is the property that keeps a font named in the settings but absent from
the machine from breaking startup."
  (let ((ps/font-mono '("DefinitelyNotInstalledXYZ"))
        (ps/font-prose '("AlsoNotInstalledXYZ"))
        (ps/font-ui '("NorThisOneXYZ")))
    (should (equal (ps/fonts-apply) '((mono . nil) (prose . nil) (ui . nil))))))

(ert-deftest ps/fonts-test-apply-covers-every-role ()
  "`ps/fonts-apply' is driven by `ps/fonts--roles', so adding a role there
cannot leave it unapplied."
  (should (equal (mapcar #'car (ps/fonts-apply))
                 (mapcar #'car ps/fonts--roles))))

;;; Line spacing

(ert-deftest ps/fonts-test-line-spacing-is-per-family ()
  "Leading is a property of the font, so it is looked up by family."
  (let ((alist '(("Menlo" . 0.2) ("Iosevka" . 3))))
    (should (equal (ps/fonts--line-spacing "Menlo" alist) 0.2))
    (should (equal (ps/fonts--line-spacing "Iosevka" alist) 3))))

(ert-deftest ps/fonts-test-line-spacing-defaults-to-none ()
  "A family with no entry gets no extra leading, not an error."
  (should (null (ps/fonts--line-spacing "Monaco" '(("Menlo" . 0.2)))))
  (should (null (ps/fonts--line-spacing nil '(("Menlo" . 0.2))))))

;;; Roles

(ert-deftest ps/fonts-test-role-variable ()
  "Every role resolves to the setting that holds its candidate list."
  (should (eq (ps/fonts--role-variable 'mono) 'ps/font-mono))
  (should (eq (ps/fonts--role-variable 'prose) 'ps/font-prose))
  (should (null (ps/fonts--role-variable 'nonexistent))))

;;; Promotion (the settings line an audition echoes)

(ert-deftest ps/fonts-test-promote-puts-the-choice-first ()
  "The audition choice becomes the preference; the fallbacks stay behind it."
  (should (equal (ps/fonts--promote "Iosevka" '("Monaco" "Menlo"))
                 '("Iosevka" "Monaco" "Menlo"))))

(ert-deftest ps/fonts-test-promote-does-not-duplicate ()
  "Choosing a font already in the list moves it up rather than repeating it."
  (should (equal (ps/fonts--promote "Menlo" '("Monaco" "Menlo" "Consolas"))
                 '("Menlo" "Monaco" "Consolas"))))

(ert-deftest ps/fonts-test-promote-keeps-the-rest-in-order ()
  "Fallback order is a preference, so promotion must not reshuffle it."
  (should (equal (ps/fonts--promote "D" '("A" "B" "C"))
                 '("D" "A" "B" "C"))))

(ert-deftest ps/fonts-test-setting-line-is-pasteable ()
  "The echoed line is valid elisp that reproduces the list."
  (let* ((line (ps/fonts--setting-line 'ps/font-mono '("JetBrains Mono" "Monaco")))
         (form (car (read-from-string line))))
    (should (equal line "(setq ps/font-mono '(\"JetBrains Mono\" \"Monaco\"))"))
    (should (eq (nth 0 form) 'setq))
    (should (eq (nth 1 form) 'ps/font-mono))
    (should (equal (eval (nth 2 form) t) '("JetBrains Mono" "Monaco")))))

;;; Preview candidate lists

(ert-deftest ps/fonts-test-preview-families-splits-installed-and-missing ()
  "Installed families are drawn, missing ones are only listed."
  (let ((ps/font-mono '("Monaco"))
        (ps/font-prose '("Charter"))
        (ps/font-preview-candidates '("Iosevka" "Literata")))
    (should (equal (ps/fonts--preview-families
                    (lambda (f) (member f '("Monaco" "Literata"))))
                   '(("Monaco" "Literata") . ("Charter" "Iosevka"))))))

(ert-deftest ps/fonts-test-preview-families-lists-current-settings-first ()
  "What you are already looking at heads the list, to compare against."
  (let ((ps/font-mono '("Menlo"))
        (ps/font-prose '("Georgia"))
        (ps/font-preview-candidates '("Menlo" "Iosevka")))
    (should (equal (car (ps/fonts--preview-families (lambda (_) t)))
                   '("Menlo" "Georgia" "Iosevka")))))

(ert-deftest ps/fonts-test-preview-families-deduplicates ()
  "A family named by both a setting and the candidate list appears once."
  (let ((ps/font-mono '("Monaco" "Monaco"))
        (ps/font-prose '("Monaco"))
        (ps/font-preview-candidates '("Monaco")))
    (should (equal (ps/fonts--preview-families (lambda (_) t))
                   '(("Monaco"))))))

;;; Cycling favourites

(ert-deftest ps/fonts-test-next-steps-forward ()
  "Cycling moves one along the shortlist."
  (should (equal (ps/fonts--next "Monaco" '("Monaco" "Menlo" "IBM Plex Mono"))
                 "Menlo")))

(ert-deftest ps/fonts-test-next-wraps ()
  "The last favourite cycles back to the first, so the list is a loop."
  (should (equal (ps/fonts--next "IBM Plex Mono" '("Monaco" "Menlo" "IBM Plex Mono"))
                 "Monaco")))

(ert-deftest ps/fonts-test-next-starts-at-the-beginning ()
  "A font that is not in the shortlist -- the usual case on the first cycle,
when what is applied came from the settings -- starts the list from the top."
  (should (equal (ps/fonts--next "Iosevka" '("Monaco" "Menlo")) "Monaco"))
  (should (equal (ps/fonts--next nil '("Monaco" "Menlo")) "Monaco")))

(ert-deftest ps/fonts-test-next-handles-an-empty-shortlist ()
  "No favourites means nothing to cycle to, not an error."
  (should (null (ps/fonts--next "Monaco" nil))))

(ert-deftest ps/fonts-test-favourites-cover-every-role ()
  "Every role can be cycled; a role without favourites would fail only when
someone pressed the key."
  (dolist (role (mapcar #'car ps/fonts--roles))
    (should (alist-get role ps/font-favourites))))

;;; Relative scales

(ert-deftest ps/fonts-test-scales-exclude-mono ()
  "`mono' has no relative size: it is the body size, set in points, and every
other size is a multiple of it."
  (should (null (alist-get 'mono ps/fonts--role-scales)))
  (should (eq (alist-get 'prose ps/fonts--role-scales) 'ps/font-prose-scale))
  (should (eq (alist-get 'ui ps/fonts--role-scales) 'ps/font-ui-scale)))

;;; Prose in Org buffers

(ert-deftest ps/fonts-test-prose-enable-is-off-by-default ()
  "With `ps/font-prose-in-org' nil nothing is remapped, whatever the buffer."
  (let ((ps/font-prose-in-org nil))
    (with-temp-buffer
      (ps/fonts-prose-enable)
      (should (null ps/fonts--prose-cookies)))))

(ert-deftest ps/fonts-test-prose-skips-out-of-scope-buffers ()
  "A buffer that is not one of the agenda's plan files is left alone even when
the feature is on -- that is what keeps config.org and the journal monospaced."
  (let ((ps/font-prose-in-org t))
    (with-temp-buffer
      ;; No visited file, so `ps/org-files-in-scope-p' is false.
      (ps/fonts-prose-enable)
      (should (null ps/fonts--prose-cookies)))))

(ert-deftest ps/fonts-test-prose-disable-is-idempotent ()
  "Disabling twice must not signal: the hook can run on a buffer that was
never enabled."
  (with-temp-buffer
    (ps/fonts-prose-disable)
    (ps/fonts-prose-disable)
    (should (null ps/fonts--prose-cookies))))

(ert-deftest ps/fonts-test-prose-pins-the-column-aligned-faces ()
  "The pin list has to cover the islands in a plan file that are still laid
out by column; losing one of these is how a proportional buffer breaks."
  (dolist (face '(org-table org-block org-code org-verbatim org-indent))
    (should (memq face ps/font-prose-fixed-pitch-faces))))

(ert-deftest ps/fonts-test-prose-does-not-pin-line-numbers ()
  "`ps-line-numbers.el' already remaps `line-number' with its own `:inherit';
a second relative remap would fight it."
  (should-not (memq 'line-number ps/font-prose-fixed-pitch-faces))
  (should-not (memq 'line-number-current-line ps/font-prose-fixed-pitch-faces)))

;;; Preview rendering

(ert-deftest ps/fonts-test-preview-block-renders ()
  "A preview block draws without error and names its family."
  (with-temp-buffer
    (ps/fonts--preview-insert-family "Monaco")
    (should (string-match-p "Monaco" (buffer-string)))
    (should (string-match-p "use as mono" (buffer-string)))
    (should (string-match-p "use as prose" (buffer-string)))))

(ert-deftest ps/fonts-test-preview-block-is-tagged-throughout ()
  "Every position in a block answers with its family, including the blank line
that ends it -- otherwise `m'/`p' pressed just below a block would act on the
next one, or on nothing."
  (with-temp-buffer
    (ps/fonts--preview-insert-family "Monaco")
    (goto-char (point-min))
    (should (equal (ps/fonts--preview-family-at-point) "Monaco"))
    (goto-char (1- (point-max)))
    (should (equal (ps/fonts--preview-family-at-point) "Monaco"))))

(ert-deftest ps/fonts-test-preview-sample-covers-the-fragile-glyphs ()
  "The sample has to include the characters the schedule ruler and agenda use,
since those are exactly the ones a text font lacks and silently falls back for."
  (let ((grid (cdr (assq 'grid ps/fonts--preview-sample))))
    (should (string-match-p "┆" grid))
    (should (string-match-p "┄" grid))))

(ert-deftest ps/fonts-test-preview-sample-has-a-monospace-check ()
  "Two equal-length runs of a narrow and a wide glyph: same width on screen
means the family is monospaced.  Their *character* counts must match for the
comparison to mean anything."
  (let ((widths (mapcar #'cdr (seq-filter (lambda (line) (eq (car line) 'width))
                                          ps/fonts--preview-sample))))
    (should (= (length widths) 2))
    (should (apply #'= (mapcar #'length widths)))))

;;; ps-fonts tests end here
