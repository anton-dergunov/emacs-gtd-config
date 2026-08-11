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
        (ps/font-prose '("AlsoNotInstalledXYZ")))
    (should (equal (ps/fonts-apply) '(nil . nil)))))

;;; ps-fonts tests end here
