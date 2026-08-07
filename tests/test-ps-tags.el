;;; test-ps-tags.el --- ERT tests for ps-tags -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path "lisp")
(require 'ps-tags)

(ert-deftest ps/tags--nobreak-string-replaces-padding-spaces ()
  "The pill padding org-modern puts in its display strings loses its spaces.
A plain space there is a `word-wrap' break opportunity, which is what
strands an empty coloured box at the right edge when a headline wraps."
  (should (equal (ps/tags--nobreak-string " m")
                 (string ps/tags--nobreak-space ?m)))
  (should (equal (ps/tags--nobreak-string "l ")
                 (string ?l ps/tags--nobreak-space))))

(ert-deftest ps/tags--nobreak-string-keeps-text-properties ()
  "org-modern stores a `cursor' property inside those display strings."
  (let* ((original (propertize " m" 'cursor t))
         (fixed (ps/tags--nobreak-string original)))
    (should (eq (get-text-property 0 'cursor fixed) t))
    (should (eq (get-text-property 1 'cursor fixed) t))))

(ert-deftest ps/tags--nobreak-string-leaves-space-free-strings-alone ()
  "Nothing to fix means the very same object comes back, so refontifying
an already-fixed pill does no work and allocates nothing."
  (let ((original "ml"))
    (should (eq (ps/tags--nobreak-string original) original)))
  (let ((already (string ps/tags--nobreak-space ?m)))
    (should (eq (ps/tags--nobreak-string already) already))))

(ert-deftest ps/tags--nobreak-string-does-not-mutate-its-argument ()
  "The display strings belong to org-modern; rewrite a copy."
  (let ((original " m"))
    (ps/tags--nobreak-string original)
    (should (equal original " m"))))

(ert-deftest ps/tags--unbreak-region-rewrites-only-display-strings ()
  "Only string-valued `display' properties are touched; other values and
plain text are left as they are."
  (with-temp-buffer
    (insert ":ml:")
    (put-text-property 2 3 'display " m")
    (put-text-property 3 4 'display '(space :width 2))
    (ps/tags--unbreak-region (point-min) (point-max))
    (should (equal (get-text-property 2 'display)
                   (string ps/tags--nobreak-space ?m)))
    (should (equal (get-text-property 3 'display) '(space :width 2)))
    (should-not (get-text-property 1 'display))))

(ert-deftest ps/tags--pill-p-recognises-both-face-shapes ()
  "org-modern sets the face as a bare symbol, or as a list when
`org-modern-tag-faces' applies."
  (with-temp-buffer
    (insert "abc")
    (put-text-property 1 2 'face 'org-modern-tag)
    (put-text-property 2 3 'face '(bold org-modern-tag))
    (put-text-property 3 4 'face 'org-level-1)
    (should (ps/tags--pill-p 1))
    (should (ps/tags--pill-p 2))
    (should-not (ps/tags--pill-p 3))))

;;; test-ps-tags.el ends here
