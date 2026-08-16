;;; test-ps-dired.el --- ERT tests for ps-dired -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'dired)
(add-to-list 'load-path "lisp")
(require 'ps-dired)

;;; -------------------------------------------------------
;;; ps/dired--kind -- what a line is
;;; -------------------------------------------------------

(ert-deftest ps/dired--kind-names-the-parent-before-anything-else ()
  "`..' is a directory as far as the file system is concerned, but it is the
way *out* and gets its own glyph, so the name is checked first."
  (should (eq (ps/dired--kind "/a/b/.." '(t)) 'parent))
  (should (eq (ps/dired--kind "/a/b/." '(t)) 'parent)))

(ert-deftest ps/dired--kind-tells-a-directory-from-a-file ()
  (should (eq (ps/dired--kind "/a/b/notes" '(t)) 'directory))
  (should (eq (ps/dired--kind "/a/b/notes.md" '(nil)) 'file)))

(ert-deftest ps/dired--kind-treats-an-unreadable-entry-as-a-file ()
  "A broken symlink has no attributes to read.  Calling it a file shows it
with the generic glyph, which is better than the line having no marker at all."
  (should (eq (ps/dired--kind "/a/b/gone.md" nil) 'file)))

;;; -------------------------------------------------------
;;; ps/dired--size-label -- sizes, for files only
;;; -------------------------------------------------------

(ert-deftest ps/dired--size-label-is-human-readable ()
  (should (equal (ps/dired--size-label 'file (list nil 1 0 0 nil nil nil 1200))
                 "1.2k")))

(ert-deftest ps/dired--size-label-is-empty-for-a-directory ()
  "A directory's own size is the size of its entry table -- it says nothing
about what is inside, so showing it invites exactly the wrong conclusion."
  (should (equal (ps/dired--size-label 'directory (list t 5 0 0 nil nil nil 192)) ""))
  (should (equal (ps/dired--size-label 'parent (list t 5 0 0 nil nil nil 192)) "")))

(ert-deftest ps/dired--size-label-survives-an-unreadable-entry ()
  (should (equal (ps/dired--size-label 'file nil) "")))

;;; -------------------------------------------------------
;;; ps/dired--annotation -- the two columns
;;; -------------------------------------------------------

(ert-deftest ps/dired--annotation-right-aligns-the-size ()
  "Right-aligned so the digits line up down the column; a left-aligned size
column is unreadable the moment two entries differ by a factor of ten."
  (let ((ps/dired-size-width 7))
    (should (equal (ps/dired--annotation "d" "1.2k") "d    1.2k  "))
    (should (equal (ps/dired--annotation "d" "") "d          "))))

;;; -------------------------------------------------------
;;; ps/dired--glyph -- one icon per kind, the file tree's own
;;; -------------------------------------------------------

(ert-deftest ps/dired--glyph-falls-back-to-text-without-the-icon-font ()
  "The type marker is the job here.  Where Material Symbols is not installed
the listing still has to say which entries are folders, or the distinction
falls back to colour alone -- which is where this started."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda () nil)))
    (should (equal (ps/dired--glyph 'parent) "↑"))
    (should (equal (ps/dired--glyph 'directory) "▸"))
    (should (equal (ps/dired--glyph 'file) "·"))))

;;; -------------------------------------------------------
;;; The listing
;;; -------------------------------------------------------

(ert-deftest ps/dired-decorate-annotates-every-entry-once ()
  "Including on a re-read: Dired regenerates the buffer wholesale on a revert,
and a second pass that kept the first one's overlays would draw two icons."
  (let ((directory (make-temp-file "ps-dired-test-" :directory)))
    (unwind-protect
        (progn
          (write-region "hello\n" nil (expand-file-name "one.md" directory) nil 'silent)
          (make-directory (expand-file-name "inside" directory))
          (let ((buffer (dired-noselect directory)))
            (unwind-protect
                (with-current-buffer buffer
                  (ps/dired-decorate)
                  (ps/dired-decorate)
                  (goto-char (point-min))
                  (let ((annotated 0))
                    (while (not (eobp))
                      (when-let* ((start (dired-move-to-filename)))
                        (let ((overlays (seq-filter (lambda (o) (overlay-get o 'ps/dired))
                                                    (overlays-in start (1+ start)))))
                          (should (= (length overlays) 1))
                          (setq annotated (1+ annotated))))
                      (forward-line 1))
                    ;; `one.md', `inside', and `..' -- `.' is omitted by the
                    ;; listing settings, but this buffer is raw Dired, so both
                    ;; dot entries are here.
                    (should (>= annotated 3))))
              (kill-buffer buffer))))
      (delete-directory directory :recursive))))

(ert-deftest ps/dired-decorate-does-nothing-when-turned-off ()
  (let ((directory (make-temp-file "ps-dired-test-" :directory))
        (ps/dired-decorate-listing nil))
    (unwind-protect
        (let ((buffer (dired-noselect directory)))
          (unwind-protect
              (with-current-buffer buffer
                (ps/dired-decorate)
                (should-not (seq-filter (lambda (o) (overlay-get o 'ps/dired))
                                        (overlays-in (point-min) (point-max)))))
            (kill-buffer buffer)))
      (delete-directory directory :recursive))))

;;; -------------------------------------------------------
;;; The omit list, which is what puts `..' back
;;; -------------------------------------------------------

(ert-deftest ps/dired-omit-regexp-keeps-the-parent ()
  "The listing's own setting, asserted here because `..' disappearing is a
silent loss: nothing errors, there is simply no way back up with the mouse."
  (let ((omit ps/dired-omit-files))
    (should-not (string-match-p omit ".."))
    (should (string-match-p omit "."))
    (should (string-match-p omit ".DS_Store"))
    (should (string-match-p omit ".#lock"))
    (should-not (string-match-p omit "index.md"))
    (should-not (string-match-p omit ".claude"))))

(provide 'test-ps-dired)
;;; test-ps-dired.el ends here
