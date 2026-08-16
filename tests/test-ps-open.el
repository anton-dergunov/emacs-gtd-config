;;; test-ps-open.el --- ERT tests for ps-open -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path "lisp")
(require 'ps-open)

;;; -------------------------------------------------------
;;; ps/open-handler -- the policy, decided by name
;;; -------------------------------------------------------

(ert-deftest ps/open-handler-covers-every-kind-the-inbox-holds ()
  "A captured item's directory is a mixed-media tree, and each kind of file in
it has a different right answer.  These are the extensions a real inbox
actually contains."
  (should (eq (ps/open-handler "/i/2026-08-16_165/index.md") 'emacs))
  (should (eq (ps/open-handler "/i/links.json") 'emacs))
  (should (eq (ps/open-handler "/i/raw/transcript.txt") 'emacs))
  (should (eq (ps/open-handler "/i/raw/source.html") 'browser))
  (should (eq (ps/open-handler "/i/raw/media/01_image.jpg") 'emacs))
  (should (eq (ps/open-handler "/i/raw/paper.pdf") 'external))
  (should (eq (ps/open-handler "/i/raw/media/01_video.mp4") 'external))
  (should (eq (ps/open-handler "/i/raw/audio.ogg") 'refuse)))

(ert-deftest ps/open-handler-takes-the-first-match ()
  "The list is ordered policy, not a set: an earlier entry must win, so a
narrow rule can be put in front of a broad one without rewriting the broad one."
  (let ((ps/open-handlers '(("/raw/.*\\.md\\'" . refuse)
                            ("\\.md\\'" . emacs))))
    (should (eq (ps/open-handler "/i/raw/notes.md") 'refuse))
    (should (eq (ps/open-handler "/i/index.md") 'emacs))))

(ert-deftest ps/open-handler-is-nil-for-an-extension-nobody-listed ()
  "Which is what sends it to the content sniff rather than to a guess."
  (should-not (ps/open-handler "/i/capture/attachment.heic")))

;;; -------------------------------------------------------
;;; ps/open--binary-string-p -- the fallback for the unlisted
;;; -------------------------------------------------------

(ert-deftest ps/open--binary-string-p-settles-on-a-nul-byte ()
  "No text encoding this config meets puts a NUL in a file."
  (should (ps/open--binary-string-p "PK\3\4\0\0plausible text"))
  (should-not (ps/open--binary-string-p "# A heading\n\nSome prose.\n")))

(ert-deftest ps/open--binary-string-p-tolerates-utf-8-prose ()
  "Captions arrive full of emoji and CJK, and none of that is a control byte.
Treating them as binary would refuse to open the very files worth reading."
  (should-not (ps/open--binary-string-p "📍 Gexian Mountain (葛仙山), Jiangxi\n"))
  (should-not (ps/open--binary-string-p "line\r\n\tindented\r\n")))

(ert-deftest ps/open--binary-string-p-catches-control-heavy-headers ()
  "A container format with no NUL in its first bytes is still not text."
  (should (ps/open--binary-string-p (concat "\1\2\3\4\5\6\a\b\v\16" "ftyp"))))

(ert-deftest ps/open--binary-string-p-says-nothing-about-an-empty-file ()
  "An empty file is not binary, and dividing by its length would fail."
  (should-not (ps/open--binary-string-p "")))

;;; -------------------------------------------------------
;;; ps/open-resolve -- name first, content only as a fallback
;;; -------------------------------------------------------

(ert-deftest ps/open-resolve-never-reads-a-file-it-can-decide-by-name ()
  "The sniff opens the file; the policy must not pay for that on the common
path.  A named handler answers for a path that does not even exist."
  (should (eq (ps/open-resolve "/nowhere/at/all/index.md") 'emacs))
  (should (eq (ps/open-resolve "/nowhere/at/all/clip.mp4") 'external)))

(ert-deftest ps/open-resolve-asks-before-opening-an-unlisted-binary ()
  (let ((file (make-temp-file "ps-open-test-")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'binary))
            (write-region "\0\0\0\30ftypmp42" nil file nil 'silent))
          (should (eq (ps/open-resolve file) 'ask)))
      (delete-file file))))

(ert-deftest ps/open-resolve-opens-an-unlisted-text-file ()
  "A capture format nobody listed yet, holding prose, is still readable."
  (let ((file (make-temp-file "ps-open-test-" nil ".unheard-of")))
    (unwind-protect
        (progn
          (write-region "just some notes\n" nil file nil 'silent)
          (should (eq (ps/open-resolve file) 'emacs)))
      (delete-file file))))

;;; -------------------------------------------------------
;;; ps/open-file -- refusal
;;; -------------------------------------------------------

(ert-deftest ps/open-file-refuses-audio-and-says-why ()
  "A refusal that does not say why reads as a bug in the config."
  (let ((file (make-temp-file "ps-open-test-" nil ".ogg")))
    (unwind-protect
        (progn
          (write-region "x" nil file nil 'silent)
          (let ((error-message
                 (cadr (should-error (ps/open-file file) :type 'user-error))))
            (should (string-match-p "transcript" error-message))))
      (delete-file file))))

(ert-deftest ps/open-file-refuses-a-file-that-is-not-there ()
  (should-error (ps/open-file "/nowhere/at/all/index.md") :type 'user-error))

;;; -------------------------------------------------------
;;; ps/open-markdown-thing -- RET off a link
;;; -------------------------------------------------------

(ert-deftest ps/open-markdown-thing-is-quiet-off-a-link-in-a-read-only-buffer ()
  "It is bound to RET, and every captured file is read-only -- so falling
through to `markdown-enter-key' there greeted anyone who missed a link with
\"Buffer is read-only\"."
  (with-temp-buffer
    (insert "just prose, no link here\n")
    (goto-char (point-min))
    (setq buffer-read-only t)
    (should-not (ps/open-markdown-thing))))

;;; -------------------------------------------------------
;;; ps/open-file -- a directory is not sniffed
;;; -------------------------------------------------------

(ert-deftest ps/open-file-opens-a-directory-without-reading-it ()
  "Resolving the handler first sent a directory to `ps/open--binary-file-p',
which reads a file's opening bytes -- and reading a directory signals \"Read
error: Is a directory\".  That is what clicking an item's `directory' link
did."
  (let ((directory (make-temp-file "ps-open-test-dir-" :directory))
        (visited nil)
        (sniffed nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ps/window-visit-only-here)
                   (lambda (file) (setq visited file)))
                  ((symbol-function 'ps/open--binary-file-p)
                   (lambda (_file) (setq sniffed t) nil)))
          (ps/open-file directory)
          (should (equal visited (expand-file-name directory)))
          (should-not sniffed))
      (delete-directory directory))))

;;; -------------------------------------------------------
;;; Following on a plain click
;;; -------------------------------------------------------

(defun test-ps-open--posn (position)
  "Return a position list naming POSITION in the current buffer's window."
  (list (selected-window) position '(0 . 0) 0))

(defun test-ps-open--click (position)
  "Return a `mouse-1' event at POSITION."
  (list 'mouse-1 (test-ps-open--posn position)))

(defun test-ps-open--drag (from to)
  "Return a `drag-mouse-1' event from FROM to TO."
  (list 'drag-mouse-1 (test-ps-open--posn from) (test-ps-open--posn to)))

(ert-deftest ps/open--clickable-at-p-answers-for-a-mouse-face ()
  "The one marker Org, Markdown and Dired all put on the text a click is aimed
at -- and only on that text, so the padding around it stays inert."
  (with-temp-buffer
    (insert "before ")
    (let ((start (point)))
      (insert (propertize "link" 'mouse-face 'highlight))
      (insert " after")
      (should (ps/open--clickable-at-p start))
      (should-not (ps/open--clickable-at-p (point-min)))
      (should-not (ps/open--clickable-at-p (1- (point-max)))))))

(ert-deftest ps/open--event-stationary-p-tells-a-wobble-from-a-selection ()
  "A drag that never left the position it started at is a click the hand moved
during, and Emacs reports one of those whenever the pointer drifts more than
`double-click-fuzz' pixels."
  (should (ps/open--event-stationary-p (test-ps-open--drag 12 12)))
  (should-not (ps/open--event-stationary-p (test-ps-open--drag 12 40))))

(ert-deftest ps/open-click-follows-only-what-it-landed-on ()
  "Clicking a link follows it; clicking beside one moves point and stops
there, which is what keeps a click on a heading or a size column harmless."
  (with-temp-buffer
    (insert "plain ")
    (let ((link (point))
          (followed 0))
      (insert (propertize "link" 'mouse-face 'highlight))
      (setq-local ps/open-follow-function
                  (lambda () (interactive) (setq followed (1+ followed))))
      (cl-letf (((symbol-function 'mouse-set-point)
                 (lambda (event) (goto-char (posn-point (event-end event))))))
        (ps/open-click (test-ps-open--click (point-min)))
        (should (= followed 0))
        (should (= (point) (point-min)))
        (ps/open-click (test-ps-open--click link))
        (should (= followed 1))))))

(ert-deftest ps/open-drag-click-follows-a-wobble-and-selects-a-real-drag ()
  (with-temp-buffer
    (insert (propertize "link" 'mouse-face 'highlight))
    (insert " and more text")
    (let ((followed 0)
          (selected 0))
      (setq-local ps/open-follow-function
                  (lambda () (interactive) (setq followed (1+ followed))))
      (cl-letf (((symbol-function 'mouse-set-point)
                 (lambda (event) (goto-char (posn-point (event-end event)))))
                ((symbol-function 'mouse-set-region)
                 (lambda (_event) (setq selected (1+ selected)))))
        (ps/open-drag-click (test-ps-open--drag 2 2))
        (should (= followed 1))
        (should (= selected 0))
        (ps/open-drag-click (test-ps-open--drag 2 10))
        (should (= followed 1))
        (should (= selected 1))))))

(ert-deftest ps/open-bind-click-binds-both-halves-of-a-click ()
  "A drag binding without a click binding, or the other way round, is the bug
this whole pair exists to fix."
  (let ((map (make-sparse-keymap)))
    (ps/open-bind-click map)
    (should (eq (lookup-key map [mouse-1]) #'ps/open-click))
    (should (eq (lookup-key map [drag-mouse-1]) #'ps/open-drag-click))))

;;; test-ps-open.el ends here
