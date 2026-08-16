;;; test-ps-open.el --- ERT tests for ps-open -*- lexical-binding: t; -*-

(require 'ert)
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

;;; test-ps-open.el ends here
