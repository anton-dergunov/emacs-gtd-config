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
  "Return a position list naming POSITION in the selected window."
  (list (selected-window) position '(0 . 0) 0))

(defun test-ps-open--press (position)
  "Return a `down-mouse-1' event at POSITION."
  (list 'down-mouse-1 (test-ps-open--posn position)))

(defun test-ps-open--click (position)
  "Return a `mouse-1' event at POSITION."
  (list 'mouse-1 (test-ps-open--posn position)))

(defun test-ps-open--drag (from to)
  "Return a `drag-mouse-1' event from FROM to TO."
  (list 'drag-mouse-1 (test-ps-open--posn from) (test-ps-open--posn to)))

(defmacro test-ps-open--with-clicked-buffer (&rest body)
  "Run BODY in a temporary buffer that the selected window is showing.
The click predicates answer in the buffer of the window the event names, so a
buffer that no window shows cannot be clicked in even in a test."
  (declare (indent 0) (debug t))
  `(save-window-excursion
     (with-temp-buffer
       (set-window-buffer (selected-window) (current-buffer))
       ,@body)))

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

(ert-deftest ps/open--clickable-event-p-reads-the-window-that-was-clicked ()
  "Not the current buffer: by the time the release arrives, following the link
has usually selected a different one, and the release must still recognise
that its own press landed on a link."
  (test-ps-open--with-clicked-buffer
    (insert "plain ")
    (let ((link (point)))
      (insert (propertize "link" 'mouse-face 'highlight))
      (setq-local ps/open-follow-function #'ignore)
      (should (ps/open--clickable-event-p (test-ps-open--press link)))
      (should-not (ps/open--clickable-event-p (test-ps-open--press (point-min))))
      ;; The answer must not change when some other buffer is current.
      (with-temp-buffer
        (should (ps/open--clickable-event-p (test-ps-open--press link)))))))

(ert-deftest ps/open--clickable-event-p-needs-a-buffer-that-follows-anything ()
  "`mouse-face' alone is not enough — a buffer with no follow function has
nothing a click could do, so the press must fall through to a normal drag."
  (test-ps-open--with-clicked-buffer
    (insert (propertize "link" 'mouse-face 'highlight))
    (should-not (ps/open--clickable-event-p (test-ps-open--press (point-min))))))

(ert-deftest ps/open-down-click-follows-on-the-press-without-moving-point ()
  "Point staying put is the mechanism, not a detail: moving it into the link is
what makes org-appear reveal the raw syntax and slide the rest of the line out
from under the pointer."
  (test-ps-open--with-clicked-buffer
    (insert "plain ")
    (let ((link (point))
          (followed 0)
          (dragged 0))
      (insert (propertize "link" 'mouse-face 'highlight))
      (goto-char (point-min))
      (setq-local ps/open-follow-function
                  (lambda () (interactive) (setq followed (1+ followed))))
      (cl-letf (((symbol-function 'mouse-drag-region)
                 (lambda (_event) (setq dragged (1+ dragged)))))
        (ps/open-down-click (test-ps-open--press link))
        (should (= followed 1))
        (should (= dragged 0))
        (should (= (point) (point-min)))
        ;; Off a link the press is an ordinary one.
        (ps/open-down-click (test-ps-open--press (point-min)))
        (should (= followed 1))
        (should (= dragged 1))))))

(ert-deftest ps/open-releases-decline-what-the-press-already-did ()
  "The press opens; a release that also set point or a region would undo the
point-never-moves guarantee, or select the link it just followed."
  (test-ps-open--with-clicked-buffer
    (insert "plain ")
    (let ((link (point))
          (pointed 0)
          (selected 0))
      (insert (propertize "link" 'mouse-face 'highlight))
      (setq-local ps/open-follow-function #'ignore)
      (cl-letf (((symbol-function 'mouse-drag-region) #'ignore)
                ((symbol-function 'mouse-set-point)
                 (lambda (_event &optional _promote) (setq pointed (1+ pointed))))
                ((symbol-function 'mouse-set-region)
                 (lambda (_event) (setq selected (1+ selected)))))
        (ps/open-down-click (test-ps-open--press link))
        (ps/open-click (test-ps-open--click link))
        (ps/open-down-click (test-ps-open--press link))
        (ps/open-drag-click (test-ps-open--drag link link))
        (should (= pointed 0))
        (should (= selected 0))
        ;; Off a link both releases do their ordinary work.
        (ps/open-down-click (test-ps-open--press (point-min)))
        (ps/open-click (test-ps-open--click (point-min)))
        (ps/open-down-click (test-ps-open--press (point-min)))
        (ps/open-drag-click (test-ps-open--drag (point-min) 3))
        (should (= pointed 1))
        (should (= selected 1))))))

(ert-deftest ps/open-drag-click-treats-a-wobble-as-a-click ()
  "Emacs calls a release a drag as soon as the pointer moved more than
`double-click-fuzz' pixels, which a hand on a trackpad does without meaning
to.  `mouse-set-region' on a drag of zero length leaves an *active* empty
region; nothing shows until the buffer scrolls, and then the region opens up
behind point and highlights the whole listing."
  (let ((pointed 0)
        (selected 0))
    (cl-letf (((symbol-function 'mouse-set-point)
               (lambda (_event &optional _promote) (setq pointed (1+ pointed))))
              ((symbol-function 'mouse-set-region)
               (lambda (_event) (setq selected (1+ selected)))))
      (ps/open-drag-click (test-ps-open--drag 12 12))
      (should (= pointed 1))
      (should (= selected 0))
      (ps/open-drag-click (test-ps-open--drag 12 40))
      (should (= pointed 1))
      (should (= selected 1)))))

(ert-deftest ps/open-release-remembers-the-press-across-a-buffer-change ()
  "Following a link routinely replaces the buffer in the very window that was
clicked, so a release cannot ask that window whether its own press landed on a
link -- it would be asking about different text.  It said no, and drew a
selection across whatever had just opened.  Dired's `..' showed it every time."
  (test-ps-open--with-clicked-buffer
    (insert (propertize "link" 'mouse-face 'highlight))
    (let ((selected 0)
          (replacement (generate-new-buffer " ps-open-test-opened")))
      (unwind-protect
          (progn
            (setq-local ps/open-follow-function
                        (lambda ()
                          (interactive)
                          ;; What opening something in this window looks like.
                          (set-window-buffer (selected-window) replacement)))
            (cl-letf (((symbol-function 'mouse-set-region)
                       (lambda (_event) (setq selected (1+ selected)))))
              (ps/open-down-click (test-ps-open--press (point-min)))
              ;; The clicked window now shows plain text with no `mouse-face'.
              (should-not (ps/open--clickable-event-p (test-ps-open--press (point-min))))
              (ps/open-drag-click (test-ps-open--drag (point-min) 3))
              (should (= selected 0))))
        (kill-buffer replacement)))))

(ert-deftest ps/open--take-press-followed-does-not-outlive-one-release ()
  "A release with no press of ours before it must not inherit a stale answer."
  (let ((ps/open--press-followed t))
    (should (ps/open--take-press-followed))
    (should-not (ps/open--take-press-followed))))

(ert-deftest ps/open-release-stands-down-in-a-buffer-that-binds-nothing ()
  "The release is delivered to whatever buffer the click *opened*, not to the
one it was aimed at.  A JSON buffer binds no mouse of its own, so the release
reached Emacs's `mouse-set-region', which set an active mark wherever the
press had landed and let scrolling grow a region down the file.  Hence the
global bindings: the release has to stand down wherever it arrives."
  (let ((selected 0))
    (cl-letf (((symbol-function 'mouse-set-region)
               (lambda (_event) (setq selected (1+ selected)))))
      (let ((ps/open--press-followed t))
        ;; A buffer with no `ps/open-follow-function' at all -- the release
        ;; still has to know the press already did the work.
        (with-temp-buffer
          (ps/open-drag-click (test-ps-open--drag 1 1))))
      (should (= selected 0)))))

(ert-deftest ps/open-click-setup-keeps-what-the-global-map-already-did ()
  "These go in the global map, so anything they drop is dropped everywhere.
`mouse-set-point's second argument is what makes a double click select a word."
  (should (equal (cadr (interactive-form #'ps/open-click))
                 (cadr (interactive-form #'mouse-set-point))))
  (let ((previous (current-global-map)))
    (unwind-protect
        (progn
          (use-global-map (make-sparse-keymap))
          (ps/open-click-setup)
          (should (eq (lookup-key (current-global-map) [mouse-1]) #'ps/open-click))
          (should (eq (lookup-key (current-global-map) [drag-mouse-1])
                      #'ps/open-drag-click)))
      (use-global-map previous))))

(ert-deftest ps/open-click-promotes-a-double-click-to-a-word ()
  "Off a link this must behave exactly as the global binding it replaces."
  (let ((promoted nil))
    (cl-letf (((symbol-function 'mouse-set-point)
               (lambda (_event &optional promote) (setq promoted promote))))
      (let ((ps/open--press-followed nil))
        (ps/open-click (test-ps-open--click 1) 2))
      (should (equal promoted 2)))))

(ert-deftest ps/open--dired-header-directory-reads-the-component-at-point ()
  "Dired's header spells the folder out one clickable component at a time.
Emacs reaches those by rewriting a click into a mouse-2, which is turned off
in these buffers -- so clicking one silently did nothing while still showing a
pointing hand."
  (require 'dired)
  (let* ((root (make-temp-file "ps-open-header" t))
         (child (expand-file-name "inner" root))
         (buffer nil))
    (unwind-protect
        (progn
          (make-directory child)
          (setq buffer (dired-noselect child))
          (with-current-buffer buffer
            (goto-char (point-min))
            ;; Only the ancestors are clickable -- clicking the folder already
            ;; being shown would go nowhere -- so the last clickable component
            ;; of `root/inner' is `root', which is where clicking it lands.
            (let ((last nil) (pos (pos-bol)) (end (pos-eol)))
              (while (< pos end)
                (when (get-text-property pos 'mouse-face) (setq last pos))
                (setq pos (1+ pos)))
              (should last)
              (goto-char last)
              (should (equal (file-truename (ps/open--dired-header-directory))
                             (file-truename root))))
            ;; A file line is not the header, whatever else it carries.
            (goto-char (point-min))
            (forward-line 2)
            (should-not (ps/open--dired-header-directory))))
      (when buffer (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest ps/open-bind-click-binds-the-press-and-both-releases ()
  "Binding the press without the releases leaves Emacs setting point after the
link has already opened; binding a release without the press is the two-click
bug this whole section exists to fix."
  (let ((map (make-sparse-keymap)))
    (ps/open-bind-click map)
    (should (eq (lookup-key map [down-mouse-1]) #'ps/open-down-click))
    (should (eq (lookup-key map [mouse-1]) #'ps/open-click))
    (should (eq (lookup-key map [drag-mouse-1]) #'ps/open-drag-click))))

(ert-deftest ps/open-setup-click-turns-off-the-rewrite-it-would-duplicate ()
  "Left on, `mouse-1-click-follows-link' rewrites the release into a mouse-2 —
a second, independent way to open what the press has already opened."
  (with-temp-buffer
    (ps/open-setup-click #'ignore)
    (should (eq ps/open-follow-function #'ignore))
    (should (local-variable-p 'mouse-1-click-follows-link))
    (should-not mouse-1-click-follows-link)))

;;; test-ps-open.el ends here
