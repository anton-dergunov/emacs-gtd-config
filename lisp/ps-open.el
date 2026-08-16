;;; ps-open.el --- Decide what opening a file should actually do -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs is used here for planning, not for editing every kind of file that
;; happens to sit next to a plan.  So "open this file" is not one operation: a
;; Markdown note should render in Emacs, an HTML page should be a page and not
;; its source, a PDF belongs in Preview where it is legible, a video belongs in
;; the system player, and a 6 MB `.mp4' opened as text is never what anyone
;; meant -- it is a wedged frame and a buffer to kill.
;;
;; `ps/open-handlers' is that policy as one editable list (see config.org's
;; `** Opening files' settings block), and `ps/open-file' is the single place
;; every click goes through: the file tree, Dired, an Org link, a Markdown link.
;;
;; Two decisions worth keeping:
;;
;; There is deliberately NO advice on `find-file' or `find-file-noselect'.  A
;; global hook there would have to be right about every internal caller in
;; Emacs, and `image-mode' and `archive-mode' visit binary files entirely
;; legitimately.  The guard sits on the paths a person actually clicks, and
;; `C-x C-f' with a typed name is left alone -- someone who typed the whole
;; name meant it.
;;
;; The binary check is a fallback, not the mechanism.  Anything named in
;; `ps/open-handlers' is decided by name, which is cheap and predictable; the
;; content sniff only answers for extensions nobody thought about, so a new
;; capture format that turns up in an inbox one day fails safe rather than
;; wedging the frame.

;;; Code:

(require 'seq)

(declare-function ps/window-replace-here "ps-window")
(declare-function ps/window-visit-here "ps-window")
(declare-function ps/window--select-main "ps-window")
(declare-function ps/nav-note-departure "ps-nav")
(declare-function eww-open-file "eww")

(defgroup ps/open nil
  "What opening a file of a given kind should do."
  :group 'ps)

(defcustom ps/open-handlers
  '(("\\.\\(?:md\\|markdown\\|mdown\\)\\'"        . emacs)
    ("\\.\\(?:org\\|txt\\|text\\|json\\|ya?ml\\|csv\\|tsv\\|log\\|el\\)\\'" . emacs)
    ("\\.\\(?:html?\\|xhtml\\)\\'"                . browser)
    ("\\.\\(?:jpe?g\\|png\\|gif\\|webp\\|bmp\\|svg\\)\\'" . emacs)
    ("\\.\\(?:pdf\\|epub\\)\\'"                   . external)
    ("\\.\\(?:mp4\\|mov\\|m4v\\|webm\\|mkv\\|avi\\)\\'"   . external)
    ("\\.\\(?:ogg\\|opus\\|mp3\\|m4a\\|wav\\|aac\\|flac\\)\\'" . refuse)
    ("\\.\\(?:zip\\|gz\\|tgz\\|bz2\\|xz\\|dmg\\|pkg\\)\\'"     . refuse))
  "How to open a file, by name.  An alist of (REGEXP . HANDLER), first match wins.

REGEXP is matched against the file's full name, so it can key on a directory
as well as an extension.  HANDLER is one of:

  `emacs'     visit it in Emacs, in the window you clicked from
  `browser'   render it as a page inside Emacs (`eww'), not as source
  `external'  hand it to the desktop (`ps/open-external-command')
  `refuse'    decline, and say why

or a function of one argument, the expanded file name.

A file matching nothing here is decided by looking at it: text opens in Emacs,
anything that is not text asks first.  See `ps/open-file'."
  :type '(alist :key-type regexp
                :value-type (choice (const emacs) (const browser)
                                    (const external) (const refuse) function))
  :group 'ps/open)

(defcustom ps/open-external-command
  (if (eq system-type 'darwin) "open" "xdg-open")
  "Command that hands a file to the desktop's own application for it."
  :type 'string
  :group 'ps/open)

(defcustom ps/open-refusal-reasons
  '(("\\.\\(?:ogg\\|opus\\|mp3\\|m4a\\|wav\\|aac\\|flac\\)\\'"
     . "audio -- the transcript beside it is the readable copy")
    ("\\.\\(?:zip\\|gz\\|tgz\\|bz2\\|xz\\|dmg\\|pkg\\)\\'" . "an archive"))
  "Why a `refuse' handler refuses, by file name.  An alist of (REGEXP . REASON).
REASON completes the sentence \"Not opening FILE: it is ...\"."
  :type '(alist :key-type regexp :value-type string)
  :group 'ps/open)

(defcustom ps/open-binary-sample 4096
  "How many bytes of an unrecognised file to look at before deciding it is binary."
  :type 'integer
  :group 'ps/open)

;;; Deciding

(defun ps/open-handler (file)
  "Return the handler `ps/open-handlers' names for FILE, or nil for none."
  (cdr (seq-find (lambda (entry) (string-match-p (car entry) file))
                 ps/open-handlers)))

(defun ps/open--refusal-reason (file)
  "Return why FILE is refused, phrased to complete \"it is ...\"."
  (or (cdr (seq-find (lambda (entry) (string-match-p (car entry) file))
                     ps/open-refusal-reasons))
      "not a file Emacs shows usefully"))

(defun ps/open--binary-string-p (string)
  "Non-nil when STRING looks like the start of a binary file.

A NUL byte settles it -- no text encoding this config will meet puts one in a
file.  Failing that, a high share of other control characters does: UTF-8
prose is essentially all printable plus tab, newline and return, while a
container format's header is not.  Kept pure so it is testable without a file."
  (let ((control 0)
        (length (length string)))
    (if (string-search "\0" string)
        t
      (dolist (character (string-to-list string))
        (when (and (< character 32) (not (memq character '(?\t ?\n ?\r ?\f))))
          (setq control (1+ control))))
      (and (> length 0) (> (/ (* 100.0 control) length) 10.0)))))

(defun ps/open--binary-file-p (file)
  "Non-nil when FILE's opening bytes look binary."
  (and (file-readable-p file)
       (with-temp-buffer
         (set-buffer-multibyte nil)
         (insert-file-contents-literally file nil 0 ps/open-binary-sample)
         (ps/open--binary-string-p (buffer-string)))))

(defun ps/open-resolve (file)
  "Return the handler to use for FILE, consulting its content if need be.
`ps/open-handlers' decides by name where it can; an extension nobody listed
falls through to a look at the file itself, so an unexpected capture format
fails safe instead of wedging the frame."
  (or (ps/open-handler file)
      (if (ps/open--binary-file-p file) 'ask 'emacs)))

;;; Doing

(defun ps/open-externally (file)
  "Hand FILE to the desktop's own application for it.
Runs detached (a nil buffer and a zero destination), so a PDF viewer that
takes two seconds to start does not hold Emacs while it does."
  (let ((program (executable-find ps/open-external-command)))
    (unless program
      (user-error "Cannot open %s: no `%s' on PATH" file ps/open-external-command))
    (call-process program nil 0 nil (expand-file-name file))
    (message "Opened %s outside Emacs" (file-name-nondirectory file))))

(defun ps/open-in-browser (file)
  "Render FILE as a page inside Emacs rather than showing its source."
  (require 'eww)
  (ps/window--select-main)
  (when (fboundp 'ps/nav-note-departure) (ps/nav-note-departure))
  (eww-open-file (expand-file-name file)))

;;;###autoload
(defun ps/open-file (file)
  "Open FILE the way `ps/open-handlers' says to, in the selected window.

This is the one entry point every click goes through -- the file tree, Dired,
an Org link, a Markdown link -- so the policy is stated once and applies
everywhere.  Navigation replaces the selected window rather than adding one:
see `ps/window-replace-here'."
  (interactive "fOpen file: ")
  (let* ((file (expand-file-name file))
         (handler (ps/open-resolve file)))
    (cond
     ((not (file-exists-p file)) (user-error "No such file: %s" file))
     ((file-directory-p file) (ps/window-visit-here file))
     ((functionp handler) (funcall handler file))
     ((eq handler 'external) (ps/open-externally file))
     ((eq handler 'browser) (ps/open-in-browser file))
     ((eq handler 'refuse)
      (user-error "Not opening %s: it is %s"
                  (file-name-nondirectory file) (ps/open--refusal-reason file)))
     ((eq handler 'ask)
      ;; Deliberately `yes-or-no-p': the cost of getting this wrong is a wedged
      ;; frame, which is more than a stray `y' should be able to buy.
      (if (yes-or-no-p (format "%s does not look like text.  Open it in Emacs anyway? "
                               (file-name-nondirectory file)))
          (ps/window-visit-here file)
        (message "Left %s closed" (file-name-nondirectory file))))
     (t (ps/window-visit-here file)))))

;;; The two places a click on a file name comes from

(declare-function dired-get-file-for-visit "dired")
(declare-function markdown-link-url "markdown-mode")
(declare-function markdown-enter-key "markdown-mode")

;;;###autoload
(defun ps/open-dired-thing ()
  "Open the file or folder at point in Dired, in this window.
Files go through `ps/open-file'; folders stay in Dired.  Bound over Dired's
own commands because the stock ones open in *another* window, which is what
turns walking one folder into a ping-pong between two of them."
  (interactive)
  (let ((target (dired-get-file-for-visit)))
    (if (file-directory-p target)
        (ps/window-visit-here target)
      (ps/open-file target))))

;;;###autoload
(defun ps/open-dired-thing-at-mouse (event)
  "Open whatever EVENT clicked on in Dired, in this window."
  (interactive "e")
  (mouse-set-point event)
  (ps/open-dired-thing))

(defun ps/open--url-p (target)
  "Non-nil when TARGET is an absolute URL rather than a path."
  (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" target))

;;;###autoload
(defun ps/open-markdown-thing ()
  "Follow the Markdown link at point through `ps/open-file'.
With no link at point, does what RET does in Markdown normally -- binding
this to RET must not stop a Markdown buffer being typed in."
  (interactive)
  (let ((target (and (fboundp 'markdown-link-url) (markdown-link-url))))
    (cond
     ((null target) (call-interactively #'markdown-enter-key))
     ((ps/open--url-p target) (browse-url target))
     (t (ps/open-file (expand-file-name target default-directory))))))

(provide 'ps-open)
;;; ps-open.el ends here
