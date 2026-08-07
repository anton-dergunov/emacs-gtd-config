;;; ps-emphasis.el --- Render Org emphasis in generated buffers -*- lexical-binding: t; -*-

;;; Commentary:

;; Org fontifies `*bold*', `/italic/', `=code=' and friends in Org buffers, but
;; the agenda is a *generated* buffer: `ps/agenda-layout--title' copies the
;; heading out with `substring-no-properties', so the markers arrive as literal
;; text.
;;
;; `ps/emphasis-render' turns such a string into what Org would have shown: the
;; markers are dropped and the face from `org-emphasis-alist' is applied to the
;; text they wrapped.
;;
;; The markers are *deleted* rather than hidden with an `invisible' property
;; because `string-width' counts invisible characters, and the agenda lays its
;; columns out by width -- hiding would push every emphasised row a couple of
;; columns out of line.
;;
;; This is a display-layer cleanup, not an Org renderer: like
;; `ps/mode-line--clean-markup', it approximates nesting (emphasis found inside
;; a verbatim span is still marked up).

;;; Code:

(require 'org)

(defun ps/emphasis--face (marker)
  "Return the face `org-emphasis-alist' gives MARKER, a marker string."
  (nth 1 (assoc marker org-emphasis-alist)))

(defun ps/emphasis--render-1 (s re)
  "Return S with every emphasis matching RE unwrapped and faced.
RE is `org-emph-re' or `org-verbatim-re'; both group the whole emphasis
\(markers included) as 2, the marker as 3 and the wrapped body as 4."
  (let ((out "")
        (pos 0))
    (while (string-match re s pos)
      (let ((body (copy-sequence (match-string 4 s)))
            (face (ps/emphasis--face (match-string 3 s))))
        (when face
          (add-face-text-property 0 (length body) face nil body))
        ;; Everything before the markers, including the match's leading
        ;; pre-character (group 1), is carried over untouched.
        (setq out (concat out (substring s pos (match-beginning 2)) body))
        ;; Resume at the closing marker's end, so the trailing post-character
        ;; (group 5) is still available as the next match's pre-character.
        (setq pos (match-end 2))))
    (concat out (substring s pos))))

(defun ps/emphasis-render (s)
  "Return S with Org emphasis markers removed and their faces applied.
Existing text properties on S are preserved; the emphasis faces are added on
top of whatever face S already carries.  Returns S unchanged when it holds no
emphasis."
  (if (or (null s) (string-empty-p s))
      s
    ;; Verbatim first: `=' and `~' suppress markup inside them in Org, so
    ;; unwrapping them before the general pass is the closer approximation.
    (ps/emphasis--render-1
     (ps/emphasis--render-1 s org-verbatim-re)
     org-emph-re)))

(provide 'ps-emphasis)
;;; ps-emphasis.el ends here
