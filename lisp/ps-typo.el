;;; ps-typo.el --- High-precision, multilingual typo checking for Org -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A quiet, multilingual word-level typo checker for Org buffers, built on top
;; of Jinx (https://github.com/minad/jinx, an `enchant'-backed spell checker).
;;
;; Design goals (see docs/typo-checker.md for the full rationale):
;;
;; * High precision, low recall.  A flagged word should almost always be a real
;;   typo.  Recall is intentionally sacrificed.
;;
;; * Multilingual by union.  Every configured language is loaded at once and a
;;   word is flagged only when *no* language recognises it.  So "calor" (valid
;;   Spanish) and Cyrillic words are never flagged inside English prose.  This
;;   is the highest-precision rule available and needs no language detection.
;;
;; * Skips code and markup.  Org src/example blocks, =verbatim=, ~code~, links,
;;   file paths and "code-like" tokens (camelCase, snake_case, URLs, @handles,
;;   versions) are never checked, using Jinx's face/regexp exclusion driven by
;;   Org's own fontification.
;;
;; * Cheap.  Jinx checks only the visible window on an idle timer; no LLM.
;;
;; This module is the thin, testable glue around Jinx: the regexp exclusion set,
;; the optional near-miss precision gate, and the optional correction-suggestion
;; ordering by detected language.  Dictionary lookup, suggestions and rendering
;; stay inside Jinx.  Nothing here requires Jinx to be installed at *load* time;
;; the engine is only touched from `ps/typo-setup' onwards, so the pure helpers
;; remain unit-testable with a bare `emacs -Q'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;; Jinx / guess-language are optional at load time; declare to quiet the byte
;; compiler.  They are soft-required from `ps/typo-setup'.
(declare-function jinx-mode "ext:jinx" (&optional arg))
(declare-function jinx-correct "ext:jinx")
(declare-function jinx--mod-suggest "ext:jinx" (dict word))
(declare-function jinx--word-valid-p "ext:jinx" (word))
(declare-function jinx--correct-suggestions "ext:jinx" (word))
(declare-function guess-language-region "ext:guess-language" (beg end))
(defvar jinx-languages)
(defvar jinx-exclude-faces)
(defvar jinx-exclude-regexps)
(defvar jinx-mode-map)
(defvar jinx--dicts)
(defvar guess-language-languages)

;;; Customization

(defcustom ps/typo-languages '("en_US" "ru_RU" "es_ES")
  "Languages accepted as valid, as a list of dictionary/locale codes.
A word is flagged as a typo only when it is unknown to *every* language in
this list (union semantics) — the highest-precision rule.  Extend this list
to add languages you write in; each needs a dictionary installed for
`enchant' (see README.md)."
  :type '(repeat string)
  :group 'ps-typo)

(defcustom ps/typo-min-length 3
  "Minimum length (in characters) of a word that may be flagged.
Shorter alphabetic tokens are skipped, since one- and two-letter non-words
are rarely worth flagging and are a common source of noise."
  :type 'integer
  :group 'ps-typo)

(defcustom ps/typo-near-miss-gate nil
  "When non-nil, only flag an unknown word if it looks like a near miss.
A word unknown to every dictionary is flagged only when the engine can offer
a suggestion within `ps/typo-near-miss-max-distance' edits — i.e. it really
looks like a misspelling of a real word, not an unfamiliar name or term.
This raises precision further at a small CPU cost.  Disabled by default so
you can A/B it against the plain union behaviour."
  :type 'boolean
  :group 'ps-typo)

(defcustom ps/typo-near-miss-max-distance 2
  "Maximum Levenshtein distance for the `ps/typo-near-miss-gate'.
A flagged word must be within this many character edits of some dictionary
suggestion to survive the gate."
  :type 'integer
  :group 'ps-typo)

(defcustom ps/typo-order-suggestions-by-language t
  "When non-nil, order `M-$' correction suggestions by the local language.
The language of the text around point is detected (cheaply, via
`guess-language') and that dictionary's suggestions are listed first.  This
never affects *whether* a word is flagged — only the order in which fixes are
offered.  Set to nil to disable, or if you do not want `guess-language'."
  :type 'boolean
  :group 'ps-typo)

(defcustom ps/typo-underline-color 'auto
  "Color of the subtle wavy underline drawn under misspelled words.
Either a color name/hex string, or the symbol `auto' to derive a muted,
theme-appropriate color from the `shadow' face."
  :type '(choice (string :tag "Color")
                 (const :tag "Derive from theme (shadow face)" auto))
  :group 'ps-typo)

(defcustom ps/typo-exclude-faces
  '(org-block org-block-begin-line org-block-end-line
    org-code org-verbatim org-meta-line org-link
    org-cite org-cite-key org-footnote
    org-special-keyword org-property-value org-drawer
    org-date org-tag org-todo org-done org-checkbox
    org-table org-formula org-latex-and-related
    org-document-info org-document-info-keyword
    ;; Native fontification inside #+begin_src blocks uses the language's own
    ;; faces (not org-block on every character), so exclude the common ones too.
    font-lock-comment-face font-lock-comment-delimiter-face
    font-lock-string-face font-lock-doc-face
    font-lock-keyword-face font-lock-function-name-face
    font-lock-variable-name-face font-lock-type-face
    font-lock-constant-face font-lock-builtin-face
    font-lock-preprocessor-face)
  "Faces whose text is never spell-checked in Org buffers.
These are merged into Jinx's own `jinx-exclude-faces' for `org-mode', so they
add to (rather than replace) Jinx's defaults.  Excluding a face can only skip
text, never cause a false positive, so it is safe to be generous here."
  :type '(repeat face)
  :group 'ps-typo)

(defcustom ps/typo-exclude-regexps
  '("[[:alpha:]]+[._/:][[:alnum:]._/:~-]*"   ; foo.bar  a/b  ns::name  paths
    "[[:lower:]]+[[:upper:]][[:alnum:]]*"     ; camelCase / dromedaryCase
    "[[:alpha:]]*_[[:alnum:]_]*"              ; snake_case / __dunder__
    "[@#$][[:alnum:]_-]+"                     ; @handle  #tag  $var
    "\\\\[[:alpha:]]+")                       ; \command  (LaTeX / escapes)
  "Extra regexps for tokens that are never spell-checked.
These are merged into Jinx's own `jinx-exclude-regexps' (which already skips
ALL-CAPS acronyms, versions/numbers, URLs and emails), adding patterns for
\"looks like code\" tokens.  As with faces, over-matching only reduces recall,
never precision."
  :type '(repeat regexp)
  :group 'ps-typo)

;;; Pure helpers (no Jinx required — unit-tested)

(defun ps/typo--locale->lang (locale)
  "Map a LOCALE code like \"en_US\" to a short language symbol like `en'."
  (intern (downcase (car (split-string locale "[_-]")))))

(defun ps/typo--code-like-p (word)
  "Return non-nil if WORD matches any pattern in `ps/typo-exclude-regexps'.
This mirrors the regexp set handed to Jinx, documenting and testing which
tokens are treated as code and therefore never spell-checked.  Matching is
case-sensitive (as Jinx matches its exclusions), so e.g. the camelCase rule
does not accidentally swallow ordinary lowercase words."
  (let ((case-fold-search nil))
    (seq-some (lambda (re) (string-match-p re word)) ps/typo-exclude-regexps)))

(defun ps/typo--near-miss-keep-p (word suggestions)
  "Return non-nil if WORD is a plausible near-miss of some SUGGESTIONS.
WORD survives the near-miss gate when at least one suggestion is within
`ps/typo-near-miss-max-distance' character edits (case-insensitive).  With no
suggestions, returns nil (the word looks like an unknown name/term, not a
typo, so it should not be flagged)."
  (let ((w (downcase word)))
    (seq-some (lambda (s)
                (<= (string-distance w (downcase s))
                    ps/typo-near-miss-max-distance))
              suggestions)))

(defun ps/typo--order-langs (langs detected)
  "Return LANGS reordered so the entry matching DETECTED comes first.
LANGS is a list of locale codes like \"en_US\".  DETECTED is a language as a
symbol or string (e.g. `es' or \"es\"); matching is by the leading code,
case-insensitively.  Order is otherwise preserved, and LANGS is returned
unchanged when DETECTED is nil or matches nothing."
  (if (null detected)
      langs
    (let* ((code (downcase (if (symbolp detected) (symbol-name detected)
                             detected)))
           (match (seq-find (lambda (l) (string-prefix-p code (downcase l)))
                            langs)))
      (if match
          (cons match (remove match langs))
        langs))))

(defun ps/typo--order-dicts (langs dicts detected)
  "Reorder DICTS in parallel with LANGS so DETECTED's dictionary comes first.
LANGS and DICTS are equal-length parallel lists (Jinx keeps them in sync).
Returns a new list of dicts; unchanged when DETECTED matches nothing."
  (let ((ordered (ps/typo--order-langs langs detected)))
    (mapcar (lambda (l) (nth (seq-position langs l) dicts)) ordered)))

(defun ps/typo--merge-into-alist (alist key values)
  "Return ALIST with VALUES appended (deduped) to the entry for KEY.
KEY is compared with `eq' (suits symbol and t keys).  Existing order and
entries are preserved; only genuinely new VALUES are added."
  (let* ((entry (assq key alist))
         (existing (cdr entry))
         (new (seq-remove (lambda (v) (member v existing)) values)))
    (cond
     ((null new) alist)
     (entry
      (cons (cons key (append existing new))
            (assq-delete-all key (copy-sequence alist))))
     (t (cons (cons key (append new nil)) alist)))))

(defun ps/typo--short-word-regexp ()
  "Return a regexp that matches whole alphabetic words below `ps/typo-min-length'.
Returns nil when `ps/typo-min-length' is 1 or less (nothing to exclude)."
  (when (> ps/typo-min-length 1)
    (format "\\<[[:alpha:]]\\{1,%d\\}\\>" (1- ps/typo-min-length))))

(defun ps/typo--underline-color ()
  "Resolve `ps/typo-underline-color' to a concrete color string."
  (if (eq ps/typo-underline-color 'auto)
      (or (face-foreground 'shadow nil t) "gray50")
    ps/typo-underline-color))

;;; Jinx integration

(defun ps/typo--apply-face (&rest _)
  "Style `jinx-misspelled' as a subtle wavy underline, theme-aware.
Added to `enable-theme-functions' so the color tracks theme changes."
  (when (facep 'jinx-misspelled)
    (set-face-attribute 'jinx-misspelled nil
                        :inherit 'unspecified
                        :underline `(:style wave :color ,(ps/typo--underline-color)))))

(defun ps/typo--suggestions (word)
  "Return raw spell suggestions for WORD across the active Jinx dictionaries."
  (when (and (fboundp 'jinx--mod-suggest) (boundp 'jinx--dicts))
    (cl-loop for dict in jinx--dicts
             nconc (copy-sequence (jinx--mod-suggest dict word)))))

(defun ps/typo--word-valid-advice (orig word)
  "Around-advice for `jinx--word-valid-p' implementing the near-miss gate.
ORIG is the original predicate.  WORD is the word string, or (as Jinx also
allows) a buffer position whose word ends at point.  When the gate is on, a
word Jinx considers misspelled is treated as valid (NOT flagged) unless it is
a near miss of a real word."
  (or (funcall orig word)
      (and ps/typo-near-miss-gate
           (condition-case nil
               (let ((str (if (stringp word) word
                            (buffer-substring-no-properties word (point)))))
                 (not (ps/typo--near-miss-keep-p str (ps/typo--suggestions str))))
             (error nil)))))

(defun ps/typo--detect-language ()
  "Detect the language around point with `guess-language'.
Return a language symbol (e.g. `es') or nil when unavailable/inconclusive.
Cheap: a trigram scan of the surrounding paragraph only."
  (when (fboundp 'guess-language-region)
    (ignore-errors
      (save-excursion
        (let ((beg (progn (backward-paragraph) (point)))
              (end (progn (forward-paragraph) (point))))
          (guess-language-region beg end))))))

(defun ps/typo--suggestions-order-advice (orig word)
  "Around-advice for `jinx--correct-suggestions' to surface the local language.
Detects the language of the surrounding text and, when it matches a configured
dictionary, lists that dictionary's suggestions for WORD first.  Defensive:
any failure falls back to ORIG unchanged, so suggestions always work."
  (if (not ps/typo-order-suggestions-by-language)
      (funcall orig word)
    (condition-case nil
        (let* ((detected (ps/typo--detect-language))
               (langs (and (boundp 'jinx-languages)
                           (split-string (or jinx-languages "") nil t))))
          (if (and detected langs (boundp 'jinx--dicts)
                   (= (length langs) (length jinx--dicts)))
              (let ((jinx--dicts (ps/typo--order-dicts langs jinx--dicts detected)))
                (funcall orig word))
            (funcall orig word)))
      (error (funcall orig word)))))

(defun ps/typo--configure-exclusions ()
  "Merge this module's face/regexp exclusions into Jinx's, for `org-mode'."
  (when (boundp 'jinx-exclude-faces)
    (setq jinx-exclude-faces
          (ps/typo--merge-into-alist jinx-exclude-faces 'org-mode
                                     ps/typo-exclude-faces)))
  (when (boundp 'jinx-exclude-regexps)
    (let ((short (ps/typo--short-word-regexp)))
      (setq jinx-exclude-regexps
            (ps/typo--merge-into-alist
             jinx-exclude-regexps t
             (append ps/typo-exclude-regexps
                     (and short (list short))))))))

(defun ps/typo--enable ()
  "Turn on `jinx-mode' in the current buffer, swallowing setup errors.
Added to `org-mode-hook'.  If the Jinx module or `enchant' is not installed,
typo checking is silently skipped rather than aborting Org startup."
  (when (fboundp 'jinx-mode)
    (condition-case err
        (jinx-mode 1)
      (error (message "ps-typo: could not enable jinx-mode: %s"
                      (error-message-string err))))))

;;;###autoload
(defun ps/typo-setup ()
  "Configure Jinx-based, multilingual, high-precision typo checking for Org.
Idempotent.  Safe to call when Jinx/`enchant' are not yet installed — it warns
and does nothing.  Intended to be called once from config.org; it registers
`org-mode-hook' so every Org buffer gets the checker."
  (interactive)
  (if (not (require 'jinx nil t))
      (message "ps-typo: `jinx' not available; skipping typo checker setup")
    (require 'guess-language nil t)
    ;; Accepted languages (union) + candidate set for detection.
    (setq jinx-languages (string-join ps/typo-languages " "))
    (when (boundp 'guess-language-languages)
      (setq guess-language-languages
            (mapcar #'ps/typo--locale->lang ps/typo-languages)))
    ;; What never gets checked (code, markup, links, short/code-like tokens).
    (ps/typo--configure-exclusions)
    ;; Subtle wavy underline, kept in sync with the active theme.
    (ps/typo--apply-face)
    (when (boundp 'enable-theme-functions)
      (add-hook 'enable-theme-functions #'ps/typo--apply-face))
    ;; Optional precision/quality layers (advice is idempotent).
    (when (fboundp 'jinx--word-valid-p)
      (advice-add 'jinx--word-valid-p :around #'ps/typo--word-valid-advice))
    (when (fboundp 'jinx--correct-suggestions)
      (advice-add 'jinx--correct-suggestions :around
                  #'ps/typo--suggestions-order-advice))
    ;; Correction.  Jinx already binds `M-$' and right-click on the misspelled
    ;; word itself; also bind `M-$' in the mode map so it corrects the nearest
    ;; misspelling even when point is not exactly on a flagged word.  Both
    ;; `jinx-correct' prompts offer "add to personal dictionary".
    (when (boundp 'jinx-mode-map)
      (keymap-set jinx-mode-map "M-$" #'jinx-correct))
    ;; Enable in Org buffers only.
    (add-hook 'org-mode-hook #'ps/typo--enable)))

(provide 'ps-typo)
;;; ps-typo.el ends here
