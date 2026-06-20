;;; test-ps-typo.el --- ERT tests for ps-typo -*- lexical-binding: t; -*-

;; These tests cover the pure, engine-independent logic of ps-typo: the
;; defcustom defaults, the "code-like" regexp set, the near-miss gate decision,
;; language/dictionary ordering, and the alist merge used to extend Jinx's
;; exclusions.  Jinx and guess-language are NOT required (tests run under
;; `emacs -Q'); the module is designed to load without them.

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path "lisp")
(require 'ps-typo)

;;; -------------------------------------------------------
;;; defcustom defaults
;;; -------------------------------------------------------

(ert-deftest ps/typo--defaults ()
  "The user-facing defcustoms have the documented defaults."
  (should (equal ps/typo-languages '("en_US" "ru_RU" "es_ES")))
  (should (= ps/typo-min-length 3))
  (should (null ps/typo-near-miss-gate))
  (should (= ps/typo-near-miss-max-distance 2))
  (should (eq ps/typo-order-suggestions-by-language t))
  (should (eq ps/typo-underline-color 'auto))
  (should (null ps/typo-inhibit-predicate)))

;;; -------------------------------------------------------
;;; enable gate (ps/typo-inhibit-predicate)
;;; -------------------------------------------------------

(ert-deftest ps/typo--enable-respects-inhibit-predicate ()
  "`ps/typo--enable' skips jinx-mode exactly when the predicate returns non-nil."
  (let (enabled)
    (cl-letf (((symbol-function 'jinx-mode)
               (lambda (&optional _arg) (setq enabled t))))
      ;; Predicate says "inhibit" -> jinx-mode must NOT be called.
      (let ((ps/typo-inhibit-predicate (lambda () t)))
        (setq enabled nil)
        (ps/typo--enable)
        (should-not enabled))
      ;; Predicate says "do not inhibit" -> jinx-mode IS called.
      (let ((ps/typo-inhibit-predicate (lambda () nil)))
        (setq enabled nil)
        (ps/typo--enable)
        (should enabled))
      ;; No predicate at all -> jinx-mode IS called.
      (let ((ps/typo-inhibit-predicate nil))
        (setq enabled nil)
        (ps/typo--enable)
        (should enabled)))))

;;; -------------------------------------------------------
;;; locale -> language symbol
;;; -------------------------------------------------------

(ert-deftest ps/typo--locale->lang-strips-region ()
  (should (eq (ps/typo--locale->lang "en_US") 'en))
  (should (eq (ps/typo--locale->lang "ru_RU") 'ru))
  (should (eq (ps/typo--locale->lang "es-ES") 'es))
  (should (eq (ps/typo--locale->lang "de") 'de)))

;;; -------------------------------------------------------
;;; code-like detection (the regexp exclusion set)
;;; -------------------------------------------------------

(ert-deftest ps/typo--code-like-detects-code-tokens ()
  "Tokens that look like code are recognised and would be skipped."
  (dolist (w '("camelCase" "snake_case" "__dunder__"
               "foo.bar" "a/b/c" "ns::name" "config.org"
               "@handle" "#tag" "$var" "\\alpha"))
    (should (ps/typo--code-like-p w))))

(ert-deftest ps/typo--code-like-passes-ordinary-words ()
  "Ordinary words in several languages are NOT treated as code."
  (dolist (w '("color" "calor" "colour" "Madrid"
               "recieve" "teh" "playa" "цвет" "привет" "niño"))
    (should-not (ps/typo--code-like-p w))))

(ert-deftest ps/typo--code-like-passes-sentence-final-words ()
  "A word immediately followed by a separator with nothing after it (e.g.
the last word of a sentence, before the closing punctuation) is NOT
treated as code -- only a genuine multi-segment token like \"foo.bar\" is."
  (dolist (w '("typo." "sentence:" "path/"))
    (should-not (ps/typo--code-like-p w))))

;;; -------------------------------------------------------
;;; near-miss gate
;;; -------------------------------------------------------

(ert-deftest ps/typo--near-miss-keeps-close-typo ()
  "A word within the edit-distance threshold of a suggestion is kept (flagged)."
  (should (ps/typo--near-miss-keep-p "teh" '("the" "tech" "ten")))
  (should (ps/typo--near-miss-keep-p "recieve" '("receive"))))

(ert-deftest ps/typo--near-miss-drops-distant-word ()
  "A word far from every suggestion (a likely name/term) is dropped (not flagged)."
  (should-not (ps/typo--near-miss-keep-p "Kubernetes" '("the" "color")))
  (should-not (ps/typo--near-miss-keep-p "Anthropic" nil)))

(ert-deftest ps/typo--near-miss-is-case-insensitive ()
  (should (ps/typo--near-miss-keep-p "Teh" '("The"))))

(ert-deftest ps/typo--near-miss-respects-max-distance ()
  "The threshold is honoured: distance 3 fails at the default max of 2."
  (let ((ps/typo-near-miss-max-distance 2))
    (should-not (ps/typo--near-miss-keep-p "abcd" '("wxyz"))))
  (let ((ps/typo-near-miss-max-distance 4))
    (should (ps/typo--near-miss-keep-p "abcd" '("wxyz")))))

;;; -------------------------------------------------------
;;; language / dictionary ordering for suggestions
;;; -------------------------------------------------------

(ert-deftest ps/typo--order-langs-moves-detected-first ()
  (should (equal (ps/typo--order-langs '("en_US" "ru_RU" "es_ES") 'es)
                 '("es_ES" "en_US" "ru_RU")))
  (should (equal (ps/typo--order-langs '("en_US" "ru_RU" "es_ES") "ru")
                 '("ru_RU" "en_US" "es_ES"))))

(ert-deftest ps/typo--order-langs-noop-on-nil-or-miss ()
  (let ((langs '("en_US" "ru_RU" "es_ES")))
    (should (equal (ps/typo--order-langs langs nil) langs))
    (should (equal (ps/typo--order-langs langs 'fr) langs))
    ;; Already first -> unchanged order.
    (should (equal (ps/typo--order-langs langs 'en) langs))))

(ert-deftest ps/typo--order-dicts-tracks-langs ()
  "Dicts are reordered in parallel with their languages."
  (should (equal (ps/typo--order-dicts '("en_US" "ru_RU" "es_ES")
                                       '(:en :ru :es) 'es)
                 '(:es :en :ru)))
  (should (equal (ps/typo--order-dicts '("en_US" "ru_RU" "es_ES")
                                       '(:en :ru :es) nil)
                 '(:en :ru :es))))

;;; -------------------------------------------------------
;;; alist merge used to extend Jinx exclusions
;;; -------------------------------------------------------

(ert-deftest ps/typo--merge-adds-new-values ()
  (let ((alist '((org-mode org-code) (t "URL"))))
    (should (equal (ps/typo--merge-into-alist alist 'org-mode '(org-verbatim))
                   '((org-mode org-code org-verbatim) (t "URL"))))))

(ert-deftest ps/typo--merge-dedupes ()
  "Existing values are not duplicated; order is preserved."
  (let ((alist '((t "a" "b"))))
    (should (equal (ps/typo--merge-into-alist alist t '("b" "c"))
                   '((t "a" "b" "c"))))))

(ert-deftest ps/typo--merge-creates-missing-key ()
  (should (equal (ps/typo--merge-into-alist '((t "x")) 'org-mode '(org-code))
                 '((org-mode org-code) (t "x")))))

(ert-deftest ps/typo--merge-noop-when-nothing-new ()
  (let ((alist '((t "a" "b"))))
    (should (eq (ps/typo--merge-into-alist alist t '("a")) alist))))

;;; -------------------------------------------------------
;;; short-word regexp + underline color
;;; -------------------------------------------------------

(ert-deftest ps/typo--short-word-regexp-matches-short-only ()
  (let ((ps/typo-min-length 3))
    (let ((re (ps/typo--short-word-regexp)))
      (should (string-match-p re "ab"))
      (should (string-match-p re "x"))
      (should-not (string-match-p re "abc"))
      (should-not (string-match-p re "color")))))

(ert-deftest ps/typo--short-word-regexp-nil-when-disabled ()
  (let ((ps/typo-min-length 1))
    (should (null (ps/typo--short-word-regexp)))))

(ert-deftest ps/typo--underline-color-explicit-and-auto ()
  (let ((ps/typo-underline-color "#999999"))
    (should (equal (ps/typo--underline-color) "#999999")))
  (let ((ps/typo-underline-color 'auto))
    ;; Always resolves to some concrete color string.
    (should (stringp (ps/typo--underline-color)))))

;;; test-ps-typo.el ends here
