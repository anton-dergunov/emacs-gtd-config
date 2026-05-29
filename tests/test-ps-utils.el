;;; test-ps-utils.el --- ERT tests for ps-utils -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path "lisp")
(require 'ps-utils)

(ert-deftest ps/org--shorten-short-string ()
  "A string shorter than the limit is returned unchanged."
  (should (equal (ps/org--shorten "hello") "hello")))

(ert-deftest ps/org--shorten-long-string ()
  "A string longer than the limit is truncated with ellipsis."
  (let ((ps/org-link-title-max-length 5))
    (should (equal (ps/org--shorten "abcdefgh") "abcde..."))))

(ert-deftest ps/org--shorten-exactly-at-limit ()
  "A string at exactly the limit length is not truncated."
  (let ((ps/org-link-title-max-length 5))
    (should (equal (ps/org--shorten "abcde") "abcde"))))

(ert-deftest ps/org--shorten-nil ()
  "nil input is returned as nil."
  (should (null (ps/org--shorten nil))))

(ert-deftest ps/org--clean-title-trims-whitespace ()
  "Leading and trailing whitespace is removed."
  (should (equal (ps/org--clean-title "  hello  ") "hello")))

(ert-deftest ps/org--clean-title-nil ()
  "nil input returns nil."
  (should (null (ps/org--clean-title nil))))

(ert-deftest ps/org--clean-title-empty ()
  "Empty string returns nil."
  (should (null (ps/org--clean-title ""))))

;;; -------------------------------------------------------
;;; clean-title: HTML entity decoding
;;; -------------------------------------------------------

(ert-deftest ps/org--clean-title-named-entity-amp ()
  "The named entity &amp; decodes to a literal ampersand."
  (should (equal (ps/org--clean-title "Tom &amp; Jerry") "Tom & Jerry")))

(ert-deftest ps/org--clean-title-named-entities-lt-gt ()
  "&lt; and &gt; decode to angle brackets."
  (should (equal (ps/org--clean-title "a &lt;b&gt; c") "a <b> c")))

(ert-deftest ps/org--clean-title-nbsp-fallback ()
  "&nbsp; decodes to a regular space (HTML fallback)."
  (should (equal (ps/org--clean-title "a&nbsp;b") "a b")))

(ert-deftest ps/org--clean-title-mdash-fallback ()
  "&mdash; decodes to an em dash (HTML fallback)."
  (should (equal (ps/org--clean-title "a&mdash;b") "a—b")))

(ert-deftest ps/org--clean-title-hex-entity ()
  "Hexadecimal numeric entity decodes to an em dash."
  (should (equal (ps/org--clean-title "a&#x2014;b") "a—b")))

(ert-deftest ps/org--clean-title-decimal-entity ()
  "Decimal numeric entity decodes to an em dash."
  (should (equal (ps/org--clean-title "a&#8212;b") "a—b")))

(ert-deftest ps/org--clean-title-multiple-entities ()
  "Multiple non-adjacent entities in one string are all decoded."
  (should (equal (ps/org--clean-title "&lt;a&gt; and &lt;b&gt;") "<a> and <b>")))

(ert-deftest ps/org--clean-title-amp-adjacent-to-entity ()
  "A decoded &amp; is not re-consumed by a following entity (regression).
Previously \"&amp; &lt;\" mis-parsed the fresh & together with the next entity."
  (should (equal (ps/org--clean-title "&lt;a&gt; &amp; &lt;b&gt;") "<a> & <b>")))

(ert-deftest ps/org--clean-title-numeric-amp-not-reconsumed ()
  "A numeric ampersand entity (&#38;) is also not re-consumed."
  (should (equal (ps/org--clean-title "&#38; &lt;x&gt;") "& <x>")))

(ert-deftest ps/org--clean-title-bare-ampersand-preserved ()
  "A bare ampersand with no trailing semicolon is left intact."
  (should (equal (ps/org--clean-title "Rock & Roll") "Rock & Roll")))

;;; -------------------------------------------------------
;;; clean-title: non-printable stripping (whitelist sanitizer)
;;; Control-char inputs are built with elisp char literals (?\xNN,
;;; ?\uXXXX) so the source file stays plain ASCII.
;;; -------------------------------------------------------

(ert-deftest ps/org--clean-title-strips-c0-control ()
  "A C0 control character (outside the whitelist) is removed."
  (should (equal (ps/org--clean-title (concat "ab" (string ?\x01) "cd")) "abcd")))

(ert-deftest ps/org--clean-title-strips-null ()
  "A NUL byte is removed."
  (should (equal (ps/org--clean-title (concat "ab" (string ?\x00) "cd")) "abcd")))

(ert-deftest ps/org--clean-title-strips-c1-control ()
  "A C1 control character (in the \\u007F-\\u009F gap) is removed."
  (should (equal (ps/org--clean-title (concat "x" (string ?\x85) "y")) "xy")))

(ert-deftest ps/org--clean-title-keeps-tab ()
  "A TAB in the middle is whitelisted and preserved."
  (should (equal (ps/org--clean-title (concat "a" (string ?\t) "b"))
                 (concat "a" (string ?\t) "b"))))

(ert-deftest ps/org--clean-title-keeps-accented-char ()
  "A Latin-1 accented character (within \\u00A0-\\uD7FF) is preserved."
  (should (equal (ps/org--clean-title (concat "caf" (string ?\u00E9)))
                 (concat "caf" (string ?\u00E9)))))

(ert-deftest ps/org--clean-title-keeps-bmp-char ()
  "A private-use BMP character (within \\uE000-\\uFFFD) is preserved."
  (should (equal (ps/org--clean-title (concat "a" (string ?\uE000) "b"))
                 (concat "a" (string ?\uE000) "b"))))

;;; test-ps-utils.el ends here
