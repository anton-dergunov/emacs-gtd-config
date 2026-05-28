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
