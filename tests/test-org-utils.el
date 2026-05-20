(require 'ert)
(require 'org)

(add-to-list 'load-path "lisp")

(require 'org-utils)

(ert-deftest my/org--shorten-short-string ()
  (should
   (equal
    (my/org--shorten "hello")
    "hello")))

(ert-deftest my/org--shorten-long-string ()
  (let ((my/org-link-title-max-length 5))
    (should
     (equal
      (my/org--shorten "abcdefgh")
      "abcde..."))))
