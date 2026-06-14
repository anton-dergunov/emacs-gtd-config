;;; test-ps-agenda-pills.el --- ERT tests for ps-agenda-pills -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path "lisp")
(require 'ps-agenda-pills)

;;; -------------------------------------------------------
;;; columns
;;; -------------------------------------------------------

(ert-deftest ps/agenda-pills--columns-add-padding ()
  "A pill is the label width plus padding on both sides."
  (let ((ps/agenda-pills-pad 1))
    (should (= (ps/agenda-pills-columns "TODO") 6))
    (should (= (ps/agenda-pills-columns "IN-PROGRESS") 13))
    (should (= (ps/agenda-pills-columns "#A") 4))))

(ert-deftest ps/agenda-pills--columns-respect-pad ()
  "Padding is configurable."
  (let ((ps/agenda-pills-pad 2))
    (should (= (ps/agenda-pills-columns "TODO") 8))))

;;; -------------------------------------------------------
;;; escape
;;; -------------------------------------------------------

(ert-deftest ps/agenda-pills--escape-xml ()
  "XML metacharacters are escaped."
  (should (equal (ps/agenda-pills--escape "a&b<c>d\"") "a&amp;b&lt;c&gt;d&quot;")))

;;; -------------------------------------------------------
;;; svg
;;; -------------------------------------------------------

(ert-deftest ps/agenda-pills--svg-contains-label-and-colors ()
  "The SVG carries the (escaped) label and the fill colors."
  (let ((svg (ps/agenda-pills--svg "TODO" 6 "#eee" "#333" nil 8 16)))
    (should (string-match-p "<svg" svg))
    (should (string-match-p ">TODO<" svg))
    (should (string-match-p "fill=\"#eee\"" svg))
    (should (string-match-p "fill=\"#333\"" svg))
    ;; rounded: a corner radius is present
    (should (string-match-p "rx=\"[0-9]+\"" svg))))

(ert-deftest ps/agenda-pills--svg-width-is-cols-times-char ()
  "The SVG width is the column count times the character width."
  (let ((svg (ps/agenda-pills--svg "TODO" 6 "#eee" "#333" nil 8 16)))
    (should (string-match-p "width=\"48\"" svg))     ; 6 cols * 8 px
    (should (string-match-p "height=\"16\"" svg))))

(ert-deftest ps/agenda-pills--svg-stroke-optional ()
  "A stroke color is emitted only when supplied."
  (should-not (string-match-p "stroke="
                              (ps/agenda-pills--svg "X" 3 "#eee" "#333" nil 8 16)))
  (should (string-match-p "stroke=\"#000\""
                          (ps/agenda-pills--svg "X" 3 "#eee" "#333" "#000" 8 16))))

;;; -------------------------------------------------------
;;; image / cache
;;; -------------------------------------------------------

(ert-deftest ps/agenda-pills--image-nil-for-empty-label ()
  "An empty or nil label yields no image."
  (should (null (ps/agenda-pills-image "")))
  (should (null (ps/agenda-pills-image nil))))

(ert-deftest ps/agenda-pills--image-is-svg-image ()
  "A non-empty label yields an SVG image."
  (ps/agenda-pills-reset-cache)
  (let ((img (ps/agenda-pills-image "TODO" :bg "#eee" :fg "#333")))
    (should (imagep img))
    (should (eq (image-property img :type) 'svg))))

(ert-deftest ps/agenda-pills--image-cached ()
  "The same spec returns the identical cached image object."
  (ps/agenda-pills-reset-cache)
  (let ((a (ps/agenda-pills-image "TODO" :bg "#eee" :fg "#333"))
        (b (ps/agenda-pills-image "TODO" :bg "#eee" :fg "#333")))
    (should (eq a b))))

;;; test-ps-agenda-pills.el ends here
