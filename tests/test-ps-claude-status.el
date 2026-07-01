;;; test-ps-claude-status.el --- ERT tests for ps-claude-status.el -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp" (file-name-directory
                                                   (directory-file-name
                                                    (file-name-directory load-file-name)))))
(require 'ps-claude-status)

;;; ps/claude-status--display-model

(ert-deftest ps-claude-status--display-model/sonnet ()
  (should (equal "Sonnet 4.6" (ps/claude-status--display-model "claude-sonnet-4-6"))))

(ert-deftest ps-claude-status--display-model/opus ()
  (should (equal "Opus 4.8" (ps/claude-status--display-model "claude-opus-4-8"))))

(ert-deftest ps-claude-status--display-model/haiku-long-id ()
  ;; Date suffix in model ID should be ignored; major.minor captured correctly
  (should (equal "Haiku 4.5" (ps/claude-status--display-model "claude-haiku-4-5-20251001"))))

(ert-deftest ps-claude-status--display-model/fable ()
  (should (equal "Fable 5" (ps/claude-status--display-model "claude-fable-5"))))

(ert-deftest ps-claude-status--display-model/nil ()
  (should (equal "" (ps/claude-status--display-model nil))))

(ert-deftest ps-claude-status--display-model/unknown ()
  (should (equal "some-unknown-model" (ps/claude-status--display-model "some-unknown-model"))))

;;; ps/claude-status--thinking-p

(ert-deftest ps-claude-status--thinking-p/empty ()
  (should-not (ps/claude-status--thinking-p '())))

(ert-deftest ps-claude-status--thinking-p/text-only ()
  (should-not (ps/claude-status--thinking-p
               '(((type . "text") (text . "hello"))))))

(ert-deftest ps-claude-status--thinking-p/has-thinking ()
  (should (ps/claude-status--thinking-p
           '(((type . "thinking") (thinking . "..."))
             ((type . "text") (text . "result"))))))

(ert-deftest ps-claude-status--thinking-p/thinking-last ()
  (should (ps/claude-status--thinking-p
           '(((type . "text") (text . "preamble"))
             ((type . "thinking") (thinking . "..."))))))

;;; ps/claude-status--render

(defun ps-claude-status--test-render (slug model thinking)
  "Build a state plist and render it; strip text properties from result."
  (let ((state (list :file nil :pos 0
                     :slug slug :model model :thinking thinking)))
    (substring-no-properties (ps/claude-status--render state))))

(ert-deftest ps-claude-status--render/nil-state ()
  (should (equal " Claude Code"
                 (substring-no-properties (ps/claude-status--render nil)))))

(ert-deftest ps-claude-status--render/no-data-yet ()
  (should (equal " Claude Code"
                 (ps-claude-status--test-render nil nil nil))))

(ert-deftest ps-claude-status--render/slug-only ()
  (should (equal " Claude Code · my-session"
                 (ps-claude-status--test-render "my-session" nil nil))))

(ert-deftest ps-claude-status--render/model-only ()
  (should (equal " Claude Code · Sonnet 4.6"
                 (ps-claude-status--test-render nil "claude-sonnet-4-6" nil))))

(ert-deftest ps-claude-status--render/full-no-thinking ()
  (should (equal " Claude Code · my-session · Sonnet 4.6"
                 (ps-claude-status--test-render "my-session" "claude-sonnet-4-6" nil))))

(ert-deftest ps-claude-status--render/full-with-thinking ()
  (should (equal " Claude Code · my-session · Opus 4.8 [thinking]"
                 (ps-claude-status--test-render "my-session" "claude-opus-4-8" t))))

(ert-deftest ps-claude-status--render/slug-truncated ()
  "Slugs longer than 32 chars are truncated with an ellipsis."
  (let ((long-slug (make-string 40 ?x)))
    (let ((rendered (ps-claude-status--test-render long-slug nil nil)))
      (should (string-match-p "…" rendered))
      ;; The slug portion must be ≤ 32 display columns
      (should (<= (string-width (substring rendered
                                           (+ (length " Claude Code") (length " · "))))
                  32)))))

(provide 'test-ps-claude-status)
;;; test-ps-claude-status.el ends here
