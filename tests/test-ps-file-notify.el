;;; test-ps-file-notify.el --- ERT tests for ps-file-notify -*- lexical-binding: t; -*-

(require 'ert)
(require 'filenotify)
(add-to-list 'load-path "lisp")
(require 'ps-file-notify)

;;; Helpers

(defmacro ps/file-notify-test--with-watch (spec &rest body)
  "Watch a fresh temp file, binding SPEC as (DESC-VAR FILE-VAR EVENTS-VAR).
EVENTS-VAR accumulates the actions the watch callback is handed, oldest
first.  The watch and the file are removed afterwards."
  (declare (indent 1))
  (let ((desc (nth 0 spec)) (file (nth 1 spec)) (events (nth 2 spec)))
    `(let* ((dir (make-temp-file "ps-file-notify" t))
            (,file (expand-file-name "watched.org" dir))
            (,events nil)
            ,desc)
       (unwind-protect
           (progn
             (with-temp-file ,file (insert "* TODO Something\n"))
             (setq ,desc (file-notify-add-watch
                          ,file '(change)
                          (lambda (event)
                            (setq ,events (append ,events (list (nth 1 event)))))))
             ,@body)
         (when ,desc (ignore-errors (file-notify-rm-watch ,desc)))
         (delete-directory dir t)))))

(defmacro ps/file-notify-test--with-guard (installed &rest body)
  "Run BODY with the handler guard installed when INSTALLED, absent otherwise.
Whatever the suite started with is restored afterwards."
  (declare (indent 1))
  `(let ((had (advice-member-p #'ps/file-notify--handler-guard
                               'file-notify--call-handler)))
     (unwind-protect
         (progn
           (if ,installed
               (ps/file-notify-setup)
             (advice-remove 'file-notify--call-handler
                            #'ps/file-notify--handler-guard))
           ,@body)
       (if had
           (ps/file-notify-setup)
         (advice-remove 'file-notify--call-handler
                        #'ps/file-notify--handler-guard)))))

(defun ps/file-notify-test--handle (desc actions file)
  "Feed ACTIONS for FILE to the watch DESC, returning any error signalled."
  (condition-case err
      (progn (file-notify--handle-event desc actions file nil) nil)
    (error err)))

;;; Tests

(ert-deftest ps/file-notify-multi-action-event-does-not-error ()
  "An action queued behind the one that removed the watch is dropped.
Without the guard, `file-notify--handle-event' reuses the watch struct
whose callback `file-notify--rm-descriptor' has just cleared, so the second
action's handler call signals (void-function nil).  A Dropbox sync lands
exactly this shape of event -- one kevent whose fflags combine
NOTE_DELETE/NOTE_RENAME with NOTE_WRITE."
  (ps/file-notify-test--with-guard t
    (ps/file-notify-test--with-watch (desc file events)
      (should-not (ps/file-notify-test--handle desc '(deleted changed) file))
      ;; The watch still saw the delete and its own `stopped' notice; only the
      ;; action that arrived after the watch was gone is dropped.
      (should (equal events '(deleted stopped))))))

(ert-deftest ps/file-notify-upstream-bug-is-still-present ()
  "Document the upstream defect the guard exists for.
A failure here is good news: filenotify.el checks the callback itself and
`ps-file-notify.el' can go away."
  (ps/file-notify-test--with-guard nil
    (ps/file-notify-test--with-watch (desc file _events)
      (should (equal (ps/file-notify-test--handle desc '(deleted changed) file)
                     '(void-function nil))))))

(ert-deftest ps/file-notify-live-watch-still-receives-events ()
  "The guard does not swallow events for a watch that is still live."
  (ps/file-notify-test--with-guard t
    (ps/file-notify-test--with-watch (desc file events)
      (should-not (ps/file-notify-test--handle desc '(changed) file))
      (should-not (ps/file-notify-test--handle desc '(attribute-changed) file))
      (should (equal events '(changed attribute-changed))))))

(ert-deftest ps/file-notify-setup-is-idempotent ()
  "Re-running setup leaves exactly one copy of the guard installed."
  (ps/file-notify-test--with-guard nil
    (ps/file-notify-setup)
    (ps/file-notify-setup)
    (should (advice-member-p #'ps/file-notify--handler-guard
                             'file-notify--call-handler))
    (advice-remove 'file-notify--call-handler #'ps/file-notify--handler-guard)
    (should-not (advice-member-p #'ps/file-notify--handler-guard
                                 'file-notify--call-handler))))

(provide 'test-ps-file-notify)
;;; test-ps-file-notify.el ends here
