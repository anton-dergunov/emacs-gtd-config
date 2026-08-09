;;; ps-file-notify.el --- Guard against a removed file-notify watch -*- lexical-binding: t; -*-

;;; Commentary:

;; A workaround for an upstream bug in filenotify.el, hit whenever an Org file
;; is replaced on disk by something other than Emacs -- here, Dropbox landing
;; an edit made in a mobile Org app.
;;
;; One notification can carry several actions.  On macOS the kqueue backend
;; watches the file itself, and a single kevent's fflags routinely combine
;; NOTE_DELETE/NOTE_RENAME with NOTE_WRITE, which `file-notify--handle-event'
;; turns into an action list such as (deleted changed).  That function looks
;; the watch up *once* and then loops over the actions.  Handling `deleted'
;; (or `renamed') removes the watch, and `file-notify--rm-descriptor' clears
;; the watch's callback -- but the loop still holds the same struct, so the
;; next action reaches `file-notify--call-handler', which unlike every other
;; caller does not check the callback before calling it:
;;
;;     file-notify--call-handler: Symbol's function definition is void: nil
;;
;; The revert itself has already happened by then, so the only damage is the
;; error, but it aborts the rest of the event and lands in *Messages*.  This
;; module makes `file-notify--call-handler' skip a watch whose callback is
;; gone, which is what the pending-rename paths in the same file already do.
;;
;; The guard stays correct once filenotify.el checks the callback itself (it
;; just makes the same decision one frame earlier), so it is safe to leave in
;; place; test-ps-file-notify.el is what will show the upstream fix landing.

;;; Code:

(require 'filenotify)

(defun ps/file-notify--handler-guard (watch &rest _)
  "Return non-nil when WATCH still has a callback to deliver an event to.
Installed as `:before-while' advice, so a nil callback -- a watch removed
part-way through a multi-action event -- skips the handler call instead of
funcalling nil."
  (and (file-notify--watch-p watch)
       (file-notify--watch-callback watch)))

(defun ps/file-notify-setup ()
  "Stop a removed file-notify watch from erroring out on a queued action."
  (when (fboundp 'file-notify--call-handler)
    (advice-add 'file-notify--call-handler :before-while
                #'ps/file-notify--handler-guard)))

(provide 'ps-file-notify)
;;; ps-file-notify.el ends here
