;;; early-init.el --- Runs before the first frame is created -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs loads this file before it creates the initial frame, and before
;; `init.el'.  Only settings that must beat frame creation belong here;
;; everything else stays in `config.org'.

;;; Code:

;; The tool bar has to be off *before* the initial frame is created.  In an NS
;; build `tool-bar-mode' is on by default, and on macOS 26 the tool bar is drawn
;; as a floating rounded panel over the frame rather than a band under the title
;; bar.  Switching it off later -- config.org's `tool-bar-mode' call, which
;; cannot run until init.el has tangled and loaded the whole config -- leaves
;; that panel painted until something forces a redraw, so on a machine whose
;; startup never gets a rescale or resize event it stays on screen over the
;; buffer text.  Setting the frame parameter here means the toolbar is never
;; built at all, so there is nothing left to tear down.
(push '(tool-bar-lines . 0) default-frame-alist)

;; Keep the mode variable agreeing with the frames, so the Options menu
;; checkbox and `M-x tool-bar-mode' still toggle in the right direction.
(setq tool-bar-mode nil)

(provide 'early-init)
;;; early-init.el ends here
