;;; early-init.el ---   -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(setq package-enable-at-startup nil) ; don't auto-initialize!
(setq package-archives nil)

(setq
 load-prefer-newer nil
 default-input-method nil
 initial-major-mode 'org-mode
 inhibit-default-init t
 inhibit-startup-screen t 	
 inhibit-startup-buffer-menu t)

;; Maximize the Emacs frame on startup
(push '(fullscreen . maximized) default-frame-alist)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.

(setq default-frame-alist
      '((menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (horizontal-scroll-bars)
        (vertical-scroll-bars)))

;; Suppress warnings and errors during asynchronous native compilation
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-jit-compilation nil)

(load-theme 'modus-vivendi t)
;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

(provide 'early-init)
;;; early-init.el ends here
