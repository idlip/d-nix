;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defconst d/on-droid (eq system-type 'android))


;; Adjust garbage collection thresholds during startup, and thereafter

;; (let ((normal-gc-cons-threshold (* 20 1024 1024))
;;       (init-gc-cons-threshold (* 128 1024 1024)))
;;   (setq gc-cons-threshold init-gc-cons-threshold)
;;   (add-hook 'emacs-startup-hook
;;             (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))


;; Bootstrap config


(setq custom-file (locate-user-emacs-file "custom.el"))

;;; --- Most of the core and necessary defaults.
;;; --- better not to comment it!
(require 'id-startup.el)
(require 'id-core.el)
(require 'id-files.el)

;; ;;; --- enhancements that are crucial these days
(require 'id-completion.el)
(require 'id-enhance.el)
(require 'id-fonts.el)

;; ;;; --- to give some color and please my eyes
(require 'id-theme.el)
(require 'id-aesthetics.el)
(require 'id-icons.el)

;;; --- helpful to have
(require 'id-help.el)
;; (require 'id-embark.el)

;;; --- to play with code/programming
;; (require 'id-code.el)
(unless d/on-droid
  (require 'id-git.el))
;; (require 'id-python.el)
;; (require 'id-rstats.el)
;; (require 'id-nix.el)
(unless d/on-droid
  (require 'id-eshell.el))

;;; --- file manager on battery
(require 'id-dired.el)
(unless d/on-droid
  (require 'id-dirvish.el))

;;; --- to read, and love reading
(require 'id-web.el)
;; (require 'id-reading.el)
(require 'id-rss.el)

;;; --- organize life
;; (require 'id-notes.el)
(require 'id-org.el)
;; (require 'id-markdown.el)

;;; --- modal editing
;; (require 'id-meow.el)

;;; --- fun personal environment
(unless d/on-droid
  (require 'id-apps.el))
;; (require 'id-ement.el)

(when d/on-droid
  (require 'id-android.el))

;; (require 'id-present.el)
;; (require 'id-vterm.el)

;; for some reason, icon size looks proper when I set this at last
(set-language-environment "UTF-8")




;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
