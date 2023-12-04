;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


;; Bootstrap config


(setq custom-file (locate-user-emacs-file "custom.el"))
(setq nixos-treesit-path treesit-extra-load-path) ;; remove after next update

;;; --- Most of the core and necessary defaults.
;;; --- better not to comment it!
(require 'id-startup)
(require 'id-core)
(require 'id-files)

;; ;;; --- enhancements that are crucial these days
(require 'id-completion)
(require 'id-enhance)
(require 'id-fonts)

;; ;;; --- to give some color and please my eyes
(require 'id-theme)
(require 'id-aesthetics)
(require 'id-icons)

;;; --- helpful to have
(require 'id-help)
(require 'id-embark)

;;; --- to play with code/programming
(unless d/on-droid
  (require 'id-code)
  (require 'id-git)
  (require 'id-python)
  (require 'id-rstats)
  (require 'id-nix))
(require 'id-eshell)

;;; --- file manager on battery
(require 'id-dired)
(unless d/on-droid
  (require 'id-dirvish))

;;; --- to read, and love reading
(require 'id-web)
(require 'id-reading)
(require 'id-rss)

;;; --- organize life
(require 'id-notes)
(require 'id-org)
;; (require 'id-markdown)

;;; --- modal editing
;; (require 'id-meow)

;;; --- fun personal environment
(require 'id-apps)
;; (require 'id-ement)

(when d/on-droid
  (require 'id-android))

(unless d/on-droid
  (require 'id-present))
;; (require 'id-vterm)




;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
