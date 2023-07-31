;;; id-startup.el --- start the voyage -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Initialize package sources
(require 'package)

(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)

(unless (bound-and-true-p package--initialized)
  (customize-set-variable 'package-enable-at-startup nil)
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-and-compile
  (customize-set-variable 'use-package-verbose (not (bound-and-true-p byte-compile-current-file))))

(put 'use-package 'lisp-indent-function 1)

(use-package use-package
  :ensure nil
  :custom
  (use-package-verbose t)
  (use-package-always-ensure nil)
  (use-package-always-defer t)
  (use-package-expand-minimally t)
  (use-package-enable-imenu-support t))

(use-package comp
  :demand t
  :custom
  (native-comp-async-report-warnings-errors nil)
  (native-comp-jit-compilation nil)
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name "var/eln-cache/" user-emacs-directory)))))

(use-package gcmh
  :ensure t
  :demand t
  :config
  (gcmh-mode 1))

(provide 'id-startup.el)
;;; id-startup.el ends here
