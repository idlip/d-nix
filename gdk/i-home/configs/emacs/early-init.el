;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;; Copyright (c) 2023  Dilip

;; Author: Dilip
;; URL: https://github.com/idlip/d-nix
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; See my dotfiles: https://github.com/idlip/d-nix (NixOS + Emacs)

;;; Code:

;; I use emacs on android too.
(defvar d/on-droid
  (memq system-type '(android))
  "Check if running on android phone.")

(setq
 frame-resize-pixelwise t
 frame-inhibit-implied-resize t
 use-dialog-box t ; only for mouse events, which I seldom use
 use-file-dialog t
 inhibit-splash-screen t
 inhibit-startup-screen t
 inhibit-x-resources t
 inhibit-startup-echo-area-message user-login-name ; read the docstring
 inhibit-startup-buffer-menu t
 package-enable-at-startup nil ; don't auto-initialize!
 initial-scratch-message nil ; empty the initial *scratch* buffer.
 inhibit-default-init t
 inhibit-startup-buffer-menu t  ; stop `list-buffers' from showing when opening multiple files
 frame-inhibit-implied-resize t ; do not resize the frame at this early stage
 use-short-answers t
 inhibit-compacting-font-caches t ; Inhibit frame resizing for performance
 select-active-regions 'only ; Emacs hangs when large selections contain mixed line endings
 ad-redefinition-action 'accept ; disable warnings from legacy advice system
 initial-major-mode 'org-mode) ;; gimme org-mode as Scratch buffer

(unless d/on-droid (menu-bar-mode -1))
(scroll-bar-mode -1)
(tool-bar-mode -1)

(custom-set-variables '(native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
'(native-compile-prune-cache t) ; Emacs 29
;; Suppress warnings and errors during asynchronous native compilation
'(native-comp-async-report-warnings-errors nil)
'(native-comp-jit-compilation nil))

;; Temporarily increase the garbage collection threshold.  These
;; changes help shave off about half a second of startup time.
(defvar d-emacs--gc-cons-threshold gc-cons-threshold)

(setq gc-cons-threshold most-positive-fixnum)

;; Same idea as above for the `file-name-handler-alist'.
(defvar d-emacs--file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold d-emacs--gc-cons-threshold
		  file-name-handler-alist d-emacs--file-name-handler-alist)))


(defun d-emacs-re-enable-frame-theme (_frame)
  "Re-enable active theme, if any, upon FRAME creation.
Add this to `after-make-frame-functions' so that new frames do
not retain the generic background set by the function
`d-emacs-avoid-initial-flash-of-light'."
  (when-let ((theme (car custom-enabled-themes)))
    (enable-theme theme)))

;; NOTE 2023-02-05: The reason the following works is because (i) the
;; `mode-line-format' is specified again and (ii) the
;; `d-emacs-theme-gsettings-dark-p' will load a dark theme.
(defun d-emacs-avoid-initial-flash-of-light ()
  "Avoid flash of light when starting Emacs, if needed.
New frames are instructed to call `d-emacs-re-enable-frame-theme'."
  ;; (when (d-emacs-theme-environment-dark-p)
    (setq mode-line-format nil)
    (set-face-attribute 'default nil :background "#000000" :foreground "#ffffff")
    (set-face-attribute 'mode-line nil :background "#000000" :foreground "#ffffff" :box 'unspecified)
    (add-hook 'after-make-frame-functions #'d-emacs-re-enable-frame-theme))

(d-emacs-avoid-initial-flash-of-light)

(add-hook 'after-init-hook (lambda () (set-frame-name "home")))

;;; early-init.el ends here
