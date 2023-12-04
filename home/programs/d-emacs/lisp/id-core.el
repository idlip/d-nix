;;; id-core.el --- sane default tweaks -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package emacs
  :ensure nil
  :init
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (fset 'x-popup-menu #'ignore)
  :custom
  (scroll-step 1)
  (inhibit-x-resources t)
  (inhibit-startup-screen t "Don't show splash screen")
  (inhibit-startup-buffer-menu t)
  (initial-scratch-message
   "--- Scratch Buffer ---\n\n\n")
  (use-short-answers t)
  (use-dialog-box t "Disable dialog boxes")
  (x-gtk-use-system-tooltips nil)
  (use-file-dialog nil)
  (enable-recursive-minibuffers t "Allow minibuffer commands in the minibuffer")
  (indent-tabs-mode nil "Spaces!")
  (tab-always-indent 'complete)
  (tab-width 4)
  (debug-on-quit nil)
  (initial-major-mode 'fundamental-mode)

  (sentence-end-double-space nil)
  (sentence-end "[.?!] ")

  :config
  ;; Terminal emacs doesn't have it
  (when (fboundp 'set-fontset-font)
    ;; a workaround for old charsets
    (set-fontset-font t 'unicode "Noto Color Emoji" nil 'append)
    )

  (with-current-buffer "*scratch*"
	(emacs-lock-mode 'kill))
  (prefer-coding-system 'utf-8)
  ;; Uppercase is same as lowercase
  (define-coding-system-alias 'UTF-8 'utf-8)
  (modify-all-frames-parameters '((alpha-background . 90))))

(use-package frame
  :ensure nil
  :bind
  ("C-z" . nil)
  ("C-x C-z" . nil)
  :custom
  (frame-resize-pixelwise t)
  (frame-inhibit-implied-resize t))


(use-package window
  :ensure nil
  :bind ("M-o" . other-window)
  ("C-<tab>" . other-window)
  ("C-x C-k" . d/kill-buffer)
  ("C-x n n" . d/narrow-or-widen-dwim)

  :custom
  (recenter-positions '(top middle bottom))

  :functions
  (doc-view-clear-cache
   org-narrow-to-subtree)

  :config
  ;; balance windows when split (https://zck.org/balance-emacs-windows)
  (seq-doseq (fn (list #'split-window #'delete-window))
    (advice-add fn :after #'(lambda (&rest args) (balance-windows))))

  (defun window-focus-mode ()
    "Make the window focused, it can toggle in and out"
    (interactive)
    (if (= 1 (length (window-list)))
	      (jump-to-register '_)
      (progn
	      (set-register '_ (list (current-window-configuration)))
	      (delete-other-windows))))

  (defun d/narrow-or-widen-dwim ()
    "If the buffer is narrowed, it widens. Otherwise,
it narrows to region, or Org subtree."
    (interactive)
    (cond ((buffer-narrowed-p) (widen))
          ((region-active-p) (narrow-to-region (region-beginning) (region-end)))
          ((eq major-mode 'org-mode) (org-narrow-to-subtree))
          (t (error "Please select a region to narrow to"))))

  (defun d/kill-buffer ()
    "Clear the image cache (to release memory) after killing a pdf buffer."
    (interactive)
    (if (one-window-p) (kill-this-buffer)
      (kill-buffer-and-window))
    (when (derived-mode-p 'doc-view-mode) (progn (clear-image-cache) (doc-view-clear-cache)))
    (when (derived-mode-p 'pdf-view-mode) (progn ((clear-image-cache) (pdf-cache-clear-data))))))


(use-package delsel
  :ensure nil
  :bind
  (:map mode-specific-map
        ("C-g" . minibuffer-keyboard-quit))
  :config
  (delete-selection-mode))

(use-package minibuf
  :ensure nil
  :custom
  (history-delete-duplicates t))

(use-package re-builder
  :ensure nil
  :custom
  (reb-re-syntax 'string))

(use-package saveplace
  :ensure nil
  :hook
  (after-init . save-place-mode))

(use-package simple
  :ensure nil
  :defer 0.1
  :bind (("<f7>" . scratch-buffer)
         ("<escape>" . keyboard-quit))
  :custom
  (kill-ring-max 30000)
  (column-number-mode 1)
  (kill-do-not-save-duplicates t)

  :config
  (global-visual-line-mode 1)
  ;; credit: yorickvP on Github
  (setq wl-copy-process nil)
  (defun wl-copy (text)
    (setq wl-copy-process (make-process :name "wl-copy"
                                        :buffer nil
                                        :command '("wl-copy" "-f" "-n")
                                        :connection-type 'pipe))
    (process-send-string wl-copy-process text)
    (process-send-eof wl-copy-process))
  (defun wl-paste ()
    (if (and wl-copy-process (process-live-p wl-copy-process))
        nil ; should return nil if we're the current paste owner
      (shell-command-to-string "wl-paste -n | tr -d \r")))
  (unless d/on-droid
    (setq interprogram-cut-function 'wl-copy)
    (setq interprogram-paste-function 'wl-paste)))

(use-package s
  :ensure nil
  :functions (s-join)
  :bind
  ("M-^" . d/join-lines)
  :config
  (defun d/join-lines (specify-separator)
    "Join lines in the active region by a separator, by default a comma.
  Specify the separator by typing C-u before executing this command."
    (interactive "P")
    (require 's)
    (unless (region-active-p)
      (message "select a region of lines first."))
    (let* ((separator (if (not specify-separator)
                          ","
                        (read-string "Separator: ")))
           (text (buffer-substring-no-properties
                  (region-beginning)
                  (region-end)))
           (lines (split-string text "\n"))
           (result (s-join separator lines)))
      (delete-region (region-beginning) (region-end))
      (insert result))))

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode)
  :custom
  (display-line-numbers-type 'relative))

(use-package misc
  :ensure nil
  :bind ("M-z" . zap-up-to-char))

(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-delay 0.1)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-style 'mixed)
  (show-paren-context-when-offscreen t))

(use-package ibuffer
  :ensure nil
  :bind
  ([remap list-buffers] . ibuffer))

(use-package replace
  :ensure nil
  :bind
  ("M-%" . query-replace-regexp))

(use-package xref
  :ensure nil
  :custom
  (xref-search-program 'ripgrep))

(use-package undo-fu-session
  :ensure nil
  :functions (undo-fu-session-global-mode)
  :defines (undo-fu-session-incompatible-files)

  :init (undo-fu-session-global-mode)
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package vundo
  :ensure nil
  :defer t
  :bind (("C-x u" . vundo)
	     ("C-z" . undo-only)
	     ("C-S-z" . undo-redo)
	     ("C-M-r" . undo-redo)))

(provide 'id-core)
;;; id-core.el ends here
