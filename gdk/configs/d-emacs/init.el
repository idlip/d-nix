;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file loads the Tangled org file.

;;; Code:

;; (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


;; Bootstrap config

(setq custom-file (locate-user-emacs-file "custom.el"))
(setq nixos-treesit-path treesit-extra-load-path) ;; remove after next update

;; Initialize package sources
(require 'package)

(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/"))

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
  (use-package-verbose nil)
  (use-package-always-ensure nil)
  (use-package-always-defer t)
  (use-package-expand-minimally t)
  (use-package-enable-imenu-support t))

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
    (advice-add fn :after #'(lambda (&rest args) (balance-windows)))))

(defun window-focus-mode ()
  "Make the window focused, it can toggle in and out."
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
  (when (derived-mode-p 'pdf-view-mode) (progn ((clear-image-cache) (pdf-cache-clear-data)))))

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
  (global-visual-line-mode 1))
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
  (setq interprogram-paste-function 'wl-paste))

(use-package s
  :ensure nil
  :functions (s-join)
  :bind
  ("M-^" . d/join-lines))

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
    (insert result)))

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
  (show-paren-style 'parenthesis)
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
  :bind
  ("C-x u" . vundo)
  ("C-z" . undo-only)
  ("C-S-z" . undo-redo)
  ("C-M-r" . undo-redo))

(use-package files
  :ensure nil
  :hook
  (before-save . delete-trailing-whitespace)
  :bind ("<f5>" . d/refresh-buffer)
  :custom
  (require-final-newline t)
  (save-silently t)
  (confirm-kill-emacs 'yes-or-no-p)
  ;; backup settings
  (backup-by-copying t)
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  (backup-directory-alist
   `((".*" . ,(no-littering-expand-var-file-name "backup/"))))
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t)
  (create-lockfiles nil))

(defun d/refresh-buffer ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(use-package vc-backup
  ;; to have auto VC track of files without in git
  ;; C-x v =
  :demand t
  :custom
  (vc-make-backup-files t)
  (vc-follow-symlinks t))

(use-package savehist
  :ensure nil
  :defer 2
  :init
  (savehist-mode)
  :custom
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring)))

(use-package autorevert
  :ensure nil
  :defer 0.1
  :defines (d/on-droid)
  :unless d/on-droid)

(use-package recentf
  :ensure nil
  :demand t
  :custom
  (recentf-auto-cleanup 30)
  :config
  (recentf-mode)
  (run-with-idle-timer 30 t 'recentf-save-list))

(use-package no-littering
  :demand t
  :ensure t
  :functions (recentf-expand-file-name)
  :defines
  (no-littering-var-directory
   no-littering-etc-directory)
  :custom
  (no-littering-etc-directory (expand-file-name "config/" user-emacs-directory))
  (no-littering-var-directory (expand-file-name "data/" user-emacs-directory))
  :config
  ;; remove it after next rolling update
  ;; due to treesit var
  (setq treesit-extra-load-path nixos-treesit-path)
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory)))

(use-package tramp
  :ensure nil
  :defer t
  :config
  (put 'temporary-file-directory 'standard-value `(,temporary-file-directory))
  :custom
  (tramp-backup-directory-alist backup-directory-alist)
  (tramp-default-method "ssh")
  (tramp-default-proxies-alist nil)

  ;; Enable full-featured Dirvish over TRAMP on certain connections
  ;; https://www.gnu.org/software/tramp/#Improving-performance-of-asynchronous-remote-processes-1.
  (add-to-list 'tramp-connection-properties
	 (list (regexp-quote "/ssh:YOUR_HOSTNAME:")
		     "direct-async-process" t))

  (tramp-verbose 0)
  (tramp-chunksize 2000)
  (tramp-use-ssh-controlmaster-options nil))

(use-package dabbrev
  :ensure nil
  :commands (dabbrev-expand dabbrev-completion)
  :custom
  (dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (dabbrev-abbrev-skip-leading-regexp "\\$\\|\\*\\|/\\|=")
  (dabbrev-backward-only nil)
  (dabbrev-case-distinction nil)
  (dabbrev-case-fold-search t)
  (dabbrev-case-replace nil)
  (dabbrev-check-other-buffers t)
  (dabbrev-eliminate-newlines nil)
  (dabbrev-upcase-means-case-search t)
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
	     ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package hippie-exp
  :ensure nil
  :bind
  ("M-/" . hippie-expand))

(use-package vertico
  :defines
  (vertico-map)
  :functions
  (vertico-mode )

  :bind (:map vertico-map
	          ("<return>" . vertico-directory-enter)
	          ("DEL" . vertico-directory-delete-char)
	          ("M-DEL" . vertico-directory-delete-word)
	          ("M-j" . vertico-quick-exit)
	          ("C-v" . vertico-scroll-up)
	          ("M-v" . vertico-scroll-down)
	          ("M-q" . d/vertico-toggle)
	          ("M-RET" . minibuffer-force-complete-and-exit)
	          ("M-TAB" . minibuffer-complete)
              ("C->"     . embark-become)
              ("C-<tab>"   . embark-act-with-completing-read)
              ("C-o"     . embark-minimal-act)
              ("C-*"     . embark-act-all)
              ("M-*"      . embark-act-all)
              ("C-c C-o" . embark-export))

  :init
  (vertico-mode)

  :custom
  (vertico-scroll-margin 5)
  (vertico-count 5)
  (vertico-resize nil)
  (vertico-cycle t)
  (completion-in-region-function
   (lambda (&rest args)
     (apply (if vertico-mode
		        #'consult-completion-in-region
	          #'completion--in-region)
	        args))))

(use-package vertico-multiform
  :commands (vertico-multiform-mode)

  :init
  (vertico-multiform-mode)

  :custom
  (vertico-multiform-commands
   '(("\\`execute-extended-command" unobtrusive
      (vertico-flat-annotate . t)
      (marginalia-annotator-registry (command marginalia-annotate-command marginalia-annotate-binding builtin none)))
     (jinx-correct reverse)
     (tab-bookmark-open reverse)
     (dired-goto-file unobtrusive)
     (load-theme grid reverse)
     (org-refile reverse)
     (org-agenda-refile reverse)
     (org-capture-refile reverse)
     (consult-find reverse)
     (dired-goto-file flat)
     (consult-dir-maybe reverse)
     (consult-dir reverse)
     (consult-flymake reverse)
     (consult-history reverse)
     (consult-completion-in-region reverse)
     (consult-recoll buffer)
     (completion-at-point reverse)
     (embark-completing-read-prompter reverse)
     (embark-act-with-completing-read reverse)
     (embark-prefix-help-command reverse)
     (embark-bindings reverse)
     (consult-org-heading reverse)
     (consult-dff unobtrusive)
     (embark-find-definition reverse)
     (xref-find-definitions reverse)))

  (vertico-multiform-categories
   '((file grid reverse)
     (consult-grep buffer)
     (jinx grid (vertico-grid-annotate . 20))
     (kill-ring reverse)
     (buffer flat (vertico-cycle . t)))))

(use-package vertico-mouse
  :unless d/on-droid
  :init
  (vertico-mouse-mode))

(use-package consult
  :functions
  (consult-register-window
   eww-read-bookmarks
   consult--read
   consult-colors--web-list
   color-rgb-to-hex
   list-colors-duplicates)

  :defines
  (consult-buffer-sources
   eww-bookmarks
   add-unicodes
   shr-color-html-colors-alist
   d/on-droid)

  :bind (
         ("C-c d i" . d/insert-unicodes)
         ("C-c d c" . d/insert-colors)

         ;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ("C-c t t" . consult-theme)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x C-b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-g s" . consult-eglot-symbols)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-fd)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-ripgrep)
         ("M-s m" . consult-man)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s i" . consult-info)
         ("M-s l" . consult-line)
         ("C-s" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  :hook (completion-list-mode . consult-preview-at-point-mode)

  :custom
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key "<")
  (consult-ripgrep-args "rg --null --line-buffered --no-ignore --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip")

  (consult-customize
   consult-theme :preview-key '(:debounce 1.5 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  :config
  (advice-add #'register-preview :override #'consult-register-window)

  (with-eval-after-load 'eww
    (defvar consult--source-eww
    (list
     :name     "Eww"
     :narrow   ?e
     :action   (lambda (bm)
                 (eww-browse-url (get-text-property 0 'url bm)))
     :items    (lambda ()
                 (eww-read-bookmarks)
                 (mapcar (lambda (bm)
                           (propertize
                            (format "%s (%s)"
                                    (plist-get bm :url)
                                    (plist-get bm :title))
                            'url (plist-get bm :url)))
                         eww-bookmarks)))))
  (add-to-list 'consult-buffer-sources 'consult--source-eww 'append))

(defun consult-colors--web-list nil
  "Return list of CSS colors for `d/colors-web'."
  (require 'shr-color)
  (sort (mapcar #'downcase (mapcar #'car shr-color-html-colors-alist)) #'string-lessp))

(defun d/colors-web (color)
  "Show a list of all CSS colors.\

  You can insert the name (default), or insert or kill the hexadecimal,
or RGB value of the selected color."
  (interactive
   (list (consult--read (consult-colors--web-list)
                        :prompt "Color: "
                        :require-match t
                        :category 'color
                        :history '(:input consult-colors-history)
                        )))
  (insert
   (when-let* ((rgb (color-name-to-rgb color))
               ;; Sets 2 digits per component.
               (hex (apply #'color-rgb-to-hex (append rgb '(2)))))
     hex)))

(defun d/insert-colors (color)
  "Show a list of all supported colors for a particular frame.\

You can insert the name (default), or insert or kill the hexadecimal
 or RGB value of the selected color."
  (interactive
   (list (consult--read (list-colors-duplicates (defined-colors))
                        :prompt "Emacs color: "
                        :require-match t
                        :category 'color
                        :history '(:input consult-colors-history)
                        )))
  (insert
   (when-let* ((rgb (color-name-to-rgb color))
               ;; Sets 2 digits per component.
               (hex (apply #'color-rgb-to-hex (append rgb '(2)))))
     hex)))

(defun d/insert-unicodes (add-unicodes)
  "Insert unicode character (emoji/icons) from given files."
  (interactive (list add-unicodes))
  (insert
   (let* ((content
           (mapcar #'(lambda (file) (with-temp-buffer (insert-file-contents file) (split-string (buffer-string) "\n" t))) add-unicodes))
          (options (apply #'append content))
          (selected-item (completing-read "Choose Icon 󰨈: " options))
          (fields (split-string selected-item)))
     (car fields))))

(setq add-unicodes (unless d/on-droid (directory-files "~/d-git/d-bin/treasure/unicodes/" t "i")))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles orderless basic partial-completion)))))

(use-package marginalia
  :functions
  (marginalia-mode)

  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package corfu
  :defer 1

  :defines
  (corfu-map)

  :functions
  (global-corfu-mode
   corfu-mode)

  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-cycle t)
  ;; (corfu-preview-current t)    ;; Disable current candidate preview
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-quit-no-match t)
  (corfu-auto-prefix 3)
  (corfu-auto-delay 0.4)
  (corfu-quit-at-boundary 'separator)
  (corfu-popupinfo-resize t)
  (corfu-popupinfo-hide nil)
  (corfu-preview-current 'insert)
  (corfu-popupinfo-delay 1.0)
  (corfu-history 1)
  (corfu-scroll-margin 0)

  :bind (:map corfu-map
	          ("M-SPC" . corfu-insert-separator)
	          ("TAB" . corfu-insert)
	          ("<escape>" . corfu-quit)
	          ("C-j" . corfu-next)
	          ("C-k" . corfu-previous)
	          ("M-j" . corfu-quick-insert))
  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  :init
  (global-corfu-mode))

(use-package corfu-history
  :disabled
  :init
  (corfu-history-mode))

(use-package corfu-popupinfo
  :unless d/on-droid
  :after corfu
  :init
  (corfu-popupinfo-mode))

(use-package corfu-echo
  :unless d/on-droid
  :after corfu
  :init
  (corfu-echo-mode))
(eldoc-add-command #'corfu-insert)

(unless (display-graphic-p)
  (corfu-terminal-mode +1))

(defun corfu-enable-always-in-minibuffer ()
  "Enable corfu in minibuffer, if vertico is not active."
  (unless (or (bound-and-true-p vertico--input)
		      (eq (current-local-map) read-passwd-map))
    (setq-local corfu-auto t
		        corfu-popupinfo-delay nil
		        corfu-auto-delay 0
		        corfu-auto-prefix 0
		        completion-styles '(orderless basic))
    (corfu-mode 1)))
;; (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

;; Add extensions
(use-package cape
  :after corfu

  :functions
  (cape-wrap-silent
   cape-wrap-purify)
  :defines
  (cape-dict-file)

  :bind (("C-c p p" . completion-at-point)
	     ("C-c p t" . complete-tag)
	     ("C-c p d" . cape-dabbrev)
	     ("C-c p h" . cape-history)
	     ("C-c p f" . cape-file)
	     ("C-c p k" . cape-keyword)
	     ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
	     ("C-c p a" . cape-abbrev)
	     ("C-c p i" . cape-ispell)
	     ("C-c p l" . cape-line)
	     ("C-c p w" . cape-dict)
	     ("C-c p \\" . cape-tex)
	     ("C-c p _" . cape-tex)
	     ("C-c p ^" . cape-tex)
	     ("C-c p &" . cape-sgml)
	     ("C-c p r" . cape-rfc1345))

  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-to-list 'completion-at-point-functions #'cape-tex)
  ;; (add-to-list 'completion-at-point-functions #'cape-sgml)
  ;; (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  :config

  ;; Silence the pcomplete capf, no errors or messages!
  ;; Important for corfu
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package tempel
  :after corfu
  :hook
  (prog-mode . tempel-abbrev-mode)

  ;; Require trigger prefix before template name when completing.
  :custom
  (tempel-trigger-prefix "<")
  (tempel-path "~/.config/emacs/templates/*")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
	     ("M-*" . tempel-insert)))

(use-package tempel-collection
  :after tempel)

(use-package mwheel
  :ensure nil
  :bind
  ([mouse-9] . [prior]) ;; binds mouse fwd button to page up
  ([mouse-8] . [next]) ;; mouse bwd button to page down
  :custom
  ;; (mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
  (mouse-wheel-progressive-speed nil)
  (scroll-margin 4)
  (scroll-conservatively 101))

(use-package pixel-scroll
  :ensure nil
  :init (pixel-scroll-precision-mode)
  :commands
  (pixel-scroll-precision-scroll-down pixel-scroll-precision-scroll-up)
  :bind
  (("C-v" . d/scroll-down)
   ("M-v" . d/scroll-up)))

(defun d/scroll-down ()
  "Trust me, make scrolling alot smoother.
+1 Makes you fall in love with Emacs again!"
  (interactive)
  (pixel-scroll-precision-scroll-down 20))

(defun d/scroll-up ()
  "Trust me, adds a wonderfull smooth scroll.
You can do this by trackpad too (laptop)"
  (interactive)
  (pixel-scroll-precision-scroll-up 20))

(use-package tooltip
  :ensure nil
  :defer t
  :config
  (tooltip-mode -1))

(use-package time
  :ensure nil
  :defer t
  :hook
  (after-init . display-time)
  :custom
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  (display-time-format "%H:%M"))

(use-package battery
  :ensure nil
  :hook
  (after-init . display-battery-mode)
  :custom
  ;; better to keep charge between 40-80
  (battery-load-low '40)
  (battery-load-critical '29))

(use-package winner
  :ensure nil
  :hook after-init
  :bind
  ("C-c w n" . winner-undo)
  ("C-c w p" . winner-redo)
  :commands (winner-undo winnner-redo))

(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :defer t
  :hook '(prog-mode help-mode)
  :bind ("C-c t c" . rainbow-mode))

(use-package image-mode
  :ensure nil
  :defines (d/on-droid olivetti-body-width)
  :functions (olivetti-mode)
  :unless d/on-droid
  :bind (:map image-mode-map
              ("q" . d/kill-buffer))
  :hook
  (image-mode . (lambda () (olivetti-mode) (setq olivetti-body-width 0.45))))

(use-package select
  :ensure nil
  :custom
  (selection-coding-system 'utf-8)
  (x-select-request-type 'text/plain\;charset=utf-8)
  (select-enable-clipboard t "Use the clipboard"))

(use-package simple
  :ensure nil
  :bind
  ("M-c" . d/flex)
  ("M-l" . downcase-dwim))

;; taken from an planet emacs rss feed post
;; Stolen from the wiki somewhere
(defun increment-number-at-point ()
  "Increment the number at point."
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun d/flex ()
  "Perform smart flexing at point.

E.g. capitalize or decapitalize the next word, increment number at point."
  (interactive)
  (let ((case-fold-search nil))
    (call-interactively
     (cond
      ((looking-at "[[:space:]]") (forward-char 1) (d/flex))
      ((looking-at "[0-9]+") #'increment-number-at-point)
      ((looking-at "[[:lower:]]") #'capitalize-word)
      ((looking-at "==") (delete-char 1) (insert "!") (forward-char 2))
      ((looking-at "!=") (delete-char 1) (insert "=") (forward-char 2))
      ((looking-at "+") (delete-char 1) (insert "-") (forward-char 1))
      ((looking-at "-") (delete-char 1) (insert "+") (forward-char 1))
      ((looking-at "<=") (delete-char 2) (insert ">=") (forward-char 2))
      ((looking-at ">=") (delete-char 2) (insert "<=") (forward-char 2))
      ((looking-at "<") (delete-char 1) (insert ">") (forward-char 1))
      ((looking-at ">") (delete-char 1) (insert "<") (forward-char 1))
      (t #'downcase-word)))))

;; Taken from gopar's config (via Yt video)
;; https://github.com/gopar/.emacs.d
(use-package type-break
  :disabled
  :ensure nil
  :hook (after-init)

  :custom
  ;; Setting interval of that of a pomodoro session
  (type-break-interval (* 25 60)) ;; 25 mins
  (type-break-good-rest-interval (* 9 60)) ;; 9 mins
  (type-break-good-break-interval (* 5 60)) ;; 5 mins
  (type-break-query-mode t)
  (type-break-keystroke-threshold '(nil . 2625))
  (type-break-demo-boring-stats t)
  (type-break-demo-functions '(type-break-demo-agenda)))

(defun type-break-demo-agenda ()
  "Display the Org Agenda in read-only mode. Cease the demo as soon as a key is pressed."
  (let ((buffer-name "*Typing Break Org Agenda*")
        lines)
    (condition-case ()
        (progn
          (org-agenda-list)
          (setq buffer-name (buffer-name))
          ;; Set the buffer to read-only
          (with-current-buffer buffer-name
            (read-only-mode 1))
          ;; Message to be displayed at the bottom
          (let ((msg (if type-break-terse-messages
                         ""
                       "Press any key to resume from typing break")))
            ;; Loop until key is pressed
            (while (not (input-pending-p))
              (sit-for 60))
            ;; Clean up after key is pressed
            (read-event)
            (type-break-catch-up-event)
            (kill-buffer buffer-name)))
      (quit
       (and (get-buffer buffer-name)
            (kill-buffer buffer-name))))))

(defvar d/font-size (if d/on-droid 150 140)
  "Default font size based on the system.")
(defvar d/variable-font-size (if d/on-droid 160 160)
  "Default variable pitch size")

;; Dont worry about the font name, I use fork of Recursive font

;; Set reusable font name variables
(defvar d/fixed-pitch-font "Code D OnePiece"
  "The font to use for monospaced (fixed width) text.")

(defvar d/variable-pitch-font "Code D Haki"
  "The font to use for variable-pitch (documents) text.")

(use-package faces
  :ensure nil
  :defines
  (d/on-droid
   d/variable-font-size
   d/fixed-pitch-font
   d/variable-pitch-font
   d/font-size)

  :custom
  (face-font-family-alternatives
   '(("Monospace" "Code D OnePiece" "JetBrainsMono Nerd Font")
     ("Consolas" "JetBrainsMono Nerd Font" "Roboto Mono" "PT Mono" "Terminus" "Monospace")
     ("Monospace Serif" "CMU Typewriter Text" "Courier 10 Pitch" "Monospace")
     ("Serif" "Alegreya" "Noto Sans" "Georgia" "Cambria" "Times New Roman" "DejaVu Serif" "serif")))

  :custom-face
  (variable-pitch ((t (:family ,d/variable-pitch-font :height ,d/variable-font-size))))
  (fixed-pitch ((t (:family ,d/fixed-pitch-font :height ,d/font-size))))
  (default ((t (:family ,d/fixed-pitch-font :height ,d/font-size)))))

(use-package font-lock
  :ensure nil
  :defer t
  :custom ((font-lock-maximum-decoration t)
     (font-lock-global-modes '(not text-mode))
     (font-lock-verbose nil))
  :config
  (set-language-environment "UTF-8")
  (global-font-lock-mode 1))

(use-package haki-theme
  :demand t
  :load-path "~/.config/emacs/var/theme"
  :custom
  (haki-heading-font "Code D Zoro")
  (haki-sans-font "Code D Haki")
  (haki-title-font "Code D Zoro")
  (haki-link-font "Maple Mono")
  (haki-code-font "Code D Lip")
  :config
  (load-theme 'haki t))

(add-to-list 'term-file-aliases '("foot" . "xterm"))

(use-package emacs
  :ensure nil

  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui nil)
  (modus-themes-custom-auto-reload t)
  (modus-themes-disable-other-themes t)
  (modus-themes-prompts '(italic bold))
  (modus-themes-org-blocks 'gray-background)
  (modus-themes-completions
   '((matches . (extrabold))
     (selection . (semibold italic text-also))))

  (modus-themes-org-blocks 'gray-background)

  (modus-themes-headings
   '((1 . (variable-pitch 1.1))
     (2 . (1.1))
     (agenda-date . (1.2))
     (agenda-structure . (variable-pitch light 1.8))
     (t . (1.1)))))

(use-package olivetti
  :defer t
  :disabled t
  :hook
  (org-mode text-mode Info-mode helpful-mode ement-room-mode
            sdcv-mode nov-mode elfeed-show-mode markdown-mode)
  :custom
  (olivetti-body-width 0.92)
  (olivetti-minimum-body-width 40)
  (olivetti-recall-visual-line-mode-entry-state t)
  :delight " ⊛")

(use-package doom-modeline
  :disabled t
  :functions
  (doom-modeline-mode)
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-bar-width 7)
  (doom-modeline-major-mode-icon t)
  (inhibit-compacting-font-caches t)
  (doom-modeline-support-imenu t)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-time-icon t)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-buffer-name t)
  (doom-modeline-highlight-modified-buffer-name t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count t)
  (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-number-limit 99)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-workspace-name nil)
  (doom-modeline-persp-name nil)
  (doom-modeline-display-default-persp-name nil)
  (doom-modeline-persp-icon t)
  (doom-modeline-lsp t)
  (doom-modeline-github t)
  (doom-modeline-modal t)
  (doom-modeline-modal-icon t)
  (doom-modeline-battery t)
  (doom-modeline-env-version t)
  (doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
  (doom-modeline-env-load-string "...")

  (doom-modeline-height 30)
  (doom-modeline-buffer-encoding nil))

;; new way of using mode-line with `mini-echo-mode`
(use-package mini-echo
  :unless d/on-droid
  :load-path "~/d-git/forks/mini-echo"
  :demand t

  :config

  (defface mini-echo-elfeed
    '((t (:inherit elfeed-search-unread-count-face)))
    "Face for mini-echo segment of word count."
    :group 'mini-echo)

  ;; add elfeed unread counts
  (mini-echo-define-segment "elfeed"
    "Return unread feeds counts from elfeed."
    :fetch
    (propertize
     (let ((bufn "*elfeed-search*"))
       (if (get-buffer bufn)
           (concat "󰎕 "

                   (string-trim-right
                    (with-current-buffer bufn (elfeed-search--count-unread))
                    "/.*")) "")) 'face 'mini-echo-elfeed))

  (mini-echo-define-segment "battery"
    "Return the battery status.
Display format is inherited from `battery-mode-line-format'."
    :setup (display-battery-mode 1)
    :fetch
    (propertize
     (concat "󰁿"
     (string-trim
      (battery-format "%p%"
                      (funcall battery-status-function))))
                'face 'mini-echo-battery))

  (setopt mini-echo--toggled-segments '(("battery" . t) ("elfeed". t) ("time" . t)))

  (mini-echo-mode 1))

(global-set-key [f9] #'toggle-mode-line)

(defun toggle-mode-line ()
  "toggle the modeline on and off."
  (interactive)
  (setq mode-line-format
        (if (equal mode-line-format nil)
            (default-value 'mode-line-format)))
  (redraw-display))

(use-package dashboard
  :functions (dashboard-setup-startup-hook)

  :bind (:map dashboard-mode-map
              ("n" . 'dashboard-next-line)
              ("p" . 'dashboard-previous-line)
              )

  :custom
  (initial-buffer-choice 'dashboard-open)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-banner-logo-title "let's get to more 🔱 tasks today!")
  ;; (dashboard-startup-banner "~/.config/emacs/var/butterfly.png")
  (dashboard-startup-banner 'logo)
  (dashboard-image-banner-max-width 100) ;; 100 for logo
  (dashboard-center-content nil)
  (dashboard-set-navigator t) ;; a custom made navigator
  (dashboard-projects-backend 'project-el)
  (dashboard-display-icons-p t)
  (dashboard-show-shortcuts nil)
  (dashboard-set-init-info nil)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-items '((recents . 5)
                     (agenda . 10)
                     (projects . 2)
                     (bookmarks . 3)))
  (dashboard-modify-heading-icons '((recents . "file-text")
				                    (bookmarks . "book")))

  (dashboard-navigator-buttons
   `(;; line1
     ((,(nerd-icons-faicon "nf-fa-newspaper_o")
       " News"
       "Opens Elfeed"
       (lambda (&rest _) (d/elfeed-open)) nil "" " |")

      (,(nerd-icons-mdicon "nf-md-notebook")
       " Notes"
       "Denote Tree"
       (lambda (&rest _) (call-interactively #'denote-open-or-create)) warning "" " |")

      (,(nerd-icons-faicon "nf-fa-gitlab")
       " Project"
       "Open Project finder"
       (lambda (&rest _) (project-find-file)) error "" " |")

      (,(nerd-icons-octicon "nf-oct-terminal")
       " Terminal"
       "Open Eshell/Eat"
       (lambda (&rest _) (if (string= (completing-read "Hello : " '("eat" "eshell")) "eat")
                             (eat) (eshell))) warning "" " |")

      (,(nerd-icons-mdicon "nf-md-library")
       " Library"
       "Books and Docs"
       (lambda (&rest _) (find-dired "~/d-sync/reads" "")) nil "" "")
      )
     ;; line 2
     (
      (,(nerd-icons-faicon "nf-fa-music")
       " Music"
       "Play Jazz/Rhythm"
       (lambda (&rest _) (if d/on-droid (d/key-droid) (mingus))) error "" " |")

      (,(nerd-icons-faicon "nf-fa-reddit_alien")
       " Geek"
       "Browse Info"
       (lambda (&rest _) (reddigg-view-sub)) warning "" " |")

      (,(nerd-icons-mdicon "nf-md-apple_keyboard_command")
       " M-x"
       "Execute Command"
       (lambda (&rest _) (call-interactively #'execute-extended-command)) nil "" " |")

      (,(nerd-icons-sucicon "nf-seti-todo")
       " Agenda"
       "TODO Agenda"
       (lambda (&rest _) (org-agenda)) warning "" " |")

      (,(nerd-icons-mdicon "nf-md-bookmark")
       " Bookmark"
       "Open Bookmark File"
       (lambda (&rest _) (d/open-bookmark)) error "" "")

      )
     ;; Empty line
     ;; (("" "\n" "" nil nil "" ""))

     ;; Keybindings
     ))

  (dashboard-footer-messages '("Power Maketh Man Beneath" "Manners Maketh Man" "Tasks, Break, Action Works all the time" "Stop thinking, Just do it"))

  :config
  (dashboard-setup-startup-hook))

(use-package nerd-icons
  :custom
  (nerd-icons-font-family d/fixed-pitch-font))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :functions (nerd-icons-completion-mode)
  :unless d/on-droid
  :init
  (nerd-icons-completion-mode))

(define-widget 'nerd-icons-corfu-icon-type 'plist
  "The type of an icon mapping."
  :tag "Icon parameters"
  :options '((:style (choice (const :tag "wicon" "w")
                             (const :tag "faicon" "fa")
                             (const :tag "flicon" "fl")
                             (const :tag "mdicon" "md")
                             (const :tag "codicon" "cod")
                             (const :tag "devicon" "dev")
                             (const :tag "ipsicon" "ips")
                             (const :tag "octicon" "oct")
                             (const :tag "pomicon" "pom")
                             (const :tag "sucicon" "suc")))
             (:icon string)
             (:face face)))

(defcustom nerd-icons-corfu-mapping
  '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
    (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
    (class :style "cod" :icon "symbol_class" :face font-lock-type-face)
    (color :style "cod" :icon "symbol_color" :face success)
    (command :style "cod" :icon "terminal" :face default)
    (constant :style "cod" :icon "symbol_constant" :face font-lock-constant-face)
    (constructor :style "cod" :icon "triangle_right" :face font-lock-function-name-face)
    (enummember :style "cod" :icon "symbol_enum_member" :face font-lock-builtin-face)
    (enum-member :style "cod" :icon "symbol_enum_member" :face font-lock-builtin-face)
    (enum :style "cod" :icon "symbol_enum" :face font-lock-builtin-face)
    (event :style "cod" :icon "symbol_event" :face font-lock-warning-face)
    (field :style "cod" :icon "symbol_field" :face font-lock-variable-name-face)
    (file :style "cod" :icon "symbol_file" :face font-lock-string-face)
    (folder :style "cod" :icon "folder" :face font-lock-doc-face)
    (interface :style "cod" :icon "symbol_interface" :face font-lock-type-face)
    (keyword :style "cod" :icon "symbol_keyword" :face font-lock-keyword-face)
    (macro :style "cod" :icon "symbol_misc" :face font-lock-keyword-face)
    (magic :style "cod" :icon "wand" :face font-lock-builtin-face)
    (method :style "cod" :icon "symbol_method" :face font-lock-function-name-face)
    (function :style "cod" :icon "symbol_method" :face font-lock-function-name-face)
    (module :style "cod" :icon "file_submodule" :face font-lock-preprocessor-face)
    (numeric :style "cod" :icon "symbol_numeric" :face font-lock-builtin-face)
    (operator :style "cod" :icon "symbol_operator" :face font-lock-comment-delimiter-face)
    (param :style "cod" :icon "symbol_parameter" :face default)
    (property :style "cod" :icon "symbol_property" :face font-lock-variable-name-face)
    (reference :style "cod" :icon "references" :face font-lock-variable-name-face)
    (snippet :style "cod" :icon "symbol_snippet" :face font-lock-string-face)
    (string :style "cod" :icon "symbol_string" :face font-lock-string-face)
    (struct :style "cod" :icon "symbol_structure" :face font-lock-variable-name-face)
    (text :style "cod" :icon "text_size" :face font-lock-doc-face)
    (typeparameter :style "cod" :icon "list_unordered" :face font-lock-type-face)
    (type-parameter :style "cod" :icon "list_unordered" :face font-lock-type-face)
    (unit :style "cod" :icon "symbol_ruler" :face font-lock-constant-face)
    (value :style "cod" :icon "symbol_field" :face font-lock-builtin-face)
    (variable :style "cod" :icon "symbol_variable" :face font-lock-variable-name-face)
    (t :style "cod" :icon "code" :face font-lock-warning-face))
  "Mapping of completion kinds to icons.

It should be a list of elements with the form (KIND :style ICON-STY :icon
ICON-NAME [:face FACE]).  KIND is a symbol determining what the completion is,
and comes from calling the `:company-kind' property of the completion. ICON-STY
is a string with the icon style to use, from those available in Nerd Fonts.
ICON-NAME is a string with the name of the icon.  FACE, if present, is applied
to the icon, mainly for its color. The special t symbol should be used for KIND
to represent the default icon, and must be present."
  :type '(alist :key-type symbol :value-type nerd-icons-corfu-icon-type)
  :group 'nerd-icons-corfu)

;;;###autoload
(defun nerd-icons-corfu-formatter (_)
  "A margin formatter for Corfu, adding icons.

It receives METADATA, ignores it, and outputs a function that takes a candidate
and returns the icon."
  (when-let ((kindfunc (plist-get completion-extra-properties :company-kind)))
    (lambda (cand)
      (let* ((kind (funcall kindfunc cand))
             (icon-entry (or (alist-get (or kind t) nerd-icons-corfu-mapping)
                             (alist-get t nerd-icons-corfu-mapping)))
             (style (plist-get icon-entry :style))
             (icon (plist-get icon-entry :icon))
             (icon-fun (intern (concat "nerd-icons-" style "icon")))
             (icon-name (concat "nf-" style "-" icon))
             (face (plist-get icon-entry :face))
             (str (or (and (fboundp icon-fun) (funcall icon-fun icon-name :face face)) "?"))
             (space (propertize " " 'display '(space :width 1))))
        (concat " " str space)))))

(with-eval-after-load 'corfu
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package man
  :ensure nil
  :defer t
  :custom
  (Man-notify-method 'pushy "show manpage HERE")
  :custom-face
  (Man-overstrike ((t (:inherit font-lock-type-face :bold t))))
  (Man-underline ((t (:inherit font-lock-keyword-face :underline t))))
  :bind (("C-c m" . consult-man)
         :map Man-mode-map
         ("q" . kill-buffer-and-window)))

(use-package woman
  :ensure nil
  :defer t
  :custom-face
  (woman-bold ((t (:inherit font-lock-type-face :bold t))))
  (woman-italic ((t (:inherit font-lock-keyword-face :underline t)))))

(use-package flycheck
  :defer t
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (flycheck-idle-change-delay 3)
  ;; more form doom
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-buffer-switch-check-intermediate-buffers t)
  (flycheck-display-errors-delay 0.25))

(use-package helpful
  :defines (helpful-mode-map)
  :hook (helpful-mode . toggle-mode-line)
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h x" . helpful-command)
  ("C-c C-d" . helpful-at-point)
  ("C-h o" . helpful-symbol)
  ("C-h F" . helpful-function)
  (:map helpful-mode-map
        ("q" . kill-buffer-and-window)))

(use-package embark
  :defer t

  :functions
  (embark-prefix-help-command
   embark-eldoc-first-target)

  :bind
  (("C-." . embark-act)
   ("C-;" . embark-act-all)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings)
   (:map embark-identifier-map
         ("d" . sdcv-search-input)
         ("ch" . color-name-to-hex)
         ("cr" . color-name-to-rgb))
   (:map embark-url-map
         ("b" . browse-url-generic)
         ("r" . reddigg-view-comments))
   (:map embark-file-map
         ("b" . browse-url-of-dired-file))
   (:map embark-region-map
         ("U" . webpaste-paste-buffer-or-region)))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(with-eval-after-load 'embark
  (defun sudo-find-file (file)
    "Open FILE as root."
    (interactive "FOpen file as root: ")
    (when (file-writable-p file)
      (find-file file)
      (user-error "File is user writeable, opening as user"))
    (find-file (if (file-remote-p file)
                   (concat "/" (file-remote-p file 'method) ":"
                           (file-remote-p file 'user) "@" (file-remote-p file 'host)
                           "|doas:root@"
                           (file-remote-p file 'host) ":" (file-remote-p file 'localname))
                 (concat "/doas:root@localhost:" file))))
  (define-key embark-file-map (kbd "S") 'sudo-find-file))

(use-package embark-consult
  :defer t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(defun color-name-to-hex (NAME)
  "Return hexadecimal value of color with NAME.
Return nil if NAME does not designate a valid color."
  (insert
   (when-let* ((rgb (color-name-to-rgb NAME))
               ;; Sets 2 digits per component.
               (hex (apply #'color-rgb-to-hex (append rgb '(2)))))
     hex)))

(use-package executable
  :ensure nil
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package eglot
  :defer t
  :ensure nil
  :defines
  (d/on-droid)

  :unless d/on-droid
  :commands (eglot eglot-format eglot-managed-p eglot--major-mode)
  ;; (((web-mode rust-mode python-mode sh-mode c-mode c++-mode nix-mode) .
  ;; eglot-ensure)
  ;; :hook
  ;; (nix-mode . eglot-ensure)
  :custom
  (eglot-sync-connect 1)
  (eglot-connect-timeout 5)
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 45)
  (eglot-auto-display-help-buffer nil)

  :bind
  (:map eglot-mode-map
        ("C-c l r" . eglot-rename)
        ("C-c C-d" . eldoc)
        ("C-c l a" . eglot-code-actions)
        ("C-c l i" . consult-eglot-symbols))
  :config
  (add-to-list 'eglot-server-programs '(nix-mode . ("nixd")))
  )
;;   (add-to-list 'eglot-server-programs '(bash-ts-mode . ("bash-language-server")))
;;   (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman"))))

(use-package elisp-mode
  :ensure nil
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-d C-d" . describe-function)
        ("C-c C-d d" . describe-function)
        ("C-c C-k" . eval-buffer)))

(use-package treesit
  :ensure nil
  :mode
  ("\\.yaml\\'" . yaml-ts-mode)
  ("\\.toml\\'" . toml-ts-mode)
  ("\\.jsonrc\\'" . json-ts-mode)

  :custom
  (treesit-font-lock-level 4)
  (treesit-font-lock-feature-list t)
  (major-mode-remap-alist
   '((c-mode . c-ts-mode)
     (c++-mode . c++-ts-mode)
     (csharp-mode . csharp-ts-mode)
     (css-mode . css-ts-mode)
     (java-mode . java-ts-mode)
     (js-mode . js-ts-mode)
     (js-json-mode . json-ts-mode)
     ;; (org-mode . org-ts-mode) ;; not mature yet
     (python-mode . python-ts-mode)
     (julia-mode . ess-julia-mode)
     (typescript-mode . typescript-ts-mode)
     (sh-mode . bash-ts-mode)
     (ruby-mode . ruby-ts-mode)
     (rust-mode . rust-ts-mode)
     (toml-mode . toml-ts-mode)
     (yaml-mode . yaml-ts-mode))))

(use-package devdocs-browser
  :bind
  ("C-c d v" . devdocs-browser-open-in)
  :hook
  (devdocs-browser-eww-mode . shrface-mode)
  :custom
  (devdocs-browser-major-mode-docs-alist
   '((c++-ts-mode "cpp")
     (c-ts-mode "c")
     (go-ts-mode "go")
     (python-ts-mode "Python")
     (emacs-lisp-mode "elisp")
     (rust-ts-mode "rust")
     (cmake-mode "CMake")))

  (devdocs-browser-highlight-lang-mode-alist
   '(("c" . c-ts-mode)
     ("cpp" . c++-ts-mode)
     ("py" . python-ts-mode)
     ("bash" . bash-ts-mode)
     ("shell" . bash-ts-mode)
     ("python" . python-ts-mode))))

(use-package envrc
  :defer 2
  :config
  (envrc-global-mode))

(use-package elec-pair
  :ensure nil
  :hook
  (prog-mode . electric-pair-local-mode)
  :custom
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  (electric-pair-preserve-balance t)
  (electric-pair-pairs
   '((8216 . 8217)
     (8220 . 8221)
     (171 . 187)))
  (electric-pair-skip-self 'electric-pair-default-skip-self)
  (electric-pair-skip-whitespace nil)
  (electric-pair-skip-whitespace-chars '(9 10 32)))

(use-package electric
  :custom
  (electric-quote-context-sensitive t)
  (electric-quote-paragraph t)
  (electric-quote-string nil)
  (electric-quote-replace-double t)
  :config
  (electric-indent-mode -1)
  (add-hook 'prog-mode-hook #'electric-indent-local-mode))

(use-package magit
  :defer t
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-refine-hunk t))

(use-package ediff
  :ensure nil
  :custom
  (ediff-keep-variants nil)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

;; taken from Robb Enzmann
(defun d/pyrightconfig-write (virtualenv)
  "Write a `pyrightconfig.json' file at the Git root of a project,
with `venvPath' and `venv' set to the absolute path of
`virtualenv'.  When run interactively, prompts for a directory to select."
  (interactive "DEnv: ")
  ;; Naming convention for venvPath matches the field for pyrightconfig.json
  (let* ((venv-dir (tramp-file-local-name (file-truename virtualenv)))
         (venv-file-name (directory-file-name venv-dir))
         (venvPath (file-name-directory venv-file-name))
         (venv (file-name-base venv-file-name))
         (base-dir (vc-git-root default-directory))
         (out-file (expand-file-name "pyrightconfig.json" base-dir))
         (out-contents (json-encode (list :venvPath venvPath :venv venv))))
    (with-temp-file out-file (insert out-contents))
    (message (concat "Configured `" out-file "` to use environment `" venv-dir))))

(use-package python
  :ensure nil
  :hook ((python-mode . (lambda ()
                          (setq-local completion-at-point-functions
                                      '(cape-file python-completion-at-point cape-dabbrev)
                                      devdocs-browser-active-docs
                                      '("Python")))))

  :bind
  (:map python-mode-map
        ("C-c C-d" . devdocs-browser-open))
  :custom
  (python-shell-dedicated 'project)
  (python-shell-interpreter "python")
  (python-shell-interpreter-args "-i")
  ;; (python-forward-sexp-function nil)
  (python-indent-guess-indent-offset-verbose nil)
  (python-shell-completion-native-disabled-interpreters '("pypy")))

(use-package ess
  :defer t

  :custom
  (ess-use-company nil)
  (ess-eval-visibly nil)
  (ess-ask-for-ess-directory t)

  (ess-use-eldoc t)
  (ess-eldoc-show-on-symbol t)

  (ess-R-font-lock-keywords '((ess-R-fl-keyword:keywords . t)
                              (ess-R-fl-keyword:constants . t)
                              (ess-R-fl-keyword:modifiers . t)
                              (ess-R-fl-keyword:fun-defs . t)
                              (ess-R-fl-keyword:assign-ops . t)
                              (ess-R-fl-keyword:%op% . t)
                              (ess-fl-keyword:fun-calls . t)
                              (ess-fl-keyword:numbers . t)
                              (ess-fl-keyword:operators . t)
                              (ess-fl-keyword:delimiters . t)
                              (ess-fl-keyword:= . t)
                              (ess-R-fl-keyword:F&T . t)))

  (inferior-R-font-lock-keywords '((ess-S-fl-keyword:prompt . t)
                                   (ess-R-fl-keyword:keywords . t)
                                   (ess-R-fl-keyword:constants . t)
                                   (ess-R-fl-keyword:modifiers . t)
                                   (ess-R-fl-keyword:messages . t)
                                   (ess-R-fl-keyword:fun-defs . t)
                                   (ess-R-fl-keyword:assign-ops . t)
                                   (ess-fl-keyword:matrix-labels . t)
                                   (ess-fl-keyword:fun-calls . t)
                                   (ess-fl-keyword:numbers . t)
                                   (ess-fl-keyword:operators . t)
                                   (ess-fl-keyword:delimiters . t)
                                   (ess-fl-keyword:= . t)
                                   (ess-R-fl-keyword:F&T . t)))

  )

(use-package ess-r-mode
  ;; :hook (ess-r-mode . (lambda () (flycheck-mode 0)))
  :bind
  (:map ess-mode-map
        ("C-;" . ess-insert-assign))
  (:map inferior-ess-r-mode-map
        ("C-;" . ess-insert-assign))

  :custom
  (ess-indent-with-fancy-comments nil))

(use-package ess-julia
  :hook (ess-julia-mode . (lambda () (setq-local devdocs-browser-active-docs '("Julia"))))
  :bind
  (:map ess-julia-mode-map
        ("C-c C-d" . devdocs-browser-open))
  :custom
  (inferior-julia-args "--color=yes" "You get color in julia inferior process"))

(use-package julia-mode)

(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'")
  :defines (nix-mode-map)
  :functions
  (comint-send-input)
  :bind (:map nix-mode-map
              ("C-c C-e" . nix-eval-line)))
(defun nix-eval-dwim ()
  (interactive)
  (let* ((start (line-beginning-position))
         (end (line-end-position))
         (region-string (buffer-substring (region-beginning) (region-end)))
         (msg (format "%s" (if (use-region-p) region-string (buffer-substring start end)))))
    (pop-to-buffer "*Nix-REPL*")
    (insert msg)
    (comint-send-input)
    (other-window 1)))

(use-package nix-drv-mode
  :ensure nix-mode
  :mode "\\.drv\\'")

(use-package nix-shell
  :ensure nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build))

(use-package nix-repl
  :ensure nix-mode
  :commands (nix-repl))

(use-package esh-mode
  :ensure nil
  :defines
  (eshell-prompt-regexp)
  :commands
  (eshell-send-input eshell/clear)
  :hook
  (eshell-mode . toggle-mode-line)
  (eshell-mode . electric-pair-local-mode)
  (eshell-mode . (lambda () (setq outline-regexp eshell-prompt-regexp)
                   (setq-local corfu-auto nil)
                   (setq-local scroll-margin 0)
                   (corfu-mode)))
  :bind
  ("<f12>" . d/eshell-toggle)
  (:map eshell-mode-map
        ("C-S-l" . d/clear-eshell))

  :custom
  (eshell-scroll-to-bottom-on-input t)
  (eshell-directory-name (expand-file-name "eshell" user-emacs-directory))
  (eshell-aliases-file (expand-file-name "alias" eshell-directory-name))
  (eshell-highlight-prompt t)
  (eshell-hist-ignoredups t)
  (eshell-cd-on-directory t)
  (eshell-visual-command nil)
  (eshell-pushd-dunique t)
  (eshell-last-dir-unique t)
  (eshell-last-dir-ring-size 32)
  (eshell-list-files-after-cd nil)
  (eshell-cd-shows-directory t)
  (eshell-prefer-lisp-functions nil)

  (eshell-prompt-function
   (lambda nil
     (concat
      "\n"
      (propertize (concat " 󰪥 " (eshell/pwd)) 'face `(:foreground "lightblue1"))
      (when (package-installed-p 'magit) (propertize (if (magit-get-current-branch) (concat "   " (magit-get-current-branch)) "") 'face '(:foreground "orangered1")))
      (when (package-installed-p 'envrc) (propertize (if (string= envrc--status 'none) "" "   ") 'face '(:foreground "mediumspringgreen")))
      (propertize (concat "   " (format-time-string "%H:%M" (current-time))) 'face '(:foreground "lightcyan1"))
      (propertize "\n 𝝺 " 'face `(:foreground "palegreen"))
      )))
  (eshell-prompt-regexp "^ 𝝺 "))

(defun d/clear-eshell ()
  (interactive)
  (eshell-send-input (eshell/clear t)))

(defun d/eshell-toggle ()
  "Minimal hack to toggle eshell."
  (interactive)
  (cond
   ((derived-mode-p 'eshell-mode) (if (one-window-p) (switch-to-prev-buffer) (delete-window)))
   ((one-window-p) (progn (select-window (split-window-below)) (shrink-window 7) (eshell)))
   (t (progn (other-window 1)
             (if (derived-mode-p 'eshell-mode) (delete-window)
               (progn (other-window -1) (select-window (split-window-below)) (shrink-window 7) (eshell)))))))

(use-package em-hist
  :ensure nil
  :bind
  (:map eshell-hist-mode-map
        ("M-s" . nil)
        ("M-s r" . consult-ripgrep)
        ("M-s s" . consult-history))
  :custom
  (eshell-buffer-maximum-lines 10000)
  (eshell-history-size 10000))

(use-package em-smart
  :ensure nil
  :custom
  (eshell-where-to-jump 'begin)
  (eshell-review-quick-commands nil)
  (eshell-smart-space-goes-to-end t))

(use-package eat
  :defines
  (d/on-droid
   eat-mode-map
   eat-semi-char-mode-map
   eat-char-mode-map
   eat--semi-char-mode)
  :functions
  (eat
   eat-emacs-mode
   eat-semi-char-mode)

  :unless d/on-droid
  :commands (eshell d/eshell-toggle d/eat-toggle)
  :hook
  (eshell-load . eat-eshell-mode)
  (eat-mode . toggle-mode-line)
  :bind
  ("C-c d t" . d/eat-toggle)
  ("S-<f12>" . d/eat-toggle)
  (:map eat-mode-map
        ("C-x C-q" . d/eat-read-write)
        ("<f12>" . d/eat-toggle)
        ("<f9>" . toggle-mode-line))
  (:map eat-semi-char-mode-map
        ("M-o" . nil)
        ("M-s" . nil)))

(defun d/eat-read-write ()
  (interactive)
  (if eat--semi-char-mode (eat-emacs-mode) (eat-semi-char-mode))
  )

(defun d/eat-toggle ()
  "Minimal hack to toggle eat."
  (interactive)
  (cond
   ((derived-mode-p 'eat-mode) (if (one-window-p) (switch-to-prev-buffer) (delete-window)))
   ((one-window-p) (progn (split-window-below) (other-window 1) (eat)
                          (shrink-window 7)))
   (t (progn (other-window 1)
             (if (derived-mode-p 'eat-mode) (delete-window)
               (progn (other-window -1) (split-window-below) (other-window 1) (eat) (shrink-window 7)))))))

(use-package dired
  :defer t
  :init (file-name-shadow-mode 1)
  :ensure nil
  :hook
  (dired-mode . dired-hide-details-mode)
  (dired-mode . dired-omit-mode)
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)
         ("C-c f f" . window-focus-mode)
         ("C-c f e" . (lambda () (interactive) (find-file "~/.config/emacs/lisp/")))
         ("C-c f s" . (lambda () (interactive) (find-file "~/d-git/d-nix/")))
         ("C-c f m" . (lambda () (interactive) (find-file "~/d-git/d-nix/README.org"))))
  (:map dired-mode-map
        ("q" . kill-buffer-and-window)
        ("j" . dired-next-line)
        ("k" . dired-previous-line)
        ("l" . dired-find-file)
        ("h" . dired-up-directory)
        ("b" . embark-act))

  :custom
  (dired-listing-switches "-agho --group-directories-first")
  (delete-by-moving-to-trash t)
  (dired-dwim-target t))

(use-package dired-x
  :ensure nil
  :custom
  ;; Make dired-omit-mode hide all "dotfiles"
  (dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'\\|^\\..*$"))

(use-package async
  :unless d/on-droid
  :demand t
  :config
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1))

(use-package dirvish
  :unless d/on-droid
  :functions
  (dirvish-override-dired-mode
   dirvish-subtree-toggle-or-open
   dired-mouse-drag-files
   dired-mouse-find-file
   dired-mouse-find-file-other-window)
  :defines (dirvish-mode-map)

  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/"                          "Home")
     ("d" "~/dloads/"                "Downloads")
     ;; ("m" "/mnt/"                       "Drives")
     ("t" "~/.local/share/Trash/files/" "TrashCan")))

  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  (dirvish-mode-line-format
   '(:left (sort symlink) :right (omit yank index)))
  (dirvish-attributes
   '(file-time file-size collapse subtree-state vc-state git-msg))
  (delete-by-moving-to-trash t)
  (dired-listing-switches
   "-l --almost-all --human-readable --group-directories-first --no-group")
  (dirvish-hide-cursor nil)

  ;; with emacs29
  (dired-mouse-drag-files t)
  (mouse-drag-and-drop-region-cross-program t)
  (mouse-1-click-follows-link nil)

  :bind
  (("C-c f d" . dirvish-fd)
   ("C-x C-d" . dirvish)
   ("C-c f t" . dirvish-side)
   :map dirvish-mode-map
   ("<mouse-1>" . 'dirvish-subtree-toggle-or-open)
   ("<mouse-2>" . 'dired-mouse-find-file-other-window)
   ("<mouse-3>" . 'dired-mouse-find-file)
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("K"   . dired-do-kill-lines)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

(use-package shrface
  :hook
  (eww-after-render . shrface-mode)
  (devdocs-browser-eww-mode . shrface-mode)

  :bind (:map shrface-mode-map
              ("<tab>" . shrface-outline-cycle)
              ("<backtab>" . shrface-outline-cycle-buffer)
              ("M-n" . shr-next-link)
              ("M-p" . shr-previous-link)
              ("M-l" . (lambda () (interactive) (shrface-links-consult) (call-interactively #'shr-browse-url)))
              ("M-h" . mark-paragraph)
              ("C-j" . shrface-next-headline)
              ("C-k" . shrface-previous-headline))
  :custom
  (shrface-item-bullet 8226)
  (shrface-bullets-bullet-list '("󰓏" "󰚀" "󰫤"  "󰴈" "" "󰄄"))
  (shrface-href-versatile t)
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings))

(use-package shr-tag-pre-highlight
  :demand
  :config
  (add-to-list 'shr-external-rendering-functions '(pre . shrface-shr-tag-pre-highlight)))
(defun shrface-shr-tag-pre-highlight (pre)
  "Highlighting code in PRE."
  (let* ((shr-folding-mode 'none)
         (shr-current-font 'default)
         (code (with-temp-buffer
                 (shr-generic pre)
                 ;; (indent-rigidly (point-min) (point-max) 2)
                 (buffer-string)))
         (lang (or (shr-tag-pre-highlight-guess-language-attr pre)
                   (let ((sym (language-detection-string code)))
                     (and sym (symbol-name sym)))))
         (mode (and lang
                    (shr-tag-pre-highlight--get-lang-mode lang))))
    (shr-ensure-newline)
    (shr-ensure-newline)
    (setq start (point))
    (insert
     (propertize (concat "#+BEGIN_SRC " lang "\n") 'face 'org-block-begin-line)
     (or (and (fboundp mode)
              (with-demoted-errors "Error while fontifying: %S"
                (shr-tag-pre-highlight-fontify code mode)))
         code)
     (propertize "#+END_SRC" 'face 'org-block-end-line ))
    (shr-ensure-newline)
    (setq end (point))
    (add-face-text-property start end '(:background "#292b2e" :extend t :inherit fixed-pitch))
    (shr-ensure-newline)
    (insert "\n")))

(setopt shr-tag-pre-highlight-lang-modes
        '(
          ("elisp" . emacs-lisp)
          ("emacs-lisp" . emacs-lisp)
          ("ditaa" . artist)
          ("asymptote" . asy)
          ("dot" . fundamental)
          ("sqlite" . sql)
          ("calc" . fundamental)
          ("c" . c-ts)
          ("cpp" . c++-ts)
          ("C++" . c++-ts)
          ("screen" . shell-script)
          ("shell" . bash-ts)
          ("awk" . bash-ts)
          ("bash" . bash-ts)
          ("sh" . bash-ts)
          ("python" . python-ts)
          ("R" . ess-r)
          ("julia" . ess-julia)
          ("sql" . sql)
          ("ruby" . ruby)
          ("el" . emacs-lisp)))

(use-package url
  :ensure nil
  :custom
  (url-user-agent "")
  (url-privacy-level 'paranoid)
  (url-mime-accept-string "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8 ")
  (url-mime-charset-string nil)
  (url-mime-language-string "en-US,en;q=0.5")
  (url-mime-encoding-string "gzip, deflate")
  :config
  (url-setup-privacy-info))

(use-package shr
  :ensure nil
  :demand t
  :custom
  (shr-use-fonts  t)
  (shr-use-colors nil)
  (shr-indentation 4)
  (shr-bullet "• ")
  (shr-folding-mode t)
  (shr-max-width 120)
  (shr-max-image-proportion 0.4)
  (shr-width nil))

(use-package shr-color
  :ensure nil
  :defer t
  :custom
  (shr-color-visible-luminance-min 40 "Improve the contrast"))

(use-package eww
  :ensure nil
  :demand t
  :commands (eww eww-search-words)
  :hook
  (eww-mode . variable-pitch-mode)
  (eww-after-render . (lambda () (eww-readable) (setq-local line-spacing '0.4)))

  :bind ("M-s M-w" . eww-search-words)
  (:map eww-mode-map
        ("e" . readable-article)
        ("Q" . d/kill-buffer)
        ("RET" . eww-follow-link)
        ("m" . elfeed-toggle-star)
        ("b" . nil))
  :custom
  (eww-auto-rename-buffer 'title)
  (eww-search-prefix "https://duckduckgo.com/html/&q="))

(defun eww-search-words ()
  "Search the web for the text in the region.
If region is active (and not whitespace), search the web for
the text between region beginning and end.  Else, prompt the
user for a search string.  See the variable `eww-search-prefix'
for the search engine used."
  (interactive)
  (if (use-region-p)
      (let ((region-string (buffer-substring (region-beginning) (region-end))))
        (if (not (string-match-p "\\`[ \n\t\r\v\f]*\\'" region-string))
            (eww-browse-url region-string t)
          (eww-browse-url (completing-read "Browse Url" eww-prompt-history))))
    (if (shr-url-at-point nil)
        (eww (shr-url-at-point nil))
      (eww-browse-url (completing-read "Browse Url: " eww-prompt-history)))))

(use-package gnutls
  :ensure nil
  :defer t
  :custom
  (gnutls-verify-error t))

(use-package browse-url
  :ensure nil
  :config
  ;; browser script
  (unless d/on-droid
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "d-stuff")
    (setq browse-url-secondary-browser-function 'browse-url-generic
          browse-url-generic-program "d-stuff")))

(use-package ox-hugo
  :unless d/on-droid
  :after ox)

(with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
  See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ":END:"
                   "%?\n")          ;Place the cursor here finally
                 "\n")))

  (add-to-list 'org-capture-templates
               '("w" "Website Organize"))
  (add-to-list 'org-capture-templates
               '("wt" "website Todo" entry (file+headline "~/d-git/d-site/README.org" "Ideas - TODO")
                 "* TODO %?\n  SCHEDULED:%T\n " :empty-lines 1))
  (add-to-list 'org-capture-templates
               '("ww" "website work"
                 entry
                 (file+olp "~/d-git/d-site/org-mode/posts.org" "Posts")
                 (function org-hugo-new-subtree-post-capture-template))))

(defun d/external-browser ()
  (interactive)
  (cond ((image-at-point-p) (kill-new (or (shr-url-at-point nil) (plist-get (cdr (image--get-image)) :file))))
        ((or (thing-at-point 'url t) (dired-file-name-at-point) (shr-url-at-point nil)) (link-hint-copy-link-at-point))
        (t (link-hint-copy-link)))
  (let ((url (current-kill 0)))
    (if d/on-droid (browse-url url) (browse-url-generic url))))

(use-package nov
  :functions
  (toggle-mode-line)

  :hook
  (nov-mode . (lambda () (toggle-mode-line) (variable-pitch-mode)))
  (nov-mode . shrface-mode)

  :mode ("\\.epub\\'" . nov-mode)
  :custom
  (nov-text-width nil)
  (nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
  (nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions))
  (nov-variable-pitch t))

(use-package doc-view
  :ensure nil
  ;; :mode ("\\.epub\\'" . doc-view-mode)
  :bind (:map doc-view-mode-map
                ("M-g M-g" . doc-view-goto-page)
                ("<f8>" . doc-view-presentation))
  :custom
  (doc-view-continuous t)
  (doc-view-mupdf-use-svg t)
  (doc-view-scale-internally nil)
  (doc-view-image-width 900)
  (large-file-warning-threshold 700000000)
  (image-cache-eviction-delay 3))

;; Read normal text files as emacs info manuals
;; thanks to:
;;https://emacsnotes.wordpress.com/2023/09/11/view-info-texi-org-and-md-files-as-info-manual/
(defun d/text-info ()
  "View ‘info’, ‘texi’, ‘org’, ‘md’ and 'NEWS' files as ‘Info’ manual."
  (interactive)
  (require 'rx)
  (require 'ox-texinfo)
  (when (buffer-file-name)
    (let* ((org-export-with-broken-links 'mark)
           (ext (file-name-extension (buffer-file-name))))
      (cond
       ;; A NEWS files
       ((string-match "NEWS" (file-name-nondirectory (buffer-file-name)))
        (with-current-buffer
            ;; NEWS files are likely to be in read-only directories.
            ;; So make a copy with an `.org' extension.  Most NEWS
            ;; file are `outline-mode' files with `org' like heading
            ;; structure.  Many of the recent files like ORG-NEWS are
            ;; proper `org' files.
            (find-file-noselect
             (make-temp-file
              (format "%s---" (file-name-nondirectory (buffer-file-name))) nil ".org"
              (buffer-substring-no-properties (point-min) (point-max))))
          (org-with-wide-buffer
           ;; `ox-texinfo' export fails if a headline ends with a
           ;; period (= ".").  So, strip those terminating periods.
           (goto-char (point-min))
           (while (re-search-forward (rx (and bol
                                              (one-or-more "*")
                                              " "
                                              (one-or-more any)
                                              (group ".")
                                              eol))
                                     (point-max) t)
             (replace-match "" t t nil 1))
           (goto-char (point-min))
           (while nil
             ;; TODO: If a NEWS file contains text which resemble a
             ;; LaTeX fragment, the `ox-texinfo' export wouldn't
             ;; succeed.  So, enclose the LaTeX fragment with Org's
             ;; verbatim `=' marker.
             )
           (save-buffer 0)
           (info (org-texinfo-export-to-info)))))
       ;; A `.info' file
       ((or (string= "info" ext))
        (info (buffer-file-name)))
       ;; A `.texi' file
       ((or (string= "texi" ext))
        (info (org-texinfo-compile (buffer-file-name))))
       ;; An `.org' file
       ((or (derived-mode-p 'org-mode)
            (string= "org" ext))
        (info (org-texinfo-export-to-info)))
       ;; A `.md' file
       ((or (derived-mode-p 'markdown-mode)
            (string= "md" ext))
        (let ((org-file-name (concat (file-name-sans-extension (buffer-file-name)) ".org")))
          (apply #'call-process "pandoc" nil standard-output nil
                 `("-f" "markdown"
                   "-t" "org"
                   "-o" ,org-file-name
                   ,(buffer-file-name)))
          (with-current-buffer (find-file-noselect org-file-name)
            (info (org-texinfo-export-to-info)))))
       (t (user-error "Don't know how to convert `%s' to an `info' file"
                      (buffer-file-name)))))))

(global-set-key (kbd "C-x x v") 'd/text-info)

(add-hook
 'view-mode-hook
 (lambda ()
   (define-key view-mode-map (kbd "l") 'recenter-top-bottom)
   (cond ((derived-mode-p 'org-mode)
          (define-key view-mode-map (kbd "p") 'org-previous-visible-heading)
          (define-key view-mode-map (kbd "n") 'org-next-visible-heading))
         ((derived-mode-p 'markdown-mode)
          (define-key view-mode-map (kbd "p") 'markdown-outline-previous)
          (define-key view-mode-map (kbd "n") 'markdown-outline-next))
         ((derived-mode-p 'html-mode)
          (define-key view-mode-map (kbd "p") 'sgml-skip-tag-backward)
          (define-key view-mode-map (kbd "n") 'sgml-skip-tag-forward))
         ((derived-mode-p 'python-ts-mode)
          (define-key view-mode-map (kbd "p") 'python-nav-backward-block)
          (define-key view-mode-map (kbd "n") 'python-nav-forward-block))
         ((derived-mode-p 'emacs-lisp-mode)
          (define-key view-mode-map (kbd "p") 'backward-sexp)
          (define-key view-mode-map (kbd "n") 'forward-sexp))
         ((derived-mode-p 'makefile-mode)
          (define-key view-mode-map (kbd "p") 'makefile-previous-dependency)
          (define-key view-mode-map (kbd "n") 'makefile-next-dependency))
         ((derived-mode-p 'c-mode)
          (define-key view-mode-map (kbd "p") 'c-beginning-of-defun)
          (define-key view-mode-map (kbd "n") 'c-end-of-defun))
         (t
          (define-key view-mode-map (kbd "p") 'scroll-down-command)
          (define-key view-mode-map (kbd "n") 'scroll-up-command)))))

(add-hook 'view-mode-hook 'hl-line-mode)

(use-package elfeed
  :bind
  ("C-c d e" . d/elfeed-open)
  ("C-c d b" . embark-act)
  :commands
  (d/elfeed-open)
  :config
  (defface elfeed-search-star-title-face
    '((t :foreground "#f77"))
    "Marks a starred Elfeed entry.")
  (push '(star elfeed-search-star-title-face) elfeed-search-face-alist)
  (defun d/elfeed-open ()
    "Wrapper to load the elfeed db from disk before opening"
    (interactive)
    (unless (get-buffer "*elfeed-search*") (elfeed-db-load))
    (elfeed)
    (elfeed-search-update--force)))
   ;; (elfeed-update)))

(defun elfeed-toggle-show-star ()
  (interactive)
  (if (elfeed-tagged-p 'star elfeed-show-entry)
      (elfeed-show-untag 'star)
    (elfeed-show-tag 'star)))
;; (org-capture nil "l"))

(defun elfeed-toggle-star ()
  (interactive)
  (elfeed-search-toggle-all 'star))
;; (org-capture nil "l"))

(defun d/elfeed-ui ()
  (interactive)
  ;; (setq-local header-line-format " ")
  (variable-pitch-mode)
  (shrface-mode))

;;write to disk when quiting
(defun d/elfeed-quit ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

(defun d/elfeed-add-podcast ()
  "Play the enclosure URL in Mpd using 'mingus'."
  (interactive)
  (with-no-warnings
    (let* ((count (length (elfeed-entry-enclosures elfeed-show-entry)))
           (entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)))
           (dlink (shell-command-to-string (format "yt-dlp -f bestaudio -g '%s'" (shr-url-at-point nil)))))
      (require 'mingus)
      ;; (message (concat "Added: " (car (elt (elfeed-entry-enclosures elfeed-show-entry)
      ;;                                      (- enclosure-index 1)))))
      (message dlink)
      (mingus-add dlink))))
;; (cond ((shr-url-at-point nil) (shell-command-to-string (format "yt-dlp -f bestaudio -g '%s'" (shr-url-at-point current-prefix-arg))))
;;       ((derived-mode-p 'elfeed-show-mode)
;;        (if (zerop count)
;;            (shell-command-to-string (format "yt-dlp -f bestaudio -g '%s'" (elfeed-entry-link entry)))
;;          (car (elt (elfeed-entry-enclosures elfeed-show-entry)
;;                    (- enclosure-index 1)))))
;;       ((derived-mode-p 'elfeed-search-mode) (elfeed-search-selected :single))))))

(defun d/elfeed-play ()
  (interactive)
  (let* ((count (length (elfeed-entry-enclosures elfeed-show-entry)))
         (entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single))))
    (message (concat "Added: " (elfeed-entry-link entry)))
    (if (zerop count)
        (async-shell-command (format "mpc add $(yt-dlp -g \"%s\")" (elfeed-entry-link entry)) nil nil)
      (with-no-warnings
        (mingus-add (car (elt (elfeed-entry-enclosures elfeed-show-entry)
                              (- enclosure-index 1))))))
    ))

(defun d/elfeed-org-mark ()
  "use org file as bookmark for elfeed entries.
Usable as favorites or bookmark."
  (when elfeed-show-entry
    (let* ((link (elfeed-entry-link elfeed-show-entry))
           (title (elfeed-entry-title elfeed-show-entry)))
      (org-store-link-props
       :link link
       :description title))))

(defun elfeed-open-in-eww ()
  "open elfeed entry in eww."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single))))
    (eww-browse-url (elfeed-entry-link entry) t)))

(defun elfeed-open-in-reddit ()
  "open elfeed entry in reddit"
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single))))
    (reddigg-view-comments (elfeed-entry-link entry)))
  (display-buffer-pop-up-window (reddigg--get-cmt-buffer) nil))

(when d/on-droid
(with-eval-after-load 'elfeed
  (define-key elfeed-show-mode-map (kbd "<volume-up>") #'elfeed-show-prev)
  (define-key elfeed-show-mode-map (kbd "<volume-down>") #'elfeed-show-next)))

(defun d/elfeed-db-remove-entry (id)
  "Removes the entry for ID"
  (avl-tree-delete elfeed-db-index id)
  (remhash id elfeed-db-entries))

(defun d/elfeed-search-remove-selected ()
  "Remove selected entries from database"
  (interactive)
  (let* ((entries (elfeed-search-selected))
         (count (length entries)))
    (when (y-or-n-p (format "Delete %d entires?" count))
      (cl-loop for entry in entries
               do (d/elfeed-db-remove-entry (elfeed-entry-id entry)))))
  (elfeed-search-update--force))

(use-package elfeed-show
  :hook
  (elfeed-show-mode . d/elfeed-ui)

  :bind
  (:map elfeed-show-mode-map
        ("e" . elfeed-open-in-eww)
        ("i" . d/bionic-read)
        ("r" . elfeed-open-in-reddit)
        ("m" . elfeed-toggle-show-star)
        ("q" . d/elfeed-quit)
        ("C-x C-k" . d/elfeed-quit)
        ("P" . d/elfeed-add-podcast)
        ("A" . d/elfeed-play)
        ("b" . nil)))

(use-package elfeed-search
  :bind
  (:map elfeed-search-mode-map
        ("m" . elfeed-toggle-star)
        ("q" . d/elfeed-quit)
        ("G" . elfeed-search-fetch-visible)
        ("C-x C-k" . d/elfeed-quit)
        ("U" . elfeed-update)
        ("u" . elfeed-update-feed))

  :custom
  ;; (setq-default elfeed-search-filter "@1-week-ago--1-day-ago +unread -news +")
  (elfeed-search-filter "+unread +")
  (elfeed-search-date-format (if d/on-droid `("" 0 :left)  `("%d-%m 📰" 7 :left)))
  (elfeed-search-title-max-width 60)
  (elfeed-search-title-min-width 60)
  (elfeed-search-trailing-width 0)

  )

(use-package elfeed-org
  :after elfeed
  :custom
  (rmh-elfeed-org-files (list (expand-file-name "elfeed.org" user-emacs-directory)))
  :init
  (elfeed-org))

(use-package elfeed-log
  :after elfeed
  :custom
  (elfeed-log-level 'debug "debug, info, warn or error."))

(use-package avy
  :bind
  ("M-j" . avy-goto-char-timer)
  ("M-g w" . avy-goto-char-timer)
  ("M-K" . avy-kill-region)
  ("C-S-k" . avy-kill-whole-line)
  :custom
  (avy-background t))

(use-package jinx
  ;; :init (global-jinx-mode)
  :hook org-mode
  :bind ("M-$". jinx-correct))

(use-package denote
  :defer t
  :defines
  (dired-mode-map
   denote-directory)
  :hook ((find-file-hook . denote-link-buttonize-buffer)

         (dired-mode . denote-dired-mode))
  :bind
  ("C-c n j" . d/my-journal)
  ("C-c n s" . denote)
  ("C-c n t" . denote-type)
  ("C-c n d" . denote-date)
  ("C-c n n" . denote-subdirectory)
  ("M-s n" . denote-open-or-create)
  ("C-c n o" . denote-open-or-create)
  ("C-c n T" . denote-template)
  ("C-c n i" . denote-link)
  ("C-c n I" . denote-link-add-links)
  ("C-c n b" . denote-link-backlinks)
  ("C-c n f f" . denote-link-find-file)
  ("C-c n f b" . denote-link-find-backlink)
  ("C-c n r" . denote-rename-file)
  ("C-c n R" . denote-rename-file-using-front-matter)
  (:map dired-mode-map
        ("C-c C-d C-i" . denote-link-dired-marked-notes)
        ("C-c C-d C-r" . denote-dired-rename-marked-files)
        ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))

  :custom
  (denote-directory (expand-file-name "~/d-sync/notes"))
  (denote-known-keywords '("emacs" "blogs" "article"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-file-type nil)
  (denote-prompts '(title keywords))
  (denote-excluded-directories-regexp nil)
  (denote-excluded-keywords-regexp nil)
  (denote-date-prompt-use-org-read-date t)
  (denote-allow-multi-word-keywords t)
  (denote-date-format nil)
  (denote-backlinks-show-context t)
  (denote-dired-directories
   (list denote-directory
         (thread-last denote-directory (expand-file-name "attachments"))
         (expand-file-name "~/d-sync/notes/books/")))

  :config
  (defun d/my-journal ()
    (interactive)
    (let* ((date (org-read-date))
           (time (org-time-string-to-time date))
           (title (format-time-string "%A %d %B %Y" time))
           (initial (denote-sluggify title))
           (target (read-file-name "Select note: " (denote-directory) nil nil initial
                                   (lambda (f)
                                     (or (denote-file-has-identifier-p f)
                                         (file-directory-p f))))))
      (if (file-exists-p target)
          (find-file target)
        (denote title '("journal") denote-file-type nil date))))


  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%l\n%i\n%?")
    (add-to-list 'org-capture-templates
                 '("n" "New note (denote)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))

  (defun d/denote-add-to-agenda-files (keyword)
    "Append list of files containing 'keyword' to org-agenda-files"
    (interactive)
    ;; (jab/init-org-agenda-files) ;; start over
    (setq org-agenda-files (append org-agenda-files (directory-files denote-directory t keyword))))

  ;; (d/denote-add-to-agenda-files "_project")
  )

;; custom way to open up productive desk
;; from janusworx blog
(defun d/desk-ready ()
  "Getting ready for work."
  (interactive)
  (toggle-frame-maximized)
  (split-window-below)
  (find-file "~/d-sync/notes/d-desk.org")
  (find-file-other-window "~/d-sync/notes/tasks.org"))
(global-set-key (kbd "C-c r") 'd/desk-ready)

(use-package flycheck-languagetool
  :disabled
  :hook
  (text-mode . flycheck-mode)
  :custom
  (flycheck-languagetool-server-command '("languagetool-http-server"))
  (flycheck-languagetool-language "auto"))

(use-package org
  :ensure nil
  :defer t
  :commands
  (org-capture
   org-agenda
   org-indent-mode
   org-display-inline-images
   org-map-entries
   org-archive-subtree
   org-element-property
   org-element-at-point
   org-element-map
   org-element-parse-buffer
   )
  :hook (org-mode . (lambda ()
                      (org-display-inline-images 0)
                      (variable-pitch-mode 1)))

  :bind
  ("C-c c d" . calendar)
  ("C-c t R" . d/bionic-region)
  ("C-c t i" . d/set-timer)
  ("C-c t r" . d/bionic-read)
  (:map org-mode-map
        ("C-x n n" . d/narrow-or-widen-dwim))

  :custom
  (org-startup-indented nil)
  (org-image-actual-width 400)
  (org-startup-folded t)
  (org-ellipsis " ⮟")
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-done 'note)
  (org-log-into-drawer t)

  (org-todo-keywords
   '((sequence "TODO(t)" "|" "DONE(d!)")
     (sequence "REVIEW(v)" "|" "CANC(k@)")))

  (org-refile-targets
   '(("Archive.org" :maxlevel . 1)
     ("tasks.org" :maxlevel . 1)))

  (org-tag-alist
   '((:startgroup)
     (:endgroup)
     ("@work" . ?W)
     ("agenda" . ?a)
     ("linux" . ?l)
     ("planning" . ?p)
     ("note" . ?n)
     ("idea" . ?i)))

  (fill-column 80)
  ;; Where the org files live
  (org-directory "~/d-sync/notes")
  ;; Make sure we see syntax highlighting
  (org-src-fontify-natively t)
  ;; I dont use it for subs/super scripts
  (org-use-sub-superscripts nil)
  ;; Should everything be hidden?
  (org-startup-folded 'content)
  (org-M-RET-may-split-line '((default . nil)))
  ;; hide stars except for leader star
  (org-hide-leading-stars t)
  (org-hide-emphasis-markers nil)
  ;; Show as utf-8 chars
  (org-pretty-entities t)
  ;; put timestamp when finished a todo
  (org-log-done 'time)
  ;; timestamp when we reschedule
  (org-log-reschedule t)
  ;; Don't indent the stars
  (org-startup-indented nil)
  (org-list-allow-alphabetical t)
  (org-image-actual-width nil)
  ;; Save notes into log drawer
  (org-log-into-drawer t)
  ;;
  (org-fontify-whole-heading-line t)
  (org-fontify-done-headline t)
  ;;
  (org-fontify-quote-and-verse-blocks t)
  ;; See down arrow instead of "..." when we have subtrees
  ;; (org-ellipsis "⤵")
  ;; catch invisible edit
  ( org-catch-invisible-edits 'show-and-error)
  ;; Only useful for property searching only but can slow down search
  (org-use-property-inheritance t)
  ;; Count all children TODO's not just direct ones
  (org-hierarchical-todo-statistics nil)
  ;; Unchecked boxes will block switching the parent to DONE
  (org-enforce-todo-checkbox-dependencies t)
  ;; Don't allow TODO's to close without their dependencies done
  (org-enforce-todo-dependencies t)
  (org-track-ordered-property-with-tag t)
  ;; Where should notes go to? Dont even use them tho
  (org-default-notes-file (concat org-directory "notes.org"))

  ;; Needed to allow helm to compute all refile options in buffer
  (org-outline-path-complete-in-steps nil)
  (org-deadline-warning-days 2)
  (org-log-redeadline t)
  (org-log-reschedule t)
  ;; Repeat to previous todo state
  ;; If there was no todo state, then dont set a state
  (org-todo-repeat-to-state t)
  ;; Refile options
  (org-refile-use-outline-path 'file)
  (org-refile-allow-creating-parent-nodes 'confirm)
  ;; This worked ok, but lets try some more detail refiling
  ;; (org-refile-targets '((org-agenda-files :level .  1)))
  ;; Lets customize which modules we load up
  (org-modules '(ol-w3m
                 ol-bbdb
                 ol-bibtex
                 ol-docview
                 ol-gnus
                 ol-info
                 ol-irc
                 ol-mhe
                 ol-rmail
                 ol-eww
                 ;; Stuff I've enabled below
                 org-habit
                 ;; org-checklist
                 ))
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)

  :config
  (org-clock-persistence-insinuate)

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (defun org-archive-done-tasks ()
    "From the org-heading, it throws all the Done tasks to filename_archive.org"
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
     "/DONE" 'tree))

  ;; This is for managing literate nixos config in org file
  (defun get-named-src-block-contents (name &optional trim)
    "Return the contents of the named Org source block."
    (let* ((block (org-element-map (org-element-parse-buffer) 'src-block
                    (lambda (src-block)
                      (when (string= name (org-element-property :name src-block))
                        src-block))
                    nil t))
           (source (org-element-property :value block)))
      (if trim
          (string-trim source)
        source)))


  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (calc . t)
     (latex . t) (C . t)
     (R . t)
     (shell . t) (python . t)
     (julia . t)))
  (push '("conf-unix" . conf-unix) org-src-lang-modes)
  (defalias 'd/set-timer (symbol-function 'org-timer-set-timer))
  )

(use-package org-tempo
  :ensure nil
  :after org
  :config
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("p" . "src python"))
  (add-to-list 'org-structure-template-alist '("sh" . "src sh")))

(use-package org-modern
  :defer 1

  :commands
  (shrface-mode
   global-org-modern-mode)
  :hook (org-mode org-agenda-finalize-hook)

  :custom
  ;; Edit settings
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)

  ;; Org styling, hide markup etc.
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  ;;   org-ellipsis "…"

  ;; Reference:
  ;; Heading: "◉ ○ ✸ ✿"
  ;; Cool-Heading: ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
  ;; Small: ► • ★ ▸
  ;; others: ▼, ↴, ⬎, ⤷,…, and ⋱.
  ;; (org-ellipsis "⤵")
  ;; nerd-icons: "" "󰓏" "󰚀" "󰴈" "" "󰄄"

  ;; (org-modern-star '("◉" "✪" "◈" "✿" "❂"))
  ;; (org-modern-star '("" "󰓏" "󰚀" "󰴈" "" "󰄄"))
  (org-modern-star '("󰓏" "󰚀" "󰫤"  "󰴈" "" "󰄄"))
  (org-modern-hide-stars 'leading)
  (org-modern-table nil)
  (org-modern-list
   '((?* . "⁍")
     (?- . "❖")
     (?+ . "➤")))

  ;; Agenda styling
  (org-agenda-tags-column 0)
  (org-agenda-block-separator ?─)
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")

  :config
  ;; Add frame borders and window dividers
  (modify-all-frames-parameters
   '((right-divider-width . 1)
     (bottom-divider-width . 0)
     (internal-border-width . 5)))
  (dolist (face '(window-divider
                  window-divider-first-pixel
                  window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
  (global-org-modern-mode))

(use-package org-clock
  :ensure nil
  :after org
  :commands
  (org-clock-jump-to-current-clock
   org-clock-report)
  :custom
  ;; Save clock history accross emacs sessions (read var for required info)
  (org-clock-persist t)
  ;; If idle for more than 15 mins, resolve by asking what to do with clock
  (org-clock-idle-time 15)
  ;; Set clock in frame title, instead of mode line
  (org-clock-clocked-in-display 'frame-title)
  ;; Show more clocking history
  (org-clock-history-length 10)
  ;; Include running time in clock reports
  (org-clock-report-include-clocking-task t)
  ;; Put all clocking info int the "CLOCKING" drawer
  (org-clock-into-drawer "CLOCKING")
  ;; Setup default clocktable summary
  (org-clock-clocktable-default-properties
   '(:maxlevel 2 :scope file :formula % ;; :properties ("Effort" "Points")
               :sort (5 . ?t) :compact t :block today))
  :bind (:map global-map
              ("C-c j" . (lambda () (interactive) (org-clock-jump-to-current-clock)))
              :map org-mode-map
              ("C-c C-x r" . (lambda () (interactive) (org-clock-report)))))

(use-package org-agenda
  :ensure nil
  :after org
  :bind
  ("C-c d a" . org-agenda)
  (:map org-agenda-mode-map
        ("C-x C-k" . org-agenda-exit))

  ;; :hook (org-agenda-finalize . org-agenda-entry-text-mode)
  :custom
  (org-agenda-tags-column 'auto)
  (org-agenda-window-setup 'only-window)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-log-mode-items '(closed clock state))
  ;; (org-agenda-todo-ignore-scheduled 'future)
  ;; TODO entries that can't be marked as done b/c of children are shown as dimmed in agenda view
  (org-agenda-dim-blocked-tasks nil)
  (org-agenda-inhibit-startup t)
  (org-agenda-show-inherited-tags nil)
  (org-agenda-ignore-drawer-properties '(effort appt category))
  ;; Start the week view on whatever day im on
  (org-agenda-start-on-weekday nil)
  (org-agenda-files
   '("~/d-sync/notes/"
     "~/d-git/d-site/README.org"
     "~/d-sync/notes/bioinfo/")))

(use-package org-capture
  :ensure nil
  :after org
  :defines
  (my-org-agenda-headlines)

  :bind
  ("C-c c c" . org-capture)

  :custom
  ;; dont create a bookmark when calling org-capture
  (org-capture-bookmark nil)
  ;; also don't create bookmark in other things
  (org-bookmark-names-plist nil)
  (org-capture-templates
   `(
     ("t" "Task" entry (file+function "~/d-sync/notes/tasks.org" (lambda () (completing-read "Heading: " my-org-agenda-headlines)))
      "* TODO %?\n  SCHEDULED:%U\n  %a\n  %i" :empty-lines 1)

     ("l" "Link" entry
      (file+headline "~/d-sync/notes/bookmarks.org" "elfeed") "* %a\n")

     ("j" "Journal Entries")

     ("jj" "Journal" entry
      (file+olp+datetree "~/d-sync/notes/journal.org")
      "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
      ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
      :clock-in :clock-resume
      :empty-lines 1)))
  :config
  (setq my-org-agenda-headlines `(projects university tasks one-timer)))

(use-package ol
  :ensure nil
  :after org
  :custom
  (org-link-shell-confirm-function 'y-or-n-p)
  (org-link-elisp-confirm-function 'y-or-n-p))

(use-package org-src
  :ensure nil
  :after org
  :custom
  (org-src-preserve-indentation nil)
  ;; Don't ask if we already have an open Edit buffer
  (org-src-window-setup 'current-window)
  (org-src-ask-before-returning-to-edit-buffer nil)
  (org-edit-src-content-indentation 0)
  :config
  (advice-add 'org-src-get-lang-mode :filter-return
              (lambda (mode)
                (pcase (assoc mode major-mode-remap-alist)
                  (`(,mode . ,ts-mode) ts-mode)
                  (_ mode)))))

(use-package ob-core
  :ensure nil
  :after org
  :custom
  ;; Don't ask every time when I run a code block
  (org-confirm-babel-evaluate nil))

(use-package ox
  :custom
  (org-export-exclude-tags '("noexport" "ignore")))

(use-package ox-latex
  :ensure nil
  :after ox
  :custom
  (org-latex-compiler "lualatex" "Lualatex is fast and gets custom font too"))

(use-package org-re-reveal
  :after ox
  :unless d/on-droid
  :custom
  (org-re-reveal-title-slide
   "<h1 class=\"title\">%t</h1> <br> <br> <h2 class=\"subtitle\">%s</h2> <h2 class=\"author\">%a</h2> <br> <br> <h4 class=\"misc\">%m</h4> <h3 class=\"misc\">%A</h3>"))

(use-package org-habit
  :ensure nil
  :ensure nil
  :custom
  (org-habit-graph-column 45))

(use-package org-indent
  :ensure nil
  :diminish)

(defun browser-bookmarks (org-file)
  "Return all links from ORG-FILE."
  (require 'org-element)
  (require 'seq)
  (with-temp-buffer
    (let (links)
      (insert-file-contents org-file)
      (org-mode)
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (link)
          (let* ((raw-link (org-element-property  :raw-link link))
                 (content (org-element-contents link))
                 (title (substring-no-properties (or (seq-first content) raw-link))))
            (push (concat title
                          "\n"
                          (propertize raw-link 'face 'whitespace-space)
                          "\n")
                  links)))
        nil nil 'link)
      (seq-sort 'string-greaterp links))))

(defun d/open-bookmark ()
  (interactive)
  (browse-url (seq-elt (split-string (completing-read  "Open: " (browser-bookmarks "~/d-sync/notes/bookmarks.org"))  "\n") 1)))

(use-package markdown-mode
  :defer t
  :defines (markdown-mode-map)
  :functions (markdown-view-mode)
  :mode "\\.md\\'"
  :hook (markdown-mode . variable-pitch-mode)
  :bind (:map markdown-mode-map
              ("<f8>" . d/markdown-toggle))
  :config
  (defun d/set-markdown-header-font-sizes ()
    (dolist (face '((markdown-header-face-1 . 1.3)
                    (markdown-header-face-2 . 1.2)
                    (markdown-header-face-3 . 1.15)
                    (markdown-header-face-4 . 1.1)
                    (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :weight 'normal :font haki-heading-font :height (cdr face))))

  (defun d/markdown-mode-hook ()
    (d/set-markdown-header-font-sizes))

  (defun d/markdown-toggle ()
    "Toggle view mode and editing mode"
    (interactive)
    (if (derived-mode-p 'markdown-view-mode) (markdown-mode) (markdown-view-mode))
    (variable-pitch-mode 1)))

(use-package reddigg
  :defer t

  :defines
  (other-subs reddigg-subs )

  :functions
  (-concat reddigg--view-sub)

  :bind (("C-c d f" . reddigg-view-frontpage)
         ("C-c d r" . reddigg-view-sub))
  :custom
  (org-link-elisp-confirm-function 'y-or-n-p)
  (reddigg-subs '(emacs linux nixos hyprland bioinformatics onepiece fossdroid piracy bangalore india indiaspeaks developersindia manga aww))
  :config
  (setq other-subs '(crazyfuckingvideos nextfuckinglevel manga anime animepiracy fossdroid commandline memes jokes funnymemes rss holup unexpected todayilearned lifeprotips askreddit julia)))

(defun reddigg-view-sub ()
  "Prompt SUB and print its post list."
  (interactive)
  (let ((sub (completing-read "subreddit: " (-concat reddigg-subs other-subs '("frontpage" "comments")))))
    (cond ((string= sub "frontpage") (reddigg-view-frontpage))
          ((string= sub "comments") (reddigg-view-comments))
          (t (reddigg--view-sub sub)))))

(defun reddigg--ensure-modes ()
  "Get a bunch of modes up and running."
  (if (equal major-mode 'org-mode)
      (org-set-startup-visibility)
    (org-mode)
    (font-lock-flush))
  (visual-line-mode)
  (jinx-mode -1)
  (view-mode 1))

(use-package hnreader
  :defer t
  :unless d/on-droid)

(use-package howdoyou
  :defer t
  :unless d/on-droid)

(use-package mingus
  :unless d/on-droid
  :commands (d/elfeed-add-podcast)
  :defines
  (mingus-browse-mode-map)

  :bind ("C-c d m" . mingus-browse)
  (:map mingus-browse-mode-map
          ("h" . mingus-browse-top-level)
          ("l" . mingus-down-dir-or-play-song))
  :custom
  (mingus-mode-always-modeline t)
  (mingus-mode-line-string-max 15)
  (mingus-mode-line-show-volume nil)
  (mingus-mode-line-show-elapsed-time nil)
  (mingus-mode-line-show-elapsed-percentage t)
  (mingus-mode-line-show-consume-and-single-status nil))

(use-package webpaste
  :defer t
  :defines
  (webpaste-provider-priority webpaste-paste-confirmation)

  :bind (("C-c C-p C-b" . webpaste-paste-buffer)
         ("C-c C-p C-r" . webpaste-paste-region)
         ("C-c C-p C-p" . webpaste-paste-buffer-or-region))
  :config
  (setq webpaste-provider-priority '("dpaste.org" "dpaste.com" "paste.mozilla.org"))
  ;; Require confirmation before doing paste
  (setq webpaste-paste-confirmation t))

(use-package sdcv
  :defer t
  :unless d/on-droid
  :defines
  (sdcv-say-word-p sdcv-dictionary-data-dir
                   sdcv-dictionary-simple-list
                   sdcv-popup-function sdcv-buffer-name
                   sdcv-mode-map)


  :hook (sdcv-mode . toggle-mode-line)
  :config
  (setq sdcv-say-word-p t
        sdcv-dictionary-data-dir "~/d-git/d-bin/treasure/dict/"
        sdcv-dictionary-simple-list
        '("wn" "mw-thesaurus" "dict")
        sdcv-popup-function 'popup-tip
        sdcv-buffer-name "StarDict")
  :bind (("C-c d w" . sdcv-search-input)
         ("C-c d d" . sdcv-search-input+))
  (:map sdcv-mode-map
        ("q" . kill-buffer-and-window)
        ("n" . sdcv-next-dictionary)
        ("TAB" . hide-entry)
        ("<backtab>" . show-entry)
        ("p" . sdcv-previous-dictionary)))

(use-package proced
  :bind ("C-x x p" . 'proced)
  :init (setq proced-auto-update-interval 1
              proced-enable-color-flag 1
              proced-format 'medium
              proced-sort 'rss)
  :hook (proced-mode . (lambda ()
                         (interactive)
                         (proced-toggle-auto-update 1))))

(use-package speed-type
  :unless d/on-droid
  :hook
  (speed-type-mode . olivetti-mode))

(use-package ement
  :defines
  (ement-room-minibuffer-map
   ement-room-mode-map)
  :functions
  (ement--read-sessions
   ement-connect)

  :bind
  (:map ement-room-minibuffer-map
        ("<f6>" . ement-room-compose-from-minibuffer))
  (:map ement-room-mode-map
        ("M-<" . ement-room-scroll-down-command))
  :custom
  (ement-room-send-message-filter 'ement-room-send-org-filter)
  (ement-room-message-format-spec "%S> %L%B%r%R%t")
  (ement-room-list-avatars nil)
  (ement-save-sessions t)
  :config
  ;; copied from viru (ement github)
  (defun d/ement-connect ()
    (interactive)
    (if (ement--read-sessions)
        (call-interactively #'ement-connect)
      (let* ((found (auth-source-search :max 1
                                        :host "matrix.org"
                                        :port "8448"
                                        :require '(:user :secret)))
             (entry (nth 0 found))
             (password (funcall (plist-get entry :secret)))
             (user (plist-get entry :user)))
        (ement-connect :user-id user :password password)))))

;; access phone storage as default
;; Better is to symlink file to ~/ itself

;;(setq default-directory "/storage/emulated/0/")

(when d/on-droid
(custom-set-variables
 '(touch-screen-precision-scroll t)
 '(touch-screen-display-keyboard t)
 '(browse-url-android-share t))

(defun d/key-droid()
  "To enable touch screen keyboard"
  (interactive)
  (frame-toggle-on-screen-keyboard (selected-frame) nil)
  )
)

;; (setq use-dialog-box nil)

(use-package org-present
  :defer t
  :unless d/on-droid
  :after org
  :bind (:map org-present-mode-keymap
              ("<right>" . d/org-present-next-slide)
              ("<left>" . d/org-present-previous-slide)
              ("<up>" . d/org-present-up)
              ("C-c j" . d/org-present-next-slide)
              ("C-c k" . d/org-present-previous-slide)
              ("C-c h" . d/org-present-up)
              ("<f5>" . d/org-present-refresh))
  (:map org-mode-map
        ("<f8>" . d/org-present-mode))
  :hook ((org-present-mode . d/org-present-enable-hook)
         (org-present-mode-quit . d/org-present-disable-hook))
  :config

  (defun org-present-header-line ()
    ;; (let* ((levelhead (concat "org-level-" (org-current-level)))
    ;;        )
    (setq-local header-line-format (concat "   󰨖 " (propertize (org-get-title) 'face 'org-document-title)  " : " (propertize (if (org-get-heading) (nth 4 (org-heading-components)) " ") 'face 'org-level-1)  "       -         " (propertize "Dilip" 'face 'org-document-info)))

    )

  (defvar d/org-present-org-modern-keyword '(("title"       . "")
                                             ("description" . "")
                                             ("subtitle"    . "")
                                             ("date"        . "")
                                             ("author"      . "")
                                             ("email"       . "")
                                             ("language"    . "")
                                             ("options"     . "")
                                             (t . t)))

  (define-minor-mode d/org-present-mode
    "Toggle Presentation Mode."
    :global nil
    :lighter "d/org-present-mode"
    (if d/org-present-mode
        (org-present)
      (org-present-quit)))

  (defun d/org-present-enable-hook ()
    (setq d/org-present--inhibit-message inhibit-message
          d/org-present--echo-keystrokes echo-keystrokes
          d/org-present--visual-line-mode visual-line-mode
          d/org-present--org-ellipsis org-ellipsis)
    ;; d/org-present--org-indent-mode org-indent-mode)
    ;; (org-indent-mode 1)

    ;; Disable 'org-modern-mode' to setup adjustment if it's installed
    (if (package-installed-p 'org-modern)
        (org-modern-mode 0))

    (if (package-installed-p 'org-modern)
        (setq-local d/org-present--org-modern-hide-stars org-modern-hide-stars
                    d/org-present--org-modern-keyword org-modern-keyword
                    d/org-present--org-modern-block-fringe org-modern-block-fringe

                    org-modern-hide-stars 'leading
                    org-modern-block-fringe t
                    org-modern-keyword d/org-present-org-modern-keyword))

    (display-line-numbers-mode 0)

    (if (package-installed-p 'org-modern)
        (org-modern-mode 1))

    (setq-local inhibit-message t
                echo-keystrokes nil
                cursor-type t
                org-image-actual-width 300
                header-line-format " "
                org-ellipsis "󱞤")

    (dolist (face '((org-block . 1.0)
                    (org-block-begin-line . 0.1)
                    (org-document-info . 1.2)
                    (org-document-title . 1.2)
                    (org-level-1 . 1.2)
                    (org-level-2 . 1.2)
                    (org-level-3 . 1.1)
                    (org-level-4 . 1.1)
                    (org-level-5 . 1.1)
                    (org-level-6 . 1.1)
                    (org-code . 1.15)
                    (header-line . 1.0)
                    (org-verbatim . 1.15)
                    (variable-pitch . 1.1)
                    (org-level-7 . 1.1)))
      (face-remap-add-relative (car face) :height (cdr face)))


    (if (package-installed-p 'hide-mode-line)
        (hide-mode-line-mode 1))
    ;; (org-present-header-line)

    (toggle-mode-line)

    (olivetti-set-width 0.80)

    (org-display-inline-images)
    (read-only-mode 1))

  (defun d/org-present-prepare-slide ()
    (org-overview)
    (org-show-entry)
    (org-show-children))
  ;; (org-present-header-line))

  (defun d/org-present-disable-hook ()
    (setq-local header-line-format nil
                face-remapping-alist '((default variable-pitch default))
                org-adapt-indentation nil
                visual-line-mode d/org-present--visual-line-mode
                org-ellipsis d/org-present--org-ellipsis
                inhibit-message d/org-present--inhibit-message
                echo-keystrokes d/org-present--echo-keystrokes)
    (org-present-small)


    ;; (org-indent-mode d/org-present--org-indent-mode)

    (if (package-installed-p 'hide-mode-line)
        (hide-mode-line-mode 0))
    (toggle-mode-line)
    ;; (load-theme 'haki t)
    (org-mode-restart)
    (org-remove-inline-images))

  (defun d/org-present-up ()
    "Go to higher heading from current heading."
    (interactive)
    (widen)
    (org-up-heading-safe)
    (org-present-narrow)
    (org-present-run-after-navigate-functions))


  (defun d/org-present-next-slide ()
    "Go to next sibling."
    (interactive)
    (widen)
    (unless (org-goto-first-child)
      (org-get-next-sibling))
    (org-present-narrow)
    (org-fold-hide-sublevels 5)
    (org-show-entry)
    (org-present-run-after-navigate-functions))


  (defun d/org-present--last-child ()
    "Find last child of current heading."
    (when (org-goto-sibling) (d/org-present--last-child))
    (when (org-goto-first-child) (d/org-present--last-child)))


  (defun d/org-present-previous-slide ()
    "Go to previous sibling."
    (interactive)
    (widen)
    (when (org-current-level)
      (org-back-to-heading)
      (if (and (org-get-previous-sibling) (org-current-level))
          (when (org-goto-first-child)
            (d/org-present--last-child))))
    (org-present-narrow)
    (org-fold-hide-sublevels 5)
    (org-show-entry)
    (org-present-run-after-navigate-functions))

  (defun d/org-present-refresh ()
    (interactive)
    (d/org-present-mode)
    (d/org-present-mode))

  )

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
