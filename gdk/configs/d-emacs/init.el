(use-package time :ensure nil :init (display-time-mode)
  :custom (display-time-24hr-format t) (display-time-default-load-average nil))

(use-package tramp :ensure nil :custom (tramp-backup-directory-alist backup-directory-alist))

(with-eval-after-load 'tramp
  (unless d/on-droid
    (defun tramp-nspawn--completion-function (&rest _args)
      "List systemd-nspawn containers available for connection.

This function is used by ‘tramp-set-completion-function’, please
see its function help for a description of the format."
      (let* ((raw-list (shell-command-to-string
                                "machinectl list -q"))
             (lines (cdr (split-string raw-list "\n")))
             (first-words (mapcar (lambda (line) (car (split-string line)))
                                  lines))
             (machines (seq-take-while (lambda (name) name) first-words)))
        (mapcar (lambda (m) (list nil m)) machines)))

    ;; todo: check tramp-async-args and tramp-direct-async
    (defun tramp-nspawn--add-method ()
      "Add Tramp method handler for nspawn containers."
      (push `(,"systemd-nspawn-container"
              (tramp-login-program ,"machinectl")
              (tramp-login-args (("shell")
                                 ("-q")
                                 ("--uid" "%u")
                                 ("%h")))
              (tramp-remote-shell "/bin/sh")
              (tramp-remote-shell-login ("-l"))
              (tramp-remote-shell-args ("-i" "-c")))
            tramp-methods))

    (defun tramp-nspawn-setup ()
      "Initialize systemd-nspawn support for Tramp."
      (tramp-nspawn--add-method)
      (tramp-set-completion-function "systemd-nspawn-container"
                                     '((tramp-nspawn--completion-function ""))))

    (add-hook 'after-init-hook 'tramp-nspawn-setup)

    ))

(use-package battery :ensure nil :init (display-battery-mode)
  :custom (battery-load-low '40) (battery-load-critical '29))

;;; early-init.el --- Emacs 27+ pre-initialisation config -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;; Tangled File, no need to edit !!!

;;; Code:

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq frame-inhibit-implied-resize t)

(defconst d/on-droid (eq system-type 'android))

;; Emacs (gui app) is also amazing in android
;; https://sourceforge.net/projects/android-ports-for-gnu-emacs/files/termux/
(when d/on-droid
  (setenv "PATH" (format "%s:%s" "/data/data/com.termux/files/usr/bin"
		                 (getenv "PATH")))
  (setenv "LD_LIBRARY_PATH" (format "%s:%s"
				                    "/data/data/com.termux/files/usr/lib"
				                    (getenv "LD_LIBRARY_PATH")))
  (push "/data/data/com.termux/files/usr/bin" exec-path))

(provide 'early-init)
;;; early-init.el ends here

(use-package window :ensure nil
  :bind
  (("C-z" . nil) ("C-x C-z" . nil) ;; avoid suspend-emacs
   ;; panes
   ("C-x 1" . d/toggle-window-focus)
   ("C-x C-k" . d/kill-buffer)
   ("C-x n n" . d/narrow-or-widen-dwim))

  :custom
  (inhibit-startup-screen t "Don't show splash screen")
  (cursor-type 't)

  (initial-major-mode 'org-mode)
  (initial-scratch-message (format "\n\n"))

  (indent-tabs-mode nil "Spaces!")
  (tab-always-indent 'complete)
  (tab-width 2)
  (reb-re-syntax 'string)

  (window-combination-resize t)
  (history-delete-duplicates t)

  (sentence-end-double-space nil)
  (sentence-end "[.?!,;-]")

  :config
  (delete-selection-mode)
  (global-so-long-mode 1)
  (setopt
   read-process-output-max (* 1024 1024)
   inhihbit-compacting-font-caches t
   pgtk-wait-for-event-timeout nil
   )
  (with-current-buffer "*scratch*" (emacs-lock-mode 'kill))
  (modify-all-frames-parameters
   '((alpha-background . 100)
     (right-divider-width . 1)
     (internal-border-width . 1)))

  ;; balance windows when split (https://zck.org/balance-emacs-windows)
  ;; (seq-doseq (fn (list #'split-window #'delete-window))
  ;;   (advice-add fn :after #'(lambda (&rest args) (balance-windows))))
  )

(defun d/toggle-window-focus()
  "Toggle full view of selected window."
  (interactive)
    (if (window-parent) (delete-other-windows) (winner-undo)))

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
  (when (derived-mode-p 'doc-view-mode) (progn (clear-image-cache) (doc-view-clear-cache)))
  ;; (when (derived-mode-p 'pdf-view-mode) (progn (clear-image-cache) (pdf-cache-clear-data)))
  (if (one-window-p) (kill-this-buffer)
    (kill-buffer-and-window)))

(save-place-mode 1)

(use-package simple :ensure nil
  :bind
  (("M-^" . d/join-every-n-lines)
   ("M-z" . zap-up-to-char)

   ("M-%" . query-replace-regexp)

   ("M-c" . d/flex)
   ("M-l" . downcase-dwim)

   ("M-@" . d/mark-word)
   ("M-h" . d/mark-paragraph))

  :custom
  (kill-ring-max 30000)
  (column-number-mode 1)
  (kill-do-not-save-duplicates t)
  (read-mail-command 'gnus)
  :config
  ;; (global-hl-line-mode 1)
  (global-visual-line-mode 1))

(defvar d/join-lines--last-separator ","
  "Keep the last used separator for `d/join-lines' and
`d/join-every-n-lines', a comma by default.")

(defun d/join-every-n-lines (&optional specify-separator)
  "Join every N lines in the active region by a separator,
by default the last used.

Specify the separator by typing C-u before executing this
command.

Note: it depends on s.el."
  (interactive "P")
  (require 's)
  (unless (region-active-p)
    (error "select a region of all-lines first."))
  (let* ((n (string-to-number (read-string "N =: ")))
         (separator (if (not specify-separator)
                        d/join-lines--last-separator
                      (read-string "Separator: ")))
         (text (buffer-substring-no-properties
                (region-beginning)
                (region-end)))
         (all-lines (split-string text "\n"))
         n-lines
         result)
    (while all-lines
      (let (lines line)
        (dotimes (_ n)
          (when (setq line (pop all-lines))
            (push line lines)))
        (push (reverse lines) n-lines)))
    (setq result (mapconcat (lambda (lines)
                              (s-join separator lines))
                            (reverse n-lines) "\n"))
    (delete-region (region-beginning) (region-end))
    (insert result)
    (setq q/join-lines--last-separator separator)))

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

(use-package display-line-numbers :ensure nil
  :hook (prog-mode)
  :custom (display-line-numbers-type 'relative))

;; credits to
;; https://emacs.dyerdwelling.family/emacs/20231209092556-emacs--redefining-mark-paragraph-and-mark-word/
(defun d/mark-paragraph ()
  "redefinition of mark-paragraph"
  (interactive)
  (forward-char)
  (backward-paragraph)
  (push-mark)
  (forward-paragraph)
  (setq mark-active t))

(defun d/mark-word ()
  "redefinition of mark-word"
  (interactive)
  (if (not (looking-at "\\<"))
      (backward-word))
  (push-mark)
  (forward-word)
  (setq mark-active t))

(use-package files :ensure nil
  :hook (before-save . delete-trailing-whitespace)
  :bind (("<f5>" . d/refresh-buffer))
  :init (auto-save-visited-mode 1)
  :custom
  (save-silently t)
  (confirm-kill-emacs 'yes-or-no-p)
  ;; backup settings
  (backup-by-copying t)
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  (backup-directory-alist
   `((".*" . ,(no-littering-expand-var-file-name "backup/"))))
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (safe-local-variable-directories
   '("/home/idlip/d-sync/notes/" "/home/idlip/d-sync/projects/lnrna-tool/"))
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t)
  (create-lockfiles nil))

(defun d/refresh-buffer ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(use-package undo-fu-session
  :init (undo-fu-session-global-mode)
  :custom (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package vundo
  :bind
  (("C-x u" . vundo)
   ("C-z" . undo-only)
   ("C-S-z" . undo-redo)
   ("C-M-r" . undo-redo))
  :custom
  (vundo-compact-display t)
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-window-max-height 8))

(use-package vc-backup :demand t
  ;; to have auto VC track of files without in git -> C-x v =
  :custom
  (vc-make-backup-files t)
  (vc-follow-symlinks t))

(use-package autorevert :init (global-auto-revert-mode)
  :custom (global-auto-revert-non-file-buffers t))

(use-package savehist :ensure nil
  :init (savehist-mode)
  :custom (history-length 1000)
  (savehist-additional-variables
   '(mark-ring global-mark-ring search-ring regexp-search-ring register-alist extended-command-history)))

(use-package recentf :ensure nil
  :bind ("C-x C-r" . recentf)
  :custom
  (recentf-max-menu-items history-length)
  (recentf-max-saved-items history-length)
  :init (recentf-mode))

(use-package no-littering :demand t :ensure t
  :config
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory)))

(use-package dired :ensure nil
  :init (file-name-shadow-mode 1)
  :hook
  (dired-mode . dired-hide-details-mode)
  (dired-mode . dired-omit-mode)
  :bind
  ((:map dired-mode-map
         ("j" . dired-next-line) ("k" . dired-previous-line)
         ("l" . dired-find-file) ("h" . dired-up-directory)
         ("b" . embark-act) ("e" . dired-do-eww)))

  :custom
  (dired-listing-switches "-agho --group-directories-first")
  (dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'\\|^\\..*$")
  (delete-by-moving-to-trash t)
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t) ;; in case sinlge buffer is preferred
  )

(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(use-package minibuffer :ensure nil
  :hook (minibuffer-setup . cursor-intangible-mode)
  :custom
  (completion-ignore-case t) (completion-auto-select 'second-tab) (completion-auto-help 'visible)
  (completion-show-help nil) (completions-detailed t) (completions-header-format nil)
  (completions-max-height 10) (completions-format 'one-column) (completions-sort 'historical)

  (resize-mini-windows t)                    ; allow resizing of mini-windows
  (enable-recursive-minibuffers t)           ; enable recursive minibuffers
  (read-buffer-completion-ignore-case t)     ; ignore case when reading buffer name
  (read-file-name-completion-ignore-case t)  ; ignore case whn reading file name
  (minibuffer-depth-indicate-mode t)         ; show recursion depth in minibuffer prompt
  (minibuffer-electric-default-mode t)       ; show default value when it's applicable
  (minibuffer-eldef-shorten-default t)       ; shorten "(default ...)" to "[...]" in minibuffer prompts
  (minibuffer-visible-completions t))

(use-package completion-preview :ensure nil :init (global-completion-preview-mode)
  :custom (completion-preview-ignore-case t))

(use-package vertico :init (vertico-mode)
  :bind ((:map vertico-map ("C-v" . vertico-scroll-up) ("M-v" . vertico-scroll-down)))
  :custom (vertico-count 5)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package vertico-multiform :init (vertico-multiform-mode)
  :custom
  (vertico-multiform-commands
   '(
     ;; (jinx-correct reverse)
     (load-theme grid) (consult-theme grid)
     (dired-goto-file flat)
     (consult-recoll buffer)
     (consult-dff unobtrusive)
     (embark-act grid)
     ))

  (vertico-multiform-categories
   '((file  reverse)
     (consult-grep buffer)
     (jinx grid)
     (embark-bindings grid)
     (embark-keybinding grid)
     (buffer flat (vertico-cycle . t)))))

(use-package consult :defer t
  :bind
  (;; C-c bindings (mode-specific-map)
   ("C-c h" . consult-history) ("C-c M-x" . consult-mode-command)
   ("C-c k" . consult-kmacro) ("C-c t t" . consult-theme)
   ;; C-x bindings (ctl-x-map)
   ("C-x M-x" . consult-mode-command)
   ("C-x C-b" . ibuffer) ("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x r b" . consult-bookmark) ("C-x p b" . consult-project-buffer)
   ;; Custom M-# bindings for fast register access
   ("M-#" . consult-register-load) ("M-'" . consult-register-store) ("C-M-#" . consult-register)
   ("M-y" . consult-yank-pop) ;; Other custom bindings
   ;; M-g bindings (goto-map)
   ("M-g e" . consult-compile-error) ("M-g f" . consult-flymake)
   ("M-g g" . consult-goto-line) ("M-g M-g" . consult-goto-line)
   ("M-g o" . consult-outline) ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark) ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi) ("M-g s" . consult-eglot-symbols)
   ;; M-s bindings (search-map)
   ("M-s d" . consult-fd) ("M-s D" . consult-locate)
   ("M-s g" . consult-ripgrep) ("M-s m" . consult-man)
   ("M-s G" . consult-git-grep) ("M-s r" . consult-ripgrep)
   ("M-s i" . consult-info) ("M-s l" . consult-line)
   ;; ("C-s" . consult-line)
   ("M-s L" . consult-line-multi) ("M-s k" . consult-keep-lines) ("M-s u" . consult-focus-lines)
   ;; Isearch integration
   ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history) ("M-s e" . consult-isearch-history)
   ("M-s l" . consult-line) ("M-s L" . consult-line-multi)
   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-s" . consult-history) ("M-r" . consult-history)

   :map org-mode-map
   ("M-g o" . consult-org-heading)
   ("M-g a" . consult-org-agenda))

  :hook (completion-list-mode . consult-preview-at-point-mode)

  :custom
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (imenu-max-item-length nil)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 2.5 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))
  (advice-add #'register-preview :override #'consult-register-window))

(use-package orderless :demand t :custom (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package embark :defer t
  :bind
  (("C-." . embark-act) ("C-;" . embark-act-all)
   ("M-." . embark-dwim) ("C-h B" . embark-bindings)
   (:map embark-identifier-map
         ("d" . sdcv-search-input)
         ("ch" . color-name-to-hex)
         ("cr" . color-name-to-rgb))
   (:map embark-url-map
         ("b" . browse-url-generic)
         ("e" . eww-open-in-new-buffer)
         ("h" . hnreader-comment)
         ("r" . reddigg-view-comments))
   (:map embark-file-map
         ("b" . browse-url-of-dired-file))
   (:map embark-region-map
         ("U" . webpaste-paste-buffer-or-region)))

  :custom
  (prefix-help-command #'embark-prefix-help-command)
  (eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  (embark-prompter 'embark-completing-read-prompter)
  (embark-indicators '(embark-minimal-indicator embark-highlight-indicator embark-isearch-highlight-indicator))

  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  )

(use-package embark-consult :defer t :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia :init (marginalia-mode))

(use-package corfu :init (global-corfu-mode)
  :config (corfu-history-mode) (corfu-echo-mode) (corfu-popupinfo-mode)
  (eldoc-add-command #'corfu-insert))

(use-package cape :after corfu
  :bind ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-history)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-abbrev))

(use-package tempel :hook (prog-mode . tempel-abbrev-mode)
  :bind (("M-+" . tempel-complete) ("M-*" . tempel-insert)))

(use-package tempel-collection :after tempel)

(use-package tab-bar :unless d/on-droid
  :custom ;; tab-bar-format-history
  (tab-bar-format '(tab-bar-separator tab-bar-format-menu-bar tab-bar-format-tabs tab-bar-format-align-right
                                      ;; tab-bar-format-global ;; An issue when used in terminal (cursor wont move properly)
                                      ))
  (tab-bar-close-button-show nil)
  ;; (tab-bar-show nil)
  (tab-bar-mode 1)
  (tab-bar-history-mode 1)
  ;; (global-tab-line-mode 1)
  )

(use-package mwheel :ensure nil
  :custom (mouse-autoselect-window t) )

(use-package xt-mouse
  :init (xterm-mouse-mode))

(use-package winner :init (winner-mode))

(use-package pixel-scroll :ensure nil
  :bind (("C-v" . pixel-scroll-interpolate-down) ("M-v" . pixel-scroll-interpolate-up))
  :init (pixel-scroll-precision-mode 1)
  :custom (scroll-step 1) (scroll-margin 0)
  ;; (pixel-scroll-precision-interpolate-page t)
  (pixel-scroll-precision-large-scroll-height 40.0)
  (mouse-wheel-progressive-speed nil) (mouse-wheel-scroll-amount '(1 ((control) . 1)  ((shift) . 2) ((meta) . 3)))
  (scroll-conservatively 101 "Dont jump") (scroll-preserve-screen-position 1 "Preserve position"))

(use-package repeat :config (repeat-mode 1)
  :custom (repeat-exit-timeout 2))

(use-package activities :unless d/on-droid
  :init (activities-mode) (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)
  :bind
  (("C-x C-a C-n" . activities-new)
   ("C-x C-a C-d" . activities-define)
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ("C-x C-a C-b" . activities-switch-buffer)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a b" . activities-switch-buffer)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a l" . activities-list)))

(use-package gptel :unless d/on-droid :defer t
  :custom (gptel-model 'llama3.2:latest)
  :config
  (setq gptel-backend
        (gptel-make-ollama "Ollama"
          :host "localhost:11434"
          :stream t
          :models '(llama3.2:latest)))
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '(mistral:latest))
  ;; some json error until next update ;; also make it lazy to not ask auth pass
  (gptel-make-gemini "Gemini"
    :key (gptel-api-key-from-auth-source "api.gemini.com" "apikey")
    :stream t)
  )

(use-package zone :ensure nil :demand t :config (zone-when-idle (* 60 5)))

(use-package helpful :hook (helpful-mode . d/toggle-bar)
  :bind (("C-h f" . helpful-callable) ("C-h v" . helpful-variable) ("C-h k" . helpful-key)
         ("C-h x" . helpful-command) ("C-c C-d" . helpful-at-point) ("C-h o" . helpful-symbol)
         ("C-h F" . helpful-function)))

(use-package magit :defer t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-refine-hunk t)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function #'magit-restore-window-configuration))

(use-package ediff :ensure nil
  :custom (ediff-window-setup-function 'ediff-setup-windows-plain "Do actions from single frame"))

(use-package envrc :defer 2
  :config (envrc-global-mode 1)
  (advice-add 'org-babel-eval :around #'envrc-propagate-environment))

(use-package esh-mode :ensure nil
  :hook
  (eshell-mode . (lambda () (setq outline-regexp eshell-prompt-regexp)
                   (setq-local corfu-auto nil)
                   (setq-local scroll-margin 0)
                   (corfu-mode 1)))
  :bind (
         ("<f12>" . d/eshell-toggle)
         ("C-c d s" . project-eshell)
         (:map eshell-mode-map
               ("C-S-l" . d/clear-eshell)))

  :custom
  (eshell-hist-ignoredups t)
  (eshell-kill-processes-on-exit 'ask)
  (eshell-aliases-file (expand-file-name "eshell/alias" user-emacs-directory))

  (eshell-prompt-function
   (lambda nil
     (concat
      "\n"
      ;; (propertize "  " 'face '(:inherit region))
      "  "
      " "
      (propertize (eshell/pwd) 'face '(:foreground "lightblue1"))
      (when (with-eval-after-load 'vc-git (propertize (if (vc-git--current-branch) (concat "   " (vc-git--current-branch)) "") 'face '(:foreground "orangered1"))))
      (when (with-eval-after-load 'envrc (propertize (if (string= envrc--status 'none) "" "   ") 'face '(:foreground "mediumspringgreen"))))
      ;; (propertize (concat "   " (format-time-string "%H:%M" (current-time))) 'face '(:foreground "lightcyan1"))
      (propertize "\n 󰘧 " 'face '(:foreground "palegreen"))
      )))
  (eshell-prompt-regexp " 󰘧 "))

(defun d/clear-eshell ()
  (interactive)
  (eshell-send-input (eshell/clear 1)))

(defun d/term-toggle (term)
  "Minimal hack to toggle eshell."
  (interactive)
  (let* ((terminal-mode (if (string= term 'eshell) 'eshell-mode 'eat-mode))
         (project-terminal (if (string= term 'eshell) #'project-eshell #'project-eat)))
    (cond
     ((derived-mode-p terminal-mode) (if (one-window-p) (switch-to-prev-buffer) (delete-window)))
     ((one-window-p) (progn (select-window (split-window-below)) (shrink-window 7) (funcall project-terminal)))
     (t (progn (other-window 1)
               (if (derived-mode-p terminal-mode) (delete-window)
                 (progn (other-window -1) (select-window (split-window-below)) (shrink-window 7) (funcall project-terminal))))))))

(defun d/eshell-toggle () (interactive) (d/term-toggle 'eshell))
(defun d/eat-toggle () (interactive) (d/term-toggle 'eat))

(use-package em-hist :ensure nil
  :bind ((:map eshell-hist-mode-map
               ("M-s" . nil)
               ("M-s r" . consult-ripgrep)
               ("M-s s" . consult-history)))
  :custom
  (eshell-buffer-maximum-lines 10000) (eshell-history-size 10000))

(add-to-list
 'display-buffer-alist
 '("\\*\\(shell\\|.*term\\|.*eshell\\|.*eat\\|help\\|compilation\\|Async Shell Command\\|Occur\\|xref\\).*\\*"
   (display-buffer-reuse-window display-buffer-in-side-window)
   (side . bottom)
   (slot . 0)
   (post-command-select-window . t)
   (window-height . 0.3)))

(use-package eat :unless d/on-droid
  :hook (eshell-load . eat-eshell-mode)
  :bind
  (("C-c d e" . d/eat-toggle)
   ("S-<f12>" . d/eat-toggle)
   (:map eat-mode-map
         ("C-x C-q" . d/eat-read-write)
         ("<f12>" . d/eat-toggle)
         ("<f9>" . d/toggle-bar))
   (:map eat-semi-char-mode-map
         ("M-o" . nil)
         ("M-s" . nil))))

(defun d/eat-read-write ()
  (interactive)
  (if eat--semi-char-mode (eat-emacs-mode) (eat-semi-char-mode)) )

(use-package comint
  :bind ("M-g r" . d/comint-page-output)
  :custom (comint-pager "cat")
  :config (setenv "MANPAGER" "cat"))

(defun d/comint-page-output ()
  "Get the comint output pager in temporary buffer."
  (interactive)
  (let ((buf (message "*%s: %s*" mode-name (comint-previous-input-string 0))))
    (unless (get-buffer buf)
      (let ((proc (get-buffer-process (current-buffer)))
            (replacement nil)
            (inhibit-read-only t))
        (save-excursion
          (let ((pmark (progn (goto-char (process-mark proc))
                              (forward-line 0)
                              (point-marker))))
            (let ((contents (buffer-substring comint-last-input-end pmark)))
              (with-current-buffer (get-buffer-create buf)
                (insert contents)
                (view-mode)))
            (delete-region comint-last-input-end pmark)
            (goto-char (process-mark proc))
            (setq replacement (concat "*** output flushed ***\n"
                                      (buffer-substring pmark (point))))
            (delete-region pmark (point))))
        ;; Output message and put back prompt
        (comint-output-filter proc replacement)))
    (pop-to-buffer buf)))

(use-package python :ensure nil
  :hook ((python-mode . (lambda ()
                          (setq-local completion-at-point-functions
                                      '(cape-file python-completion-at-point cape-dabbrev)
                                      devdocs-browser-active-docs
                                      '("Python")))))
  :bind (:map python-mode-map ("C-c C-d" . devdocs-browser-open))
  :custom
  ;; (python-forward-sexp-function nil)
  (python-indent-guess-indent-offset-verbose nil))

(use-package ess :defer t :unless d/on-droid
  :custom
  (ess-use-company nil)
  (ess-ask-for-ess-directory t)
  (ess-style 'RStudio)
  (ess-eldoc-show-on-symbol t)

  (ess-R-font-lock-keywords
   '((ess-R-fl-keyword:keywords . t) (ess-R-fl-keyword:constants . t)
     (ess-R-fl-keyword:modifiers . t) (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:assign-ops . t) (ess-R-fl-keyword:%op% . t)
     (ess-fl-keyword:fun-calls . t) (ess-fl-keyword:numbers . t)
     (ess-fl-keyword:operators . t) (ess-fl-keyword:delimiters . t)
     (ess-fl-keyword:= . t) (ess-R-fl-keyword:F&T . t)))

  (inferior-R-font-lock-keywords
   '((ess-S-fl-keyword:prompt . t) (ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:constants . t) (ess-R-fl-keyword:modifiers . t)
     (ess-R-fl-keyword:messages . t) (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:assign-ops . t) (ess-fl-keyword:matrix-labels . t)
     (ess-fl-keyword:fun-calls . t) (ess-fl-keyword:numbers . t)
     (ess-fl-keyword:operators . t) (ess-fl-keyword:delimiters . t)
     (ess-fl-keyword:= . t) (ess-R-fl-keyword:F&T . t))))

(use-package ess-r-mode :unless d/on-droid
  ;; :hook (ess-r-mode . (lambda () (flycheck-mode 0)))
  :bind (
         (:map ess-mode-map ("C-;" . ess-insert-assign))
         (:map inferior-ess-r-mode-map ("C-;" . ess-insert-assign)))
  :custom
  (ess-indent-with-fancy-comments nil))

(use-package nix-mode :bind (:map nix-mode-map ("C-c C-e" . nix-eval-line)))

(use-package nix-ts-mode :ensure nix-mode)

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

(use-package nix-drv-mode :ensure nix-mode :mode "\\.drv\\'")

(use-package nix-shell :ensure nix-mode :commands (nix-shell-unpack nix-shell-configure nix-shell-build))

(use-package nix-repl :ensure nix-mode :commands (nix-repl))

(use-package js :ensure nil :mode ("\\.jsx\\'" . js-jsx-mode))

(use-package ess-julia :unless d/on-droid
  :hook (ess-julia-mode . (lambda () (setq-local devdocs-browser-active-docs '("Julia"))))
  :bind (:map ess-julia-mode-map ("C-c C-d" . devdocs-browser-open))
  :custom (inferior-julia-args "--color=yes" "You get color in julia inferior process"))

(use-package executable :ensure nil :hook (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package flycheck :defer t
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (flycheck-idle-change-delay 3)
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-buffer-switch-check-intermediate-buffers t)
  (flycheck-display-errors-delay 0.25))

(use-package reformatter
  :hook
  (python-ts-mode . ruff-format-on-save-mode)
  (nix-mode . alejandra-format-on-save-mode)
  (ess-r-mode . styler-format-on-save-mode)
  (bash-ts-mode . shell-format-on-save-mode)
  ;; (nix-ts-mode . nixfmt-rfc-format-on-save-mode)

  :config
  (reformatter-define ruff-format :program "ruff"
    :args (list "format" "--stdin-filename" input-file "-"))

  (reformatter-define pyblack-format :program "python"
    :args (list "-m" "black" "-"))

  (reformatter-define alejandra-format :program "alejandra" :group 'nix-mode)


  (reformatter-define styler-format :program "Rscript"
    :args (list "--vanilla" "-e" "con <- file(\"stdin\")
out <- styler::style_text(readLines(con))
close(con)
out"))

  (reformatter-define shell-format :program "shfmt" )

  (reformatter-define nixfmt-rfc :program "nixfmt") )

(use-package eglot :defer t :ensure nil :unless d/on-droid
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)

  :config
  (fset #'jsonrpc--log-event #'ignore)
  (add-to-list 'eglot-server-programs '(nix-mode . ("nixd")))
  ;;   (add-to-list 'eglot-server-programs '(bash-ts-mode . ("bash-language-server")))
  ;;   (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman")))
  )

(use-package xref :ensure nil :custom (xref-search-program 'ripgrep) (grep-command "rg -nS --no-heading"))

(use-package compile
  :hook (compilation-filter . ansi-color-compilation-filter)
  :custom
  (compilation-scroll-output t)
  (compilation-auto-jump-to-first-error t)
  (compilation-max-output-line-length nil)
  (compilation-environment '("TERM=xterm-256color")))

(use-package treesit :ensure nil
  :mode
  (("\\.tsx\\'" . tsx-ts-mode)
   ("\\.yaml\\'" . yaml-ts-mode) ("\\.toml\\'" . toml-ts-mode) ("\\.jsonrc\\'" . json-ts-mode) ("\\.json\\'" .  json-ts-mode)
   ("\\.jsx\\'" . tsx-ts-mode)
   ("\\.Dockerfile\\'" . dockerfile-ts-mode)
   ("\\.sh\\'" . bash-ts-mode))

  :custom
  (treesit-font-lock-level 4)
  (standard-indent 2)
  (major-mode-remap-alist
   '((c-mode . c-ts-mode) (c++-mode . c++-ts-mode) (nix-mode . nix-ts-mode)
     (csharp-mode . csharp-ts-mode) (css-mode . css-ts-mode)
     (java-mode . java-ts-mode) (js-mode . js-ts-mode)
     (js-json-mode . json-ts-mode) ;; (org-mode . org-ts-mode) ;; not mature yet
     (python-mode . python-ts-mode) (julia-mode . ess-julia-mode)
     (typescript-mode . typescript-ts-mode) (sh-mode . bash-ts-mode) (shell-script-mode . bash-ts-mode)
     (ruby-mode . ruby-ts-mode) (rust-mode . rust-ts-mode)
     (toml-mode . toml-ts-mode) (yaml-mode . yaml-ts-mode))))

(use-package devdocs-browser
  :bind ("C-c d v" . devdocs-browser-open-in)
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
     ("python" . python-ts-mode)))

  (devdocs-browser-data-directory (expand-file-name "var/devdocs" user-emacs-directory)))

(use-package prog-mode :ensure nil :hook (prog-mode . hs-minor-mode) (prog-mode . outline-minor-mode)
  :custom (tab-width 2))

(use-package elec-pair :ensure nil :init (electric-pair-mode))

(use-package paren :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-highlight-openparen t) (show-paren-context-when-offscreen t))

(use-package colorful-mode :unless d/on-droid
  :config (global-colorful-mode))

(use-package rainbow-delimiters :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package combobulate :after treesit
  :preface (setq combobulate-key-prefix "C-c o")
  :hook
  ((python-ts-mode . combobulate-mode) (css-ts-mode . combobulate-mode)
   (bash-ts-mode . combobulate-mode)))

(use-package doc-view :ensure nil
  :bind ((:map doc-view-mode-map
               ("M-g M-g" . doc-view-goto-page)
               ("<f8>" . doc-view-presentation)
               ("j" . doc-view-next-line-or-next-page)
               ("k" . doc-view-previous-line-or-previous-page)
               ("C-v" . doc-view-scroll-up-or-next-page)
               ("M-v" . doc-view-scroll-down-or-previous-page)
               ("I" . d/doc-view-theme)
               ))
  :hook (doc-view-mode . (lambda () (setq-local pixel-scroll-precision-mode nil)))
  :custom-face (doc-view-svg-face ((t (:background "#000000" :foreground "#ffffff"))))
  :custom
  (doc-view-continuous t)
  (large-file-warning-threshold 700000000)
  (image-cache-eviction-delay 10))

(defun d/doc-view-theme ()
  "Toggle between dark and reading mode in Doc-view buffer."
  (interactive)
  (let ((choice (completing-read "theme Color: " '("black" "reader" "white" "tokyonight") nil t)))
    (cond
     ((string= "black" choice)
      (set-face-attribute 'doc-view-svg-face nil :background "#050505" :foreground "#ffffff"))
     ((string= "reader" choice)
      (set-face-attribute 'doc-view-svg-face nil :background "#edd1b0" :foreground "#000000"))
     ((string= "tokyonight" choice)
      (set-face-attribute 'doc-view-svg-face nil :background "#24283b" :foreground "#fefefe"))
     ((string= "white" choice)
      (set-face-attribute 'doc-view-svg-face nil :background "#fefefe" :foreground "#000000"))))
  (doc-view-next-page) (doc-view-previous-page))

(use-package saveplace-pdf-view :unless d/on-droid :demand t)

(use-package nov :mode ("\\.epub\\'" . nov-mode)
  :hook
  (nov-mode . d/reading-mode)
  (nov-mode . nov-imenu-setup)
  :custom
  (nov-text-width fill-column)
  (nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title))))

(define-minor-mode d/reading-mode
  "The Zen Mind"
  :init-value nil
  (if d/reading-mode
      (progn
        (read-only-mode 1)
        (d/toggle-bar)
        (variable-pitch-mode 1)
        ;; (setq-local line-spacing 0.5)
        ;; (text-scale-increase 1)
        (setq-local tab-bar-show nil) (tab-bar--update-tab-bar-lines)
        (setq-local cursor-type nil)
        (setq-local olivetti-body-width 90) (olivetti-mode 1)
        )

    (progn
      (d/toggle-bar)
      ;; (text-scale-decrease 1)
      (kill-local-variable 'tab-bar-show) (tab-bar--update-tab-bar-lines)
      (kill-local-variable 'cursor-type)
      (kill-local-variable 'olivetti-body-width)
      )
    ))

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

(use-package gnus
  :hook
  (gnus-group-mode . gnus-topic-mode)
  (gnus-summary-mode . turn-on-gnus-mailing-list-mode)
  ;; (gnus-article-mode . d/reading-mode)
  :bind (("C-c d g" . gnus)
         (:map gnus-summary-mode-map
               ("-" . gnus-summary-hide-thread)
               ("+" . gnus-summary-show-thread)
               )
         (:map gnus-article-mode-map
               ("i" . consult-imenu)))
  :custom
  (gnus-home-directory (expand-file-name "feeds/gnews" user-emacs-directory))
  (gnus-directory (expand-file-name "news" gnus-home-directory))
  (gnus-cache-directory (nnheader-concat gnus-directory "cache/"))
  (message-directory (expand-file-name "mail" gnus-home-directory))
  (gnus-startup-file (expand-file-name "newsrc" gnus-home-directory))
  (gnus-message-archive-group '((format-time-string "sent.%Y")))
  (gnus-article-save-directory (expand-file-name "saved" gnus-home-directory))
  (gnus-widen-article-window t)

  (gnus-select-method
   '(nnnil ""))

  (gnus-secondary-select-methods
   '((nntp "feedbase"
           (nntp-open-connection-function nntp-open-tls-stream) ; feedbase does not do STARTTLS (yet?)
           (nntp-port-number 563) (nntp-address "feedbase.org") )
     (nntp "gwene" (nntp-address "news.gwene.org"))
     (nnrss "")
     ))

  ;; refer: https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/gnus-guide-en.org
  (gnus-thread-sort-functions '(gnus-thread-sort-by-score gnus-thread-sort-by-most-recent-date))
  (gnus-use-cache t)
  (gnus-thread-hide-subtree t)
  ;; (gnus-activate-level 2)

  (gnus-auto-center-summary nil)
  (gnus-asynchronous t)

  ;;; credits - https://libreddit.kavin.rocks/r/emacs/comments/1cfv84p/tipps_on_gnus_summary_formatting/ - u/ballfresno
  ;; (gnus-summary-line-format "%1{%U%R%O %4k%} %3{%&user-date;%*%ud│%}%I%(%-16,16f%) %4{%s%}\n")
  (gnus-user-date-format-alist
   '(((gnus-seconds-today) . " %k:%M")
     ((+ (gnus-seconds-today) (* 24 3600)) . " %l %p")
     (604800 . " %a")
     (31536000 . "%e %b")
     (t . " %Y")))

  (gnus-summary-line-format
   (concat
    "%0{%U%R%z%}"
    "%3{│%}" "%1{%d%}" "%3{│%}" ;; date
    "  "
    "%4{%-20,20f%}"               ;; name
    "  "
    "%3{│%}"
    " "
    "%1{%B%}"
    "%s\n"))

  (setopt gnus-group-line-format "%M%S%p%P%5y:%B%(%G%)\n")

  (gnus-topic-line-format "%i[ %(%{%n%}%) -- %g | %A ]%v\n")

      ;;; credits - https://github.com/jbranso/.emacs.d/blob/master/lisp/init-gnus.org
  (gnus-sum-thread-tree-indent "  ")
  (gnus-sum-thread-tree-root "● ")
  (gnus-sum-thread-tree-false-root "◯ ")
  (gnus-sum-thread-tree-single-indent "")
  (gnus-sum-thread-tree-vertical        "│")
  (gnus-sum-thread-tree-leaf-with-other "├─► ")
  (gnus-sum-thread-tree-single-leaf     "╰─► ")

  ;; Yay (seen here: `https://github.com/cofi/dotfiles/blob/master/gnus.el')
  (gnus-cached-mark ?󰃨)
  (gnus-canceled-mark ?󱞐)
  (gnus-del-mark ?󰆴)
  ;; gnus-dormant-mark ?⚐
  (gnus-expirable-mark ?♻)
  (gnus-forwarded-mark ?)
  ;; gnus-killed-mark ?☠
  ;; gnus-process-mark ?⚙
  (gnus-read-mark ?󰑇)
  (gnus-recent-mark ?✩)
  (gnus-replied-mark ?↺)
  (gnus-unread-mark ?)
  ;; gnus-unseen-mark ?★
  ;; gnus-ticked-mark ?⚑

  :config
  (setq nnrss-group-alist '( ("manga" "https://nyaa.si/?page=rss&c=3_1&f=0") ) )
  (gnus-demon-add-handler 'gnus-group-save-newsrc 5 t) ;; minutes
  (gnus-demon-init)
  )

(use-package gnus-srvr
  :bind
  (:map gnus-server-mode-map
        ("q" . quit-window)))

;; (setopt user-mail-address "idlip@protonmail.com" ;; you can mail me to discuss anything on emacs ;)
;;         user-full-name "Dilip")

(use-package gnus
  :unless d/on-droid
  :config
  (add-to-list 'gnus-secondary-select-methods
               '(nnimap "protonmail"
                        (nnimap-stream plain)
                        (nnimap-address "127.0.0.1") ;; hydroxide
                        (nnimap-server-port 1143)))
  (add-to-list 'gnus-secondary-select-methods
               '(nnimap "tilde-green"
                        (nnimap-stream tls)
                        (nnimap-address "imap.tilde.green")
                        (nnimap-server-port 993)))
  )

(use-package smtpmail
  :unless d/on-droid
  :after gnus
  :custom
  (smtpmail-default-smtp-server "127.0.0.1")
  (mail-sources '((imap :server "127.0.0.1"
                        :user "idlip")))
  (smtpmail-smtp-server "127.0.0.1")
  (smtpmail-smtp-service 1025)
  (starttls-use-gnutls t)
  (send-mail-function 'smtpmail-send-it)
  (message-send-mail-function 'smtpmail-send-it)
  (mail-from-style 'angles)
  (smtpmail-debug-info t)
  (smtpmail-debug-verb t))

(unless d/on-droid
  (setopt gnus-init-file "~/d-sync/feeds/gnews/privmail.el"))

(use-package sdcv :defer t :unless d/on-droid
  :bind (("C-c d w" . sdcv-search-input)
         (:map sdcv-mode-map
               ("n" . sdcv-next-dictionary) ("p" . sdcv-previous-dictionary)
               ("TAB" . hide-entry) ("<backtab>" . show-entry))))

(use-package url :ensure nil
  :custom (url-privacy-level 'high) ;; reddit/SO does not like it 'paranoid
  :config (url-setup-privacy-info))

(use-package shr :ensure nil :demand t
  :custom (shr-bullet "⁍ "))

(use-package shr-tag-pre-highlight
  :demand
  :config
  (add-to-list 'shr-external-rendering-functions '(pre . shr-tag-pre-highlight)))
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

(use-package eww :ensure nil :demand t
  :hook
  (eww-mode . variable-pitch-mode)
  (eww-after-render . (lambda () (eww-readable) (setq-local line-spacing '0.4)))
  :custom
  (eww-auto-rename-buffer 'title)
  :config
  (defun d/eww-readable ()
    "Use more opinionated `eww-readable'.

Set width is set to `current-fill-column'.  Adjust size of
images."
    (interactive)
    (let ((shr-width (current-fill-column))
          (shr-max-image-proportion 0.35))
      (eww-readable)))

  (setq d/search-engines
        '(
          ("go google" . "https://google.com/search?q=%s")
          ("ddg duckduckgo" . "https://duckduckgo.com/?q=%s")
          ("yt invidious" . "https://yewtu.be/search?q=%s")
          ))

  (defun d/search-eww (term)
    "Search for a term using an engine."
    (interactive "MTerm: ")
    (let* ((url
            (cdr (assoc (completing-read "Engine: " d/search-engines) d/search-engines))))
      (if (equal url nil) (message "Error: search engine unknown.")
        (eww (format url (url-hexify-string term))))))
  )

(use-package browse-url :ensure nil :unless d/on-droid
  :config ;; browser script
  (setopt browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "d-stuff"
          browse-url-secondary-browser-function 'browse-url-default-browser))

(defun unpackaged/eww-imenu-index ()
  "Return Imenu index for current EWW buffer.
Index includes links and headings."
  (let ((shr-heading-faces '( shr-h1 shr-h2 shr-h3 shr-h4 shr-h5
                              shr-h6 shr-heading)))
    (cl-labels ((range-matching (property predicate)
                  "Return (BEG . END) cons from point where PROPERTY matches PREDICATE.
  PREDICATE is used for `text-property-search-forward', which see."
                  (when-let* ((match (text-property-search-forward property nil predicate))
                             (end (cl-loop
                                   for next-change-pos = (prop-match-end match) then next-change-pos
                                   for next-change-pos = (next-single-property-change next-change-pos property)
                                   when next-change-pos
                                   for end-pos = next-change-pos
                                   while (funcall predicate nil (get-text-property next-change-pos property))
                                   finally return end-pos)))
                    (cons (prop-match-beginning match) end)))
                (shr-heading-p (_ value-of)
                  (cl-typecase value-of
                    (atom (member value-of shr-heading-faces))
                    (list (seq-intersection value-of shr-heading-faces)))))
      (let ((links (save-excursion
                     (goto-char (point-min))
                     (delete-dups
                      (cl-loop for url = (get-text-property (point) 'shr-url)
                               when url collect (cons (format "%s <%s>"
                                                              (button-label (button-at (point)))
                                                              url)
                                                      (point))
                               for pos = (next-single-property-change (point) 'shr-url)
                               while pos do (goto-char pos)))))
            (headings (save-excursion
                        (goto-char (point-min))
                        (cl-loop for (next-beg . next-end) = (range-matching 'face #'shr-heading-p)
                                 while next-beg
                                 for text = (buffer-substring next-beg next-end)
                                 collect (cons text next-beg)
                                 and do (goto-char next-end)))))
        (list (cons "Headings" headings)
              (cons "Links" links))))))

(defun unpackaged/eww-imenu-goto (_label position)
  "Go to POSITION and call `eww-follow-link' if one is there."
  (goto-char position)
  (when (button-at (point))
    (call-interactively #'browse-url-generic)))

(defun unpackaged/eww-imenu-setup ()
  "Setup Imenu in EWW buffers."
  (setq-local imenu-create-index-function #'unpackaged/eww-imenu-index
              imenu-default-goto-function #'unpackaged/eww-imenu-goto))

(add-hook 'eww-mode-hook #'unpackaged/eww-imenu-setup)
(add-hook 'gnus-article-mode-hook #'unpackaged/eww-imenu-setup)

(use-package mpc
  :bind (("C-c d m" . mpc)
         (:map mpc-mode-map
               ("SPC" . mpc-playlist-add)
               ("RET" . mpc-select)
               ("C-k" . mpc-playlist-delete)
               ("m" . mpc-select-dwim)
               ("C-r" . mpc-songs-search)
               ("f" . mpc-ffwd)
               ("b" . mpc-rewind)
               ))
  :custom (mpc-browser-tags '(Title))
  :config
  (defun mpc-select-dwim ()
    (interactive)
    (mpc-select-toggle)
    (next-line)
    ))

(use-package ready-player :unless d/on-droid :demand t
  :custom (ready-player-open-playback-commands '(("mpv" "--audio-display=no" "--input-ipc-server=" "--speed=1.0")))
  :config (ready-player-mode))

(use-package transmission :unless d/on-droid :bind ("C-c d t" . transmission))

(use-package reddigg :defer t
  :bind (("C-c d f" . reddigg-view-frontpage)
         ("C-c d r" . reddigg-view-sub))
  :custom
  (org-link-elisp-confirm-function 'y-or-n-p)
  (reddigg-subs '(emacs linux nixos orgmode hyprland bioinformatics onepiece fossdroid piracy bangalore india indiaspeaks developersindia manga aww))
  :config
  (setq other-subs '(crazyfuckingvideos nextfuckinglevel manga anime animepiracy fossdroid commandline memes jokes funnymemes rss holup unexpected todayilearned lifeprotips askreddit julia))

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
    (view-mode 1)))

(use-package hnreader :defer t :unless d/on-droid)

(use-package howdoyou :defer t :unless d/on-droid)

(use-package webpaste :defer t
  :bind (("C-c C-p C-b" . webpaste-paste-buffer)
         ("C-c C-p C-r" . webpaste-paste-region)
         ("C-c C-p C-p" . webpaste-paste-buffer-or-region))
  :config
  (setq webpaste-provider-priority '("dpaste.org" "dpaste.com" "paste.mozilla.org"))
  ;; Require confirmation before doing paste
  (setq webpaste-paste-confirmation t))

(use-package erc :ensure nil
  :init
  (defun erc-run-irc ()
    "Run ERC and connect to Libera"
    (interactive)
    (let ((nickuser (completing-read "Irc network: " '("tilde-chat" "libera"))))
      (erc-tls :server "znc.tilde.green" :port 6697 :nick "zororg" :user
               (concat "zororg" "/" nickuser))) ;; yes, thats me
    )
  :custom (erc-hide-list '("JOIN" "PART" "QUIT")))

(use-package rcirc :ensure nil
  :custom ;; yes I'm the guy called "zororg"
  (rcirc-default-nick "zororg") (rcirc-default-user-name "zororg") (rcirc-default-full-name "Zororg")
  (rcirc-reconnect-delay 5)
  (rcirc-fill-column 90)
  (rcirc-track-ignore-server-buffer-flag t)
  (rcirc-server-alist '(("irc.libera.chat" :channels ("#systemcrafters") :port 6697 :encryption tls)))
  :config (rcirc-track-minor-mode 1) )

;; access phone storage as default
;; Better is to symlink file to ~/ itself

;;(setq default-directory "/storage/emulated/0/")

(when d/on-droid
  (custom-set-variables
   '(touch-screen-precision-scroll t)
   '(touch-screen-display-keyboard t)
   '(browse-url-android-share t)
   '(touch-screen-enable-hscroll nil "Avoid horizontal scroll that stutters"))

  ;; credits to https://github.com/danijelcamdzic/dotemacs/
  (setq display-buffer-alist
        '((".*" (display-buffer-same-window) (inhibit-same-window . nil))))

  ;; Add tool-bar options for zooming in
  (tool-bar-add-item "zoom-in" 'text-scale-increase
                     'text-scale-increase
                     :help "Zoom In")

  ;; Add tool-bar options for zooming out
  (tool-bar-add-item "zoom-out" 'text-scale-decrease
                     'text-scale-decrease
                     :help "Zoom Out")

  ;; Add tool-bar option for Org Cycle
  (tool-bar-add-item "right-arrow" 'org-cycle
                     'org-cycle
                     :help "Org Cycle")

  ;; Add tool-bar option for Org Ctrl-C Ctrl-C
  (tool-bar-add-item "prev-node" 'org-ctrl-c-ctrl-c
                     'org-ctrl-c-ctrl-c
                     :help "Execute Org Ctrl-C Ctrl-C")

  (defun d/key-droid()
    "To enable touch screen keyboard"
    (interactive)
    (frame-toggle-on-screen-keyboard (selected-frame) nil)
    )
  )
;; (setq use-dialog-box nil)

(defcustom d/font-size (if d/on-droid 170 120)
  "Default font size based on the system.")

;; Dont worry about the font name, I use fork of Iosevka font

;; Set reusable font name variables
(defcustom d/fixed-pitch-font "UbuntuSansMono NF"
  "The font to use for monospaced (fixed width) text.")

(defcustom d/variable-pitch-font "UbuntuSans NF"
  "The font to use for variable-pitch (documents) text.")

(use-package faces :ensure nil
  :custom-face
  (variable-pitch ((t (:family ,d/variable-pitch-font :height 1.1 :weight medium))))
  (fixed-pitch ((t (:family ,d/fixed-pitch-font :weight medium))))
  (default ((t (:family ,d/fixed-pitch-font :height ,d/font-size :weight medium)))))

(use-package font-lock :ensure nil :init (global-font-lock-mode 1))

(use-package haki-theme :demand t
  :load-path "~/.config/emacs/var/theme"
  :custom-face
  ;; (haki-region ((t (:background "#262626" :foreground "#ffffff"))))
  :custom
  ;; (haki-heading-font "Code D Ace")
  ;; (haki-sans-font "Code D Haki")
  ;; (haki-title-font "Code D Ace")
  (haki-bg-oled t)
  (haki-theme-mode-line nil)
  (haki-region)
  ;; (haki-link-font "")
  ;; (haki-code-font "Code D Lip")
  :config (load-theme 'haki t))

(use-package olivetti :defer t :custom (olivetti-body-width 100)
  :hook (org-mode text-mode Info-mode helpful-mode ement-room-mode gnus-group-mode eww-mode
                   gnus-article-mode sdcv-mode nov-mode elfeed-show-mode markdown-mode))

(use-package emacs :ensure nil :custom
  (mode-line-format
   '("%e" "  "
     (:propertize
      ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote))
     mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
     mode-line-format-right-align
     "  "
		 (project-mode-line project-mode-line-format) " " (vc-mode vc-mode) "  " mode-line-modes mode-line-misc-info "  ")))

(global-set-key [f9] #'d/toggle-bar)

(defun d/toggle-bar ()
  "The void space."
  (interactive)
  (setq mode-line-format
        (if (equal mode-line-format nil)
            (default-value 'mode-line-format)))
  ;; (toggle-frame-tab-bar)
  (redraw-display))

(use-package proced
  :bind ("C-x x p" . 'proced)
  :custom
  (proced-enable-color-flag t)
  (proced-format 'medium)
  (proced-sort 'pmem)
  (proced-auto-update-flag t))

;; credit: yorickvP on Github
;; (setq wl-copy-process nil)

(defun wl-copy (text)
  (let ((p (make-process :name "wl-copy"
                         :command '("wl-copy")
                         :connection-type 'pipe)))
    (process-send-string p text)
    (process-send-eof p)))

;; (defun wl-paste ()
;;   (if (and wl-copy-process (process-live-p wl-copy-process))
;;       nil ; should return nil if we're the current paste owner
;;     (shell-command-to-string "wl-paste -n")))

(unless d/on-droid
  (setq interprogram-cut-function 'wl-copy)
  ;; (setq interprogram-paste-function 'wl-paste)
  )

(use-package org :ensure nil :defer t
  :hook
  (org-mode . variable-pitch-mode)
  (org-mode . org-indent-mode)

  :bind (
         ("C-c t i" . d/set-timer)

         (:map org-mode-map
               ("C-x n n" . d/narrow-or-widen-dwim)
               ("C-c l" . org-store-link)
               ("M-n" . org-shiftdown)
               ("M-p" . org-shiftup)
               ))

  :custom
  (org-ellipsis " ")
  (org-use-sub-superscripts '{})
  (org-log-done 'note)
  (org-log-into-drawer t)
  (org-export-exclude-tags '("noexport" "ignore") "excludes these tagged heading from export")
  (org-latex-compiler "lualatex" "Lualatex is fast and gets custom font too")
  (org-link-file-path-type 'relative)
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "STARTED(s!)" "WAITING(w@/!)" "|" "DONE(d!)")
     (sequence "SOMEDAY(o)" "|")
     (sequence "|" "DELEGATED(g@/!)" "CANCELLED(c!)")))
  (org-clock-in-switch-to-state "STARTED")

  (org-refile-targets
   '( (org-default-notes-file :maxlevel . 5)
      (nil :maxlevel . 6)
     ))

  (fill-column 80)
  (org-directory "~/d-sync/notes/")
  (org-default-notes-file (concat org-directory "d-brain.org"))
  (org-src-fontify-natively t)
  (org-pretty-entities t)
  (org-log-reschedule 'note)
  (org-startup-indented t)
  (org-list-allow-alphabetical t)

  (org-fontify-whole-heading-line t)
  (org-fontify-done-headline t)

  (org-fontify-quote-and-verse-blocks t)
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-use-property-inheritance t)
  (org-hierarchical-todo-statistics nil)
  (org-enforce-todo-checkbox-dependencies t)

  (org-enforce-todo-dependencies t)
  (org-track-ordered-property-with-tag t)

  (org-special-ctrl-k t)

  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)

  (org-image-actual-width nil)

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

  (push '("conf-unix" . conf-unix) org-src-lang-modes)
  (defalias 'd/set-timer (symbol-function 'org-timer-set-timer)) )

(use-package org-modern :config (global-org-modern-mode) :hook (org-mode org-agenda-finalize)
  :custom
  ;; (org-modern-fold-stars '(("󰓏" . "▼") ("󰚀" . "▽") ("󰫤" . "⯆") ("󰴈" . "▿") ("󰄄" . "▾")))

  (org-modern-list
   '((?* . "") (?- . "") (?+ . "")))

  (org-modern-checkbox
   '((?X . "✅") (?- . "❌") (?  . "")))

  (org-modern-keyword
   '(("options" . "") ("title" . "")
     ("author" . "󱆀") ("email" . "")
     ("startup" . "") ("property" . "")
     ("date" . "") ("tags" . "")
     ("reveal" . "󰐩") ("latex" . "") ("latex_header" . "")
     ("logbook" . "log")
     ("todo" . "") (t . t)))

  (org-modern-block-name
   '(("src" . "") ("example" . "")
     ("html" . "") ("quote" . ("" ""))
     (t . t)))

  (org-modern-internal-target '("  " t " ")))

(use-package prog-mode
  :custom (prettify-symbols-alist
           '(("LOGBOOK:" . ?) ("END:" . ?󱟀) ("PROPERTIES:" . ?)
             ("REFERENCE:" . ?) ("CITATION" . ?))
           ))

(use-package org-agenda :ensure nil :after org
  :bind (("C-c d a" . org-agenda)
         ("C-c a a" . org-agenda)
         (:map org-agenda-mode-map
               ("C-x C-k" . org-agenda-exit)))

  ;; :hook (org-agenda-finalize . org-agenda-entry-text-mode)
  :custom
  (org-agenda-include-diary t)
  (org-agenda-tags-column org-tags-column)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-window-setup 'only-window)

  (org-agenda-custom-commands
   '(("n" "Next tasks" ((todo "STARTED") (todo "NEXT")))
     ("a" "Agenda and all TODOs" ((agenda "") (alltodo "")))))

  (org-agenda-files
   '("~/d-sync/notes/d-brain.org"
     "~/d-sync/notes/inbox.org"
     "~/d-git/d-site/README.org"
     )))

(use-package org-capture :ensure nil :after org
  :bind ("C-c c" . org-capture)
  :custom
  (org-capture-templates
   `(
     ("c" "Contacts" entry (file "contacts.org")
      "* %(tempel-insert 'contact)")

     ("l" "Link" item
      (file+headline "bookmarks.org" "gnus") "%a\n")

     ("d" "D Second Brain" entry
      (file+olp+datetree "d-brain.org")
      "* %<%H:%M> - %? %^G
:PROPERTIES:
:ID:       %(org-id-new)
:FROM:     %a
:END:
%i"
      ;; :clock-in t :clock-resume t
      :empty-lines 1)

     ("t" "Tasks for the Day" checkitem
      (file+olp+datetree "d-brain.org")
      "[ ] %?\n"
      )

     ("i" "Inbox Rough Notes" entry
      (file "inbox.org")
      "** %?  :fleeting:\n %U\n %i %a\n - ")

     )))

(use-package org-list
  :custom (org-list-demote-modify-bullet '(("+" . "-") ("-" . "+"))))

(use-package org-src :ensure nil :after org
    :bind ((:map org-mode-map ("C-c ;" . d/org-babel-edit)))
    :custom (org-src-window-setup 'current-window)
    :config ;; advice to get treesit modes in org src buffer/blocks
    (advice-add 'org-src-get-lang-mode :filter-return
                (lambda (mode)
                  (pcase (assoc mode major-mode-remap-alist)
                    (`(,mode . ,ts-mode) ts-mode)
                    (_ mode)))))

(defun d/org-babel-edit ()
  "Edit any src block with lsp support by tangling the block and
then setting the org-edit-special buffer-file-name to the
absolute path. Finally load eglot."
  (interactive)
  ;; org-babel-get-src-block-info returns lang, code_src, and header
  ;; params; Use nth 2 to get the params and then retrieve the :tangle
  ;; to get the filename
  (setq d/tangled-file-name (expand-file-name (assoc-default :tangle (nth 2 (org-babel-get-src-block-info)))))

  ;; tangle the src block at point
  (org-babel-tangle '(4)) (org-edit-special)

  ;; Now we should be in the special edit buffer with python-mode. Set
  ;; the buffer-file-name to the tangled file so that pylsp and
  ;; plugins can see an actual file.
  (setq-local buffer-file-name d/tangled-file-name)
  (eglot-ensure) )

(use-package org-id
  :hook (org-insert-heading . org-id-get-create)
  :custom
  (org-id-method 'ts)
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  ;; (org-id-ts-format (string-replace "here" (car (split-string (org-entry-get nil "ITEM") " ")) "%Y%m%dT%H%M%S-here"))
  (org-id-ts-format "%Y%m%dT%H%M%S"))

(use-package ob-core :ensure nil :after org
  :custom ;; Don't ask every time when I run a code block
  (org-confirm-babel-evaluate t)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t) (calc . t)
     (latex . t) (C . t)
     (R . t) (shell . t) (python . t)
     (julia . t))))

(use-package org-re-reveal :after ox :unless d/on-droid
  :custom
  (add-to-list 'org-export-backends 're-reveal)
  (org-re-reveal-title-slide
   "<h1 class=\"title\">%t</h1> <br> <br> <h2 class=\"subtitle\">%s</h2> <br> <h4 class=\"misc\">%m</h4> <h3 class=\"misc\">%A</h3> <br> <h2 class=\"author\">%a</h2>"))

(use-package org-ql :after org
  :bind ((:map org-mode-map
               ("C-c q f" . org-ql-find)
               ("C-c q s" . org-ql-search)
               ("C-c q l" . org-ql-open-link)
               ("C-c q v" . org-ql-view)))
  :config
  (cl-defun org-goto-random-heading (&key (buffers (list (current-buffer)))
                                          regexp)
    (let* ((entries (org-ql-select buffers
                      `(regexp ,regexp)
                      :action '(cons (current-buffer) (point))))
           (entry (seq-random-elt entries)))
      (pop-to-buffer (car entry))
      (goto-char (cdr entry))))

  (defun d/org-random-heading ()
    (interactive)
    (org-goto-random-heading :buffers org-agenda-files :regexp (read-from-minibuffer "Search regexp: "))))

(use-package org-super-agenda :after org
  :hook (org-agenda-mode . org-super-agenda-mode))

(use-package org-super-links :unless d/on-droid :after org
  :bind ((:map org-mode-map
               ("C-c s s" . org-super-links-link)
               ("C-c s l" . org-super-links-store-link)
               ("C-c s C-l" . org-super-links-insert-link)
               ("C-c s d" . org-super-links-quick-insert-drawer-link)
               ("C-c s i" . org-super-links-quick-insert-inline-link)
               ("C-c s C-d" . org-super-links-delete-link)))
  :config
  (setopt org-super-links-related-into-drawer "REFERENCE"
          org-super-links-link-prefix 'org-super-links-link-prefix-timestamp
          org-super-links-backlink-into-drawer "CITATION"))

(use-package org-fold :after org
  :custom
  (org-fold-show-context-detail
   '((agenda . local) (tags-tree . local) (bookmark-jump . lineage)
     (isearch . lineage) (default . ancestors)))
  ;; (org-fold-core-style 'overlays)
  )

(use-package appt :ensure nil :demand t
  :custom
  (appt-disp-window-function #'appt-org-notify)
  (appt-message-warning-time (* 3 30))
  :config
  (define-advice appt-activate (:after (&optional _arg) hold-your-horses)
    "`appt-activate' is too eager, rein it in."
    (remove-hook 'write-file-functions #'appt-update-list)
    (when (timerp appt-timer)
      (timer-set-time appt-timer (current-time) 600)))

  (define-advice appt-check (:before (&optional _force) from-org-agenda)
    "Read events from Org agenda if possible."
    (and (featurep 'org-agenda)
         (ignore-errors
           (let ((inhibit-message t))
             (org-agenda-to-appt t))))))

(use-package notifications :ensure nil :config
  (defun appt-org-notify (remaining new-time msg)
    (let ((notif (if d/on-droid 'android-notifications-notify 'notifications-notify)))
      (funcall notif
               :body (format "In %s minutes" remaining)
               :title msg
               :urgency 'critical))))

(use-package dslide :after org
  :bind ((:map org-mode-map
               ([f5] . dslide-deck-start))
         (:map dslide-mode-map
               ("n" . dslide-deck-forward)
               ("p" . dslide-deck-backward)
               )))

(use-package ox :after org
  :custom (org-export-backends '(org odt md man latex icalendar html ascii)))

(use-package org-crypt :after org :ensure nil
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt"))
  (setq org-crypt-key nil))

(use-package org-mime :unless d/on-droid :after message
  :hook (message-send . org-mime-confirm-when-no-multipart)
  :bind (:map message-mode-map ("C-c M-o" . org-mime-htmlize))
  :custom
  (org-mime-export-options
   '( :section-numbers nil
      :with-author nil
      :with-toc nil))
  )

(use-package remember :ensure nil
  :bind ("C-c r r" . remember) ("C-c r n" . remember-notes))

(use-package calendar
  :bind ("C-c d d" . calendar)
  :custom (diary-file "~/d-sync/notes/diary")
  (calendar-date-style 'european))

(use-package markdown-mode :defer t
  :mode "\\.md\\'" "\\.Rmd\\'"
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

(use-package jinx :unless d/on-droid
  ;; :init (global-jinx-mode)
  :hook org-mode
  :bind ("M-$". jinx-correct))

(use-package ispell :demand t
  :custom (ispell-alternate-dictionary (expand-file-name "~/.config/enchant/en_US.dic")))

(use-package flymake-languagetool :unless d/on-droid
  :disabled
  :hook (text-mode . flymake-languagetool-load)
  :custom (flymake-languagetool-server-command '("languagetool-http-server")))
