;; -*- lexical-binding: t; -*-

(setopt
 display-time-24hr-format t
 display-time-default-load-average nil
 display-time-format "%H:%M")
(display-time-mode 1)

(setq tramp-backup-directory-alist backup-directory-alist)

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

(setopt
 battery-load-low '40
 battery-load-critical '29
 battery-mode-line-format "⬩ %b%p%%")
(display-battery-mode 1)

(bind-keys :package emacs ("C-z") ("C-x C-z") ("M-o" . other-window)
           ("M-j" . duplicate-dwim) )

(setopt
 inhibit-startup-screen t

 initial-scratch-message (format "\n\n")

 indent-tabs-mode nil
 tab-always-indent 'complete
 tab-width 4
 reb-re-syntax 'string
 fill-column 80

 window-combination-resize t
 history-delete-duplicates t

 sentence-end-double-space nil
 sentence-end "[.?!,;-]"
 read-process-output-max (* 1024 1024)
 inhihbit-compacting-font-caches t
 pgtk-wait-for-event-timeout nil
 )

(delete-selection-mode 1) (blink-cursor-mode -1)
(global-so-long-mode 1)
(with-current-buffer "*scratch*" (emacs-lock-mode 'kill))
(modify-all-frames-parameters
 '((alpha-background . 100) ;; blur
   (right-divider-width . 1) (internal-border-width . 20)
   (left-fringe . 1) (right-fringe . 0)
   ))

(defun d/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(global-set-key (kbd "C-g") #'d/keyboard-quit-dwim)

(save-place-mode 1)

(setopt
 kill-ring-max 30000
 kill-do-not-save-duplicates t
 read-mail-command 'gnus
 set-mark-command-repeat-pop t
 use-dialog-box nil
 use-file-dialog nil
 use-short-answers t
 async-shell-command-buffer 'new-buffer
 async-shell-command-display-buffer nil
 grep-use-headings t
 save-interprogram-paste-before-kill t
 )
(put 'suspend-frame 'disabled t)
(global-hl-line-mode 1)
(global-visual-line-mode 1)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setopt display-line-numbers-type 'relative)

(auto-save-visited-mode 1)

(setopt
 save-silently t
 confirm-kill-emacs 'yes-or-no-p
 ;; backup settings
 make-backup-files nil
 view-read-only t
 truncate-lines t
 custom-file (expand-file-name "custom.el" user-emacs-directory)
 backup-directory-alist
 `((".*" . ,(no-littering-expand-var-file-name "backup/")))
 auto-save-file-name-transforms
 `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
 safe-local-variable-directories
 '("/home/idlip/d-sync/notes/" "/home/idlip/d-sync/projects/lnrna-tool/")
 remote-file-name-inhibit-delete-by-moving-to-trash t
 remote-file-name-inhibit-auto-save t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 create-lockfiles nil
 )

(use-package undo-fu-session
  :init (undo-fu-session-global-mode)
  :custom (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package vundo
  :bind
  (("C-x u" . vundo) ("C-z" . undo-only) ("C-S-z" . undo-redo) ("C-M-r" . undo-redo))
  :custom
  (vundo-compact-display t)
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-window-max-height 8))

(global-auto-revert-mode 1)
(setopt global-auto-revert-non-file-buffers t)

(savehist-mode 1)
(setopt
 history-length 200
 save-place-limit nil
 savehist-additional-variables
 '(kill-ring command-history
             set-variable-value-history custom-variable-history
             query-replace-history read-expression-history
             minibuffer-history read-char-history face-name-history
             bookmark-history file-name-history
             mark-ring global-mark-ring search-ring regexp-search-ring register-alist extended-command-history)
 )

(global-set-key (kbd "C-x C-r") #'consult-recent-file)
(setopt recentf-max-menu-items 1000
        recentf-max-saved-items 1000)
(recentf-mode 1)

(use-package no-littering :demand t :ensure t
  :config
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory)))

(file-name-shadow-mode 1)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'dired-omit-mode)

(setopt
 dired-listing-switches "-agho --group-directories-first"
 dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'\\|^\\..*$"
 dired-guess-shell-alist-user
 '(("\\.\\(png\\|jpe?g\\|tiff\\)" "swayimg")
   ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv")
   (".pdf$" "sioyek")
   (".*" "xdg-open" "open"))
 delete-by-moving-to-trash t
 dired-dwim-target t
 dired-kill-when-opening-new-dired-buffer t ;; in case sinlge buffer is preferred
 )

(setopt wdired-allow-to-change-permissions t
        wdired-create-parent-directories t)

(set-language-environment 'utf-8)
(setopt locale-coding-system 'utf-8)
(setopt buffer-file-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(setopt
 completion-ignore-case t
 completion-auto-select 'second-tab
 completion-auto-help t
 completions-detailed t
 enable-recursive-minibuffers t
 completion-show-help nil
 completions-max-height 10
 completions-sort 'historical
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t
 minibuffer-depth-indicate-mode t
 minibuffer-electric-default-mode t
 minibuffer-visible-completions t
 completions-group t
 completions-format 'vertical
 )

(define-abbrev-table 'internet-terms-abbrev-table
  '(
    ("AFAICT" "As far as I can tell" nil :count 0)
    ("IMNSHO" "In my not so humble opinion" nil :count 0)
    ("BTW" "By the way" nil :count 0)
    ("FYI" "For your information" nil :count 0)
    ("IMO" "In my opinion" nil :count 0)
    ("IMHO" "In my humble opinion" nil :count 0)
    ("YMMV" "Your mileage may vary" nil :count 0)
    ("TL;DR" "Too long; didn't read" nil :count 0)
    ("IIRC" "If I recall correctly" nil :count 0)
    ("FWIW" "For what it's worth" nil :count 0)
    ("HTH" "Hope this helps" nil :count 0)
    ("LMK" "Let me know" nil :count 0)
    ("NP" "No problem" nil :count 0)
    ("POV" "Point of view" nil :count 0)
    ("OTOH" "On the other hand" nil :count 0)
    ("TBH" "To be honest" nil :count 0)
    ("WIP" "Work in progress" nil :count 0)
    ("WRT" "With respect to" nil :count 0)
    ("WFM" "Works for me" nil :count 0)
    ("ICYMI" "In case you missed it" nil :count 0)
    ("ETA" "Estimated time of arrival" nil :count 0)
    ("AKA" "Also known as" nil :count 0)
    ("ASAP" "As soon as possible" nil :count 0)
    ("IOW" "In other words" nil :count 0)
    ("NBD" "No big deal" nil :count 0)
    ("PFA" "Please find attached" nil :count 0)
    ("TIA" "Thanks in advance" nil :count 0)))

(keymap-global-set "<remap> <dabbrev-expand>" 'hippie-expand)
(setopt hippie-expand-verbose t
        hippie-expand-dabbrev-skip-space t)

(use-package vertico :init (vertico-mode)
  :bind ((:map vertico-map ("C-v" . vertico-scroll-up) ("M-v" . vertico-scroll-down)
        ("C-<return>" . vertico-really-exit-input) ("DEL" . vertico-directory-delete-char)
               ))
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

(use-package vertico-directory :after vertico :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

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
   ("M-g g" . consult-goto-line)
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
   ("M-s a" . consult-org-agenda)
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
  (consult-ripgrep-args "rga --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number")
  :config
  (setopt completion-in-region-function #'consult-completion-in-region)
  (consult-customize
   consult-theme :preview-key '(:debounce 2.5 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  (advice-add #'register-preview :override #'consult-register-window))

(setopt completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))
        completion-category-defaults nil)

(use-package embark :defer t
  :bind
  (("C-." . embark-act) ("C-;" . embark-act-all)
   ("M-." . embark-dwim) ("C-h B" . embark-bindings)
   (:map embark-identifier-map
         ("d" . quick-sdcv-search-input)
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
  ;; (embark-prompter 'embark-completing-read-prompter)
  ;; (embark-indicators '(embark-minimal-indicator embark-highlight-indicator embark-isearch-highlight-indicator))

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

(use-package cape
  :bind ("C-c p" . cape-prefix-map)
  ("M-i" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-history)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-abbrev))

(use-package tempel :hook (prog-mode . tempel-abbrev-mode)
  :bind (("M-+" . tempel-complete) ("M-*" . tempel-insert)))

(unless d/on-droid
  (tab-bar-mode 1) (tab-bar-history-mode 1))
(setopt
 tab-bar-format
 '(tab-bar-separator tab-bar-format-menu-bar tab-bar-format-tabs-groups tab-bar-separator
                     ;; tab-bar-format-tabs
                     tab-bar-format-align-right
                     ;; tab-bar-format-global ;; An issue when used in terminal+daemon (cursor wont move properly)
                     )
 tab-bar-close-button-show nil)

(use-package ibuffer :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :hook (ibuffer-mode . ibuffer-set-filter-groups-by-mode))

(setopt uniquify-buffer-name-style 'forward)

(setopt isearch-lazy-count t
        search-whitespace-regexp ".*?")

(context-menu-mode 1)
(setopt mouse-autoselect-window t)

(unless (display-graphic-p)
  (xterm-mouse-mode 1))

(winner-mode 1)

(setopt
 scroll-step 3
 scroll-margin 0
 mouse-wheel-progressive-speed nil
 mouse-wheel-scroll-amount '(1 ((control) . 1)  ((shift) . 2) ((meta) . 3))
 scroll-conservatively 101
 scroll-preserve-screen-position t
 pixel-scroll-precision-interpolate-page t)

(pixel-scroll-precision-mode 1)

;; (bind-keys ("C-v" . View-scroll-half-page-forward) ("M-v" . View-scroll-half-page-backward))
(bind-keys ("C-v" . pixel-scroll-interpolate-down) ("M-v" . pixel-scroll-interpolate-up))

(setq repeat-exit-timeout 2)
(put 'other-window 'repeat-map nil)
(repeat-mode 1)

(use-package gptel :unless d/on-droid :defer t
  :custom (gptel-model 'gemma3:1b)
  (gptel-default-mode 'org-mode)
  :config
  (setopt gptel-backend
        (gptel-make-ollama "Gemma"
                           :host "localhost:11434"
                           :stream t
                           :models '(gemma3:1b qwen2.5:1.5b gemma3:latest )))
;; deepseek-r1:latest
  ;; some json error until next update ;; also make it lazy to not ask auth pass
  ;; (gptel-make-gemini "Gemini"
  ;;                    :key (gptel-api-key-from-auth-source "api.gemini.com" "apikey")
  ;;                    :stream t)
  )

(with-eval-after-load 'zone
  (zone-when-idle (* 60 5)))

(define-minor-mode d/write-mode
  "write comment outside emacs."
  :keymap `((,(kbd "C-c C-c") . d/write-done)))

(defcustom d/write-dir "/tmp/emacs-write"
  "Directory path for writing files.")

(defun d/write-here ()
  "Start writing outside emacs, but still using emacs."
  (files--ensure-directory d/write-dir)
  (with-current-buffer
      (find-file (format "%s/%s.org" d/write-dir (format-time-string "%Y%m%dT%H%M%S")))
    (d/write-mode)
    (insert (shell-command-to-string "wl-paste -p"))
    (make-frame '((name . "d-write") (width . 70) (height . 20)))
    ))

(defun d/write-done (&optional arg)
  "Copy current buffer to kill-ring."
  (interactive "P")
  (let* ((raw-text (buffer-substring-no-properties (point-min) (point-max)))
         ;; No idea why the 3rd argument of `org-export-with-toc' doesn't work for this.
         (org-export-with-toc nil)
         (text (pcase arg
                 (4  (org-export-string-as raw-text 'md t))
                 (_  raw-text))))
    (start-process "ee-write" nil "wtype" "-s" "2000" text)
    ;; (kill-new text)
    (save-buffer) (kill-buffer)
    (delete-frame)
    ))

(use-package info :ensure nil
  :config
  (add-to-list 'Info-additional-directory-list "~/learn/info-manuals/"))

(use-package magit :defer t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-refine-hunk t)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function #'magit-restore-window-configuration))

(use-package ediff :ensure nil
  :custom (ediff-window-setup-function 'ediff-setup-windows-plain "Do actions from single frame"))

(use-package diff-mode :ensure nil
  :custom
  (diff-default-read-only t)
  (diff-font-lock-syntax 'hunk-also)
  (diff-font-lock-prettify nil))

(use-package envrc :defer 2
  :config (envrc-global-mode 1)
  (advice-add 'org-babel-eval :around #'envrc-propagate-environment))

(use-package esh-mode :ensure nil
  :hook
  (eshell-mode . (lambda () (setq outline-regexp eshell-prompt-regexp)))

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
 '("\\*\\(shell\\|term\\|.*eshell\\|.*eat\\|help\\|compilation\\|Async Shell Command\\|Occur\\|xref\\).*\\*"
   (display-buffer-reuse-window display-buffer-in-side-window)
   (side . bottom)
   (slot . 0)
   (post-command-select-window . t)
   (window-height . 0.3)))

(add-to-list
 'display-buffer-alist
 '("\\*\\(Ibuffer\\)\\*"
   (display-buffer-in-side-window)
   (window-width . 100)
   (side . right)
   (slot . 1)))

(setq comint-pager "cat")
(setenv "MANPAGER" "cat")

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

(unless d/on-droid
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
)

(use-package js :ensure nil :mode ("\\.jsx\\'" . js-jsx-mode))

(use-package ess-julia :unless d/on-droid
  :hook (ess-julia-mode . (lambda () (setq-local devdocs-browser-active-docs '("Julia"))))
  :bind (:map ess-julia-mode-map ("C-c C-d" . devdocs-browser-open))
  :custom (inferior-julia-args "--color=yes" "You get color in julia inferior process"))

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(add-hook 'prog-mode-hook #'flymake-mode)
(setq python-flymake-command '("ruff" "--quiet" "--stdin-filename=stdin" "-"))

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

(setopt xref-search-program 'ripgrep
        grep-command "rg ")

(use-package compile
  :hook (compilation-filter . ansi-color-compilation-filter)
  :bind ("M-#" . compile) ; M-! M-# M-& M-\
  :custom
  (shell-command-switch "-c") ;; -i
  (compilation-scroll-output t)
  (compilation-auto-jump-to-first-error t)
  ;; (compilation-environment '("TERM=xterm-256color"))
  )

(use-package project :ensure nil
  :custom (project-compilation-buffer-name-function 'project-prefixed-buffer-name))

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
     (java-mode . java-ts-mode) (js-mode . js-ts-mode) (html-mode . html-ts-mode)
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

(use-package hi-lock
  :hook (prog-mode . highlight-marker-mode)
  :config
  (define-minor-mode highlight-marker-mode
    "A minor mode that toggles a chunk of functionality."
    :init-value nil
    (let ((regexps
           `((, (rx (or "FIXME:" "fixme:")) . hi-red-b)   ; fixme
             (, (rx (or "NOTE:" "note:")) . hi-blue)      ; note
             (, (rx (or "TODO:" "todo:")) . hi-green)     ; todo
             (, (rx (group (repeat 8 digit)) "T") . org-date) ; date 20250228T  (denote style)
             (, (rx "T" (group (repeat 6 digit))) . org-modeline-clock)))) ; time T201020

      (if highlight-marker-mode
          (dolist (regexp regexps)
            (highlight-regexp (car regexp) (cdr regexp)) )
        (dolist (regexp regexps)
          (unhighlight-regexp (car regexp)))))
    )
  )

(electric-pair-mode 1)

(use-package paren :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-highlight-openparen t) (show-paren-context-when-offscreen t))

(use-package colorful-mode :unless d/on-droid
  :config (global-colorful-mode))

(use-package rainbow-delimiters :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

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

(unless d/on-droid
  (use-package reader :demand t
    :load-path "~/learn/emacs-reader"
    :config (reader-global-dark-mode 1)
    (require 'reader-saveplace)
    (require 'reader-bookmark)
    (require 'reader-outline)
    ))

(use-package saveplace-pdf-view :unless d/on-droid :demand t)

(use-package nov :mode ("\\.epub\\'" . nov-mode)
  :hook
  (nov-mode . d/reading-mode)
  (nov-mode . nov-imenu-setup)
  :custom
  (nov-text-width t)
  (nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title))))

(define-minor-mode d/reading-mode
  "The Zen Mind"
  :init-value nil
  (if d/reading-mode
      (progn
        (read-only-mode 1)
        (d/toggle-bar-mode 1)
        (variable-pitch-mode 1)
        ;; (d/center-document-mode 1)
        ;; (setq-local line-spacing 0.5)
        ;; (text-scale-increase 1)
        ;; (setq-local tab-bar-show nil) (tab-bar--update-tab-bar-lines)
        (setq-local cursor-type nil)
        )

    (progn
      (d/toggle-bar-mode -1)
      ;; (d/center-document-mode -1)
      ;; (text-scale-decrease 1)
      ;; (kill-local-variable 'tab-bar-show) (tab-bar--update-tab-bar-lines)
      (kill-local-variable 'cursor-type)
      )
    ))

(use-package gnus
  :hook
  (gnus-group-mode . gnus-topic-mode)
  (gnus-summary-mode . turn-on-gnus-mailing-list-mode)
  ;; (gnus-article-mode . d/reading-mode)
  :bind (("C-c d g" . gnus))
  :custom
  (gnus-home-directory (expand-file-name "feeds/gnews" user-emacs-directory))
  (gnus-directory (expand-file-name "news" gnus-home-directory))
  (gnus-cache-directory (nnheader-concat gnus-directory "cache/"))
  (message-directory (expand-file-name "mail" gnus-home-directory))
  (gnus-startup-file (expand-file-name "newsrc" gnus-home-directory))
  (gnus-message-archive-group '((format-time-string "sent.%Y")))
  (gnus-article-save-directory (expand-file-name "saved" gnus-home-directory))
  (gnus-widen-article-window t)
  (gnus-completion-styles completion-styles)

  (gnus-select-method
   '(nnnil ""))

  (gnus-secondary-select-methods
   '((nntp "feedbase"
           (nntp-open-connection-function nntp-open-tls-stream) ; feedbase does not do STARTTLS (yet?)
					 (nntp-connection-timeout 5)
           (nntp-port-number 563) (nntp-address "feedbase.org") )
     ;; (nntp "gwene" (nntp-address "news.gwene.org"))
		 ;;  		 (nntp-open-connection-function nntp-open-network-stream) (nntp-connection-timeout 5) )
     ;; (nntp "news.gmane.io"
		 ;;  		 (nntp-open-connection-function nntp-open-network-stream)
		 ;;  		 (nntp-connection-timeout 5))
	   (nntp "yhetil" (nntp-address "news.yhetil.org"))
     (nnrss "")
     ))

  ;; refer: https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/gnus-guide-en.org
  (gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date gnus-thread-sort-by-score))
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

  (gnus-group-line-format "%M%S%p%P%5y:%B%(%G%)\n")

  (gnus-topic-line-format "%i[ %(%{%n%}%) -- %g | %A ]%v\n")

  (gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)

      ;;; credits - https://github.com/jbranso/.emacs.d/blob/master/lisp/init-gnus.org
  (gnus-sum--tree-indent " ")
  (gnus-sum-thread-tree-indent " ")
  (gnus-sum-thread-tree-false-root "○ ")
  (gnus-sum-thread-tree-single-indent "◎ ")
  (gnus-sum-thread-tree-leaf-with-other "├► ")
  (gnus-sum-thread-tree-root "● ")
  (gnus-sum-thread-tree-single-leaf "╰► ")
  (gnus-sum-thread-tree-vertical "│")


  ;; Yay (seen here: `https://github.com/cofi/dotfiles/blob/master/gnus.el')
  ;; (gnus-cached-mark ?󰃨)
  ;; (gnus-canceled-mark ?󱞐)
  ;; (gnus-del-mark ?󰆴)
  ;; gnus-dormant-mark ?⚐
  ;; (gnus-expirable-mark ?♻)
  ;; (gnus-forwarded-mark ?)
  ;; gnus-killed-mark ?☠
  ;; gnus-process-mark ?⚙
  ;; (gnus-read-mark ?󰑇)
  ;; (gnus-recent-mark ?✩)
  ;; (gnus-replied-mark ?↺)
  ;; (gnus-unread-mark ?)
  ;; gnus-unseen-mark ?★
  ;; gnus-ticked-mark ?⚑

  :config
  (setq nnrss-group-alist '( ("manga" "https://nyaa.si/?page=rss&c=3_1&f=0") ) )
  (gnus-demon-add-handler 'gnus-group-save-newsrc 5 t) ;; minutes
  (gnus-demon-init)
  )

(use-package gnus-group
  :bind
  (:map gnus-group-mode-map
        ("M-g" . goto-map)
        ("M-&") ("M-n") ("M-p") ; Gnus taking over useful keybindings
        ("C-&" . gnus-group-universal-argument)))

(use-package gnus-art
  :bind
  (:map gnus-article-mode-map
        ("C-h b") ; come on Gnus, that key binding is sacred!
        ("M-&")   ; also pretty important
        ("C-&" . gnus-summary-universal-argument)
        ("M-g" . goto-map)
        ("{" . backward-paragraph)
        ("}" . forward-paragraph)))

(use-package gnus-sum
  :bind
  (:map gnus-summary-mode-map
        ("-" . gnus-summary-hide-thread)
        ("+" . gnus-summary-show-thread)
        ("M-&")   ; also pretty important
        ("C-&" . gnus-summary-universal-argument)
        ("M-g" . goto-map) ; rescan is also on Z G, and I use that prefix a lot!
        ("M-a" . gnus-symbolic-argument)))

(use-package gnus-srvr
  :bind
  (:map gnus-server-mode-map
        ("q" . quit-window)))

(setopt user-mail-address "zororg@tilde.green" ;; you can mail me to discuss anything on emacs ;)
        user-full-name "Zororg")

(setq fast-read-process-output nil)
(setq gnus-search-use-imap t)


(use-package gnus :disabled t
  :unless d/on-droid
  :config
  (add-to-list 'gnus-secondary-select-methods
               '(nnimap "protonmail"
                        (nnimap-stream plain)
                        (nnimap-address "127.0.0.1") ;; hydroxide
                        (nnimap-server-port 1143)))
  (add-to-list 'gnus-secondary-select-methods
               '(nnimap "tilde-green"
                        (nnimap-stream plain)
                        (nnimap-address "imap.tilde.green")
          ))
  )

(use-package smtpmail :unless d/on-droid
  ;; :after gnus
  :custom
  (smtpmail-default-smtp-server "smtp.tilde.green")
  (smtpmail-smtp-server "smtp.tilde.green")
  (smtpmail-smtp-service 465)
  (starttls-use-gnutls t)
  (send-function 'smtpmail-send-it)
  (message-send-mail-function 'smtpmail-send-it)
  (mail-from-style 'angles)
  (smtpmail-debug-info t)
  (smtpmail-debug-verb t))

(setopt message-server-alist '(("zororg@tilde.green"
                                . "smtp smtp.tilde.green 465 zororg")))

(unless d/on-droid
  (setopt gnus-init-file "~/d-sync/feeds/gnews/privmail.el"))

(use-package url :ensure nil
  :custom (url-privacy-level 'high) ;; reddit/SO does not like it 'paranoid
  :config (url-setup-privacy-info))

(use-package shr :ensure nil :demand t
  :custom (shr-bullet "⦿ ")
  (shr-max-width fill-column))

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
  (eww-after-render . (lambda () (d/eww-readable) (setq-local line-spacing '0.4)))
  :custom
  (eww-auto-rename-buffer 'title)
  (eww-search-prefix "https://leta.mullvad.net/search?engine=brave&q=")
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
          ("sxng searxng" . "https://searxng.world/search?q=%s")
          ("mg marginalia rss" . "https://marginalia-search.com/search?query=")
          ("feedle rss" . "https://feedle.world/search?query=")
          ("lists gnu mail" . "https://yhetil.org/")
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

(use-package ox-hugo :unless d/on-droid :after ox)

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
                 (file+olp "~/d-git/d-site/org-mode/source.org" "Posts")
                 ;; (function org-hugo-new-subtree-post-capture-template)
                 "* TODO %^{Post Title}
:PROPERTIES:
:EXPORT_FILE_NAME: %^{File name}
:EXPORT_DATE: %(org-timestamp-inactive)
:END:
%?\n\n" )))

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

(use-package transmission :unless d/on-droid :bind ("C-c d t" . transmission))

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

(use-package erc-fill
  :custom ;; Prefer one message per line without continuation indicators.
  (erc-fill-static-center 18))

(use-package rcirc :ensure nil
  :custom ;; yes I'm the guy called "zororg"
  (rcirc-default-nick "zororg") (rcirc-default-user-name "zororg") (rcirc-default-full-name "Zororg")
  (rcirc-reconnect-delay 5)
  (rcirc-fill-column 90)
  (rcirc-track-ignore-server-buffer-flag t)
  (rcirc-server-alist '(("znc.tilde.green" :port 6697 :nick "zororg" :user-name "zororg/libera")))
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
(defcustom d/fixed-pitch-font (if d/on-droid "Maple Mono NF" "BlexMono Nerd Font")
  "The font to use for monospaced (fixed width) text.")

(defcustom d/variable-pitch-font (if d/on-droid "Inter" "IBM Plex Serif")
  "The font to use for variable-pitch (documents) text.")

(use-package faces :ensure nil
  :custom-face
  (variable-pitch ((t (:family ,d/variable-pitch-font :height 1.1 :weight normal))))
  (fixed-pitch ((t (:family ,d/fixed-pitch-font :weight normal))))
  (default ((t (:family ,d/fixed-pitch-font :height ,d/font-size :weight normal)))))

(global-font-lock-mode 1)

(use-package modus-themes :ensure nil :demand t
  :init (require-theme 'modus-themes)
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs nil)
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui nil)
  (modus-themes-custom-auto-reload t)
  (modus-themes-disable-other-themes t)
  (modus-themes-prompts '(italic bold))
  (modus-themes-completions
   '((matches . (extrabold))
     (selection . (semibold italic text-also))))

  (modus-themes-org-blocks 'gray-background)

  (modus-themes-headings
   '((1 . (variable-pitch 1.5))
     (2 . (1.3))
     (agenda-date . (1.3))
     (agenda-structure . (variable-pitch light 1.8))
     (t . (1.1))))


  (modus-vivendi-palette-overrides
   '(
     (bg-main     "#000000")
     (bg-dim      "#111111")
     (bg-active   "#222222")
     (bg-inactive "#333333")

     (fg-main     "#ffffff")
     (fg-dim      "#b4aeae")

     (cursor      "#00ffff")
     (warning     "#fafad2")

     (fg-heading-1  "#ab82ff")
     (fg-heading-2  "#fab387")
     (mail-subject  "#6ae4b9")

     (bg-completion "#2e8b57")
     (bg-region     bg-completion)
     (fg-region unspecified)
     (bg-tab-bar        bg-main)
     (bg-tab-current    bg-active)
     (bg-tab-other      bg-dim)
     (fringe unspecified)
     (bg-mode-line-active bg-dim)
     (border-mode-line-active unspecified)
     (border-mode-line-inactive unspecified)
     (bg-line-number-active  bg-main)
     (bg-line-number-inactive  bg-main)
     (fg-line-number-active fg-dim)
     (fg-line-number-inactive border)

     ))

  :config
  (load-theme 'modus-vivendi t))
(set-face-attribute 'region nil :extend nil)
(set-face-attribute 'message-header-subject nil :height 1.5)

(with-eval-after-load 'org
  (dolist (face '(org-tag org-todo org-done org-priority org-sexp-date))
    (set-face-attribute face nil
                        :inverse-video t
                        :weight 'bold)))


(modus-themes-with-colors
  (custom-set-faces
   ;; Add "padding" to the mode lines
   `(mode-line ((,c :box (:line-width 10 :color ,bg-mode-line-active))))
   `(mode-line-inactive ((,c :box (:line-width 10 :color ,bg-mode-line-inactive))))))

;; (setopt
;;  mode-line-format
;;  '("%e" "  "
;;    (:propertize
;;     ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote))
;;    mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
;;    mode-line-format-right-align
;;    "  "
;;    (project-mode-line project-mode-line-format) " " (vc-mode vc-mode) "  " mode-line-modes mode-line-misc-info "  "))

(setopt
 mode-line-format
 '("%e"
   mode-line-front-space mode-line-modified
   ;; mode-line-remote
   mode-line-window-dedicated
   "  ⬩"
   mode-line-frame-identification mode-line-buffer-identification
   "  ⬩  "
   mode-line-position mode-line-format-right-align
   (project-mode-line project-mode-line-format)
   (vc-mode vc-mode)
   " ⬩ " mode-name " ⬩ "
   ;; "  " mode-line-modes
   mode-line-misc-info))

(setopt
 mode-line-modified
 '((:eval (cond
           ((buffer-modified-p)
            (propertize ""
                        'face 'modus-themes-fg-yellow-cooler))
           (buffer-read-only
            (propertize ""
                        'face 'modus-themes-fg-magenta-cooler))
           (t
            (propertize ""
                        'face 'modus-themes-fg-cyan-cooler))))))


(setopt mode-line-position-column-line-format '("%l:%c"))
(setopt mode-line-position-line-format '("L%l"))
(setopt mode-line-right-align-edge 'window)

(use-package d/toggle-bar :ensure nil :no-require t
  :bind ([f9] . d/toggle-bar-mode)
  :init
  (define-minor-mode d/toggle-bar-mode
    "The void space to hide mode-line."
    :lighter "Vanish" :init-value nil
    (setq mode-line-format
          (if d/toggle-bar-mode
              nil
            (default-value 'mode-line-format)))
    ;; (toggle-frame-tab-bar)
    (redraw-display))
  :hook
  (help-mode nov-mode))

(use-package proced
  :bind ("C-x x p" . 'proced)
  :custom
  (proced-enable-color-flag t)
  (proced-format '(user start time pcpu pmem rss args))
  (proced-sort 'pmem)
  (proced-auto-update-flag t))

;; credit: yorickvP on Github
(defun wl-copy (text)
  (let ((p (make-process :name "wl-copy"
                         :command '("wl-copy")
                         :connection-type 'pipe)))
    (process-send-string p text)
    (process-send-eof p)))

(unless d/on-droid
  (setq interprogram-cut-function 'wl-copy)
  )

(use-package org :ensure nil :defer t
  :hook
  (org-mode . variable-pitch-mode)
  (org-mode . org-indent-mode)

  :bind (
         ("C-c t i" . org-timer-set-timer)

         (:map org-mode-map
               ("C-c l" . org-store-link)
               ("M-n" . org-shiftdown)
               ("M-p" . org-shiftup)
               ))

  :custom
  (org-ellipsis " ...")
  (org-use-sub-superscripts '{})
  (org-log-done 'note)
  (org-log-into-drawer t)
  (org-export-exclude-tags '("noexport" "ignore") "excludes these tagged heading from export")
  (org-latex-compiler "lualatex" "Lualatex is fast and gets custom font too")
  (org-link-file-path-type 'relative)
  (org-todo-keywords
   '((sequence "TODO(t)" "ONGO(o)" "WAIT(w)" "|" "DONE(d)" "SKIP(s)") ))
  (org-todo-keyword-faces
   '(("ONGO" . org-agenda-clocking)
     ("WAIT" . org-sexp-date)
     ("SKIP" . org-agenda-dimmed-todo-face)))

  (org-priority-highest 1)
  (org-priority-lowest  5)
  (org-priority-default 4)

  (org-clock-in-switch-to-state "STARTED")

  (org-refile-targets
   '( (org-default-notes-file :maxlevel . 5)
      (nil :maxlevel . 6)
     ))

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
  )

(defun d/org-activity()
  "Make temp activity buffer for org tags."
  (interactive)
  (with-current-buffer "d-brain.org"
    (let ((org-export-select-tags (completing-read-multiple "tags: " (org-get-buffer-tags))))
      (org-export-to-buffer 'org (format "export %s.org" org-export-select-tags)))
    (org-mode) (delete-other-windows)
    ))

(defun d/org-heading()
  "Jump to desired heading in temp buffer or narrow buffer."
  (interactive)
  (with-current-buffer "d-brain.org"
    (let* ((org-goto-interface 'outline-path-completion)
           (org-outline-path-complete-in-steps nil)
           (method (completing-read "Choose a method: " '("export" "narrow"))))
      (org-goto)
      (if (string= method "narrow") (progn (org-narrow-to-subtree) (switch-to-buffer "d-brain.org"))
        (org-export-to-buffer 'org (format "export-heading.org") nil t t) (org-mode) (delete-other-windows))
      ))
  )

(global-set-key (kbd "C-x C-a C-o") #'d/org-activity)
(global-set-key (kbd "C-x C-a C-p") #'d/org-heading)

(use-package org-agenda :ensure nil :demand t
  :init (org-agenda nil "a")
  :bind (("C-c d a" . org-agenda)
         ("C-c a" . org-agenda)
         (:map org-agenda-mode-map
               ("C-x C-k" . org-agenda-exit)))

  ;; :hook (org-agenda-finalize . org-agenda-entry-text-mode)
  :custom
  (org-agenda-include-diary t)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-window-setup 'only-window)

  (org-agenda-custom-commands
   '(("n" "Next tasks" ((todo "STARTED") (todo "NEXT") (todo "PROJ")))
     ("a" "Agenda and all TODOs" ((agenda "") (alltodo "")))))

  (org-agenda-files
   '("~/d-sync/notes/d-brain.org"
     "~/d-sync/notes/inbox.org"
     "~/d-git/d-nix/d-setup.org"
     ;; "~/d-git/d-site/README.org"
     )))

(with-current-buffer "d-setup.org" (emacs-lock-mode 'kill))
(with-current-buffer "d-brain.org" (emacs-lock-mode 'kill))

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
      "** %?  :z@seed:\n %U\n %i %a\n - ")

     )))

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
  :bind (
         ("M-s q" . org-ql-find)
         ("M-s n" . org-ql-find-in-org-directory)
         (:map org-mode-map
               ("C-c q f" . org-ql-find) ("C-c q /" . org-ql-sparse-tree)
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
    (org-goto-random-heading :buffers (current-buffer) :regexp (read-from-minibuffer "Search regexp: "))))

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
             (org-agenda-to-appt t)))))

  (appt-activate))

(use-package notifications :ensure nil :config
  (defun appt-org-notify (remaining new-time msg)
    (let ((notif (if d/on-droid 'android-notifications-notify 'notifications-notify)))
      (funcall notif
               :body (format "In %s minutes" remaining)
               :title msg
               :urgency 'critical))))

(use-package ox :after org
  :custom (org-export-backends '(org odt md man latex icalendar html ascii)))

(defun d/org-export-clean()
  "Function to retain only level 4 heading in `datetree' single big org file export."
  (interactive)
  (if (y-or-n-p "Do you want to only have 4th headings and flush rest?")
      (progn (beginning-of-buffer)
        (flush-lines "^\\* ")
        (flush-lines "^\\*\\* ")
        (flush-lines "^\\*\\*\\* ")
        (query-replace "****" "*"))
    ))

(use-package org-crypt :after org :ensure nil
  :config (org-crypt-use-before-save-magic)
  :custom
  (org-tags-exclude-from-inheritance '("crypt"))
  (org-crypt-key nil)
  (epg-pinentry-mode 'loopback)
  )

(use-package remember :ensure nil
  :bind ("C-c r r" . remember) ("C-c r n" . remember-notes)
  :custom
  (initial-buffer-choice 'remember-notes)
	(remember-data-file (expand-file-name "inbox.org" org-directory))
	(remember-notes-initial-major-mode 'org-mode))

(use-package calendar
  :bind ("C-c d d" . calendar)
  :custom (diary-file "~/d-sync/notes/diary")
  (calendar-date-style 'european))

(use-package markdown-mode :defer t
  :mode "\\.md\\'" "\\.Rmd\\'"
  :hook (markdown-mode . variable-pitch-mode))

(use-package ox-typst :unless d/on-droid 
  :vc (:url "https://github.com/jmpunkt/ox-typst")
  :config (defalias 'typst-mode #'typst-ts-mode))

(use-package jinx :unless d/on-droid
  :init (global-jinx-mode)
  :hook org-mode
  :bind ("M-$". jinx-correct))

(use-package quick-sdcv :unless d/on-droid )

(use-package ispell :demand t
  :custom (ispell-alternate-dictionary (expand-file-name "~/.config/enchant/en_US.dic")))

(use-package flyspell)
