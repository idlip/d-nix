;; -*- lexical-binding: t; -*-

(setopt
 display-time-24hr-format t
 display-time-default-load-average nil
 display-time-format "%H:%M")
(display-time-mode 1)

(setopt battery-load-low '40 battery-load-critical '29)
(display-battery-mode 1)

(bind-keys ("C-z") ("C-x C-z") ("M-o" . other-window) ("M-j" . duplicate-dwim) )

(setopt
 inhibit-startup-screen t
 initial-scratch-message (format "\n\n")

 tab-always-indent 'complete
 reb-re-syntax 'string
 fill-column 100

 window-combination-resize t
 history-delete-duplicates t

 sentence-end-double-space nil
 sentence-end "[.?!]"
 read-process-output-max (* 1024 1024)
 initial-major-mode 'org-mode
 enable-recursive-minibuffers t
 switch-to-buffer-obey-display-actions t
 )

(delete-selection-mode 1)
(setq-default
 indent-tabs-mode nil
 tab-width 4
 standard-indent 4)
(global-so-long-mode 1)
(with-current-buffer "*scratch*" (emacs-lock-mode 'kill))
(modify-all-frames-parameters '((alpha-background . 99)))

(dolist (cmd '(narrow-to-region
               list-timers narrow-to-region narrow-to-page upcase-region downcase-region
               upcase-dwim
               downcase-dwim
               dired-find-alternate-file
               narrow-to-page
               set-goal-column
               scroll-left
               scroll-right))
  (put cmd 'disabled nil))

(setopt
 kill-ring-max 30000
 kill-do-not-save-duplicates t
 set-mark-command-repeat-pop t
 use-dialog-box nil
 use-file-dialog nil
 use-short-answers t
 async-shell-command-buffer 'new-buffer
 async-shell-command-display-buffer nil
 grep-use-headings t
 save-interprogram-paste-before-kill t
 global-hl-line-sticky-flag 'window
 )

(global-hl-line-mode 1)
(save-place-mode 1)
(column-number-mode)
(global-visual-line-mode 1)
(global-visual-wrap-prefix-mode 1)
(global-subword-mode 1)

;; credits oantolin's config
(dolist (cmd '(narrow-to-region
               upcase-region
               downcase-region
               dired-find-alternate-file
               LaTeX-narrow-to-environment
               TeX-narrow-to-group
               narrow-to-page
               set-goal-column
               scroll-left
               scroll-right))
  (put cmd 'disabled nil))

(setopt display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(auto-save-visited-mode 1)

(setopt
 save-silently t
 confirm-kill-emacs 'yes-or-no-p
 view-read-only t
 custom-file (expand-file-name "custom.el" user-emacs-directory)
 safe-local-variable-directories
 '("~/d-sync/notes/")
 create-lockfiles nil
 backup-directory-alist '(("." . "~/.config/emacs/backups"))
 version-control t 
 delete-old-versions t
 kept-new-versions 5
 kept-old-versions 2
 )

(use-package undo-fu-session
  :init (undo-fu-session-global-mode)
  :custom (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package vundo
  :bind
  (("C-x u" . vundo) ("C-z" . undo-only) ("C-S-z" . undo-redo) ("C-M-r" . undo-redo))
  :custom
  (vundo-compact-display t) (vundo-glyph-alist vundo-unicode-symbols))

(setopt undo-limit         (* 96 1024 1024)) ;  96 MiB. The change group at which this size is exceeded is the last one kept.
(setopt undo-strong-limit (* 128 1024 1024)) ; 128 MiB
;; The change group at which this size is exceeded is discarded itself (along with all older change
;; groups).There is one exception: the very latest change group is only discarded if it exceeds
;; ‘undo-outer-limit’.
(setopt undo-outer-limit (* 1024 1024 1024)) ;   1 GiB
;;.If at garbage collection time the undo info for the current command exceeds this limit,Emacs
;;discards the info and displays a warning.This is a last ditch limit to prevent memory overflow.

(setopt global-auto-revert-non-file-buffers t
        auto-revert-check-vc-info t
        auto-revert-avoid-polling t)
(global-auto-revert-mode 1)

(setopt
 history-length 2000
 save-place-limit nil
 savehist-additional-variables
 '(kill-ring command-history
             set-variable-value-history custom-variable-history
             query-replace-history read-expression-history
             minibuffer-history read-char-history face-name-history
             bookmark-history file-name-history
             mark-ring global-mark-ring search-ring regexp-search-ring register-alist extended-command-history))
(savehist-mode 1)

(global-set-key (kbd "C-x C-r") #'recentf)
(setopt recentf-max-menu-items 1000
        recentf-max-saved-items 1000)
(recentf-mode 1)

(file-name-shadow-mode 1)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'dired-omit-mode)

(setopt
 dired-listing-switches "-agho --group-directories-first"
 dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'\\|^\\..*$"
 delete-by-moving-to-trash t
 dired-dwim-target t
 dired-kill-when-opening-new-dired-buffer nil
 )

(setopt wdired-allow-to-change-permissions t
        wdired-create-parent-directories t)

(keymap-global-set "<remap> <dabbrev-expand>" 'hippie-expand)
(setopt hippie-expand-verbose t
        hippie-expand-dabbrev-skip-space t)

(use-package vertico :init (vertico-mode)
  :bind ((:map vertico-map ("C-v" . vertico-scroll-up) ("M-v" . vertico-scroll-down)
               ("DEL" . vertico-directory-delete-char) ("M-DEL" . vertico-directory-delete-word) ))
  :custom ;;(vertico-count 5)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package vertico-multiform :init (vertico-multiform-mode)
  :custom
  (vertico-multiform-commands
   '((load-theme grid) (consult-theme grid)
     (dired-goto-file flat) (consult-recoll buffer)
     (consult-dff unobtrusive) (d/insert-bookmark-link unobtrusive)
     (embark-act grid) (org-set-tags-command grid)
     ))

  (vertico-multiform-categories
   '((consult-grep buffer)
     (jinx grid) (embark-bindings grid) (embark-keybinding grid)
     (command flat) (file grid)
     (buffer flat (vertico-cycle . t)))))

(use-package consult :defer t
  :bind
  (;; C-c bindings (mode-specific-map)
   ("C-c h" . consult-history) ("C-c k" . consult-kmacro)
   ;; C-x bindings (ctl-x-map)
   ("C-x M-x" . consult-mode-command)
   ("C-x C-b" . ibuffer) ("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x r b" . consult-bookmark) ("C-x p b" . consult-project-buffer)
   ("M-y" . consult-yank-pop) ;; Other custom bindings
   ;; M-g bindings (goto-map)
   ("M-g f" . consult-flycheck)
   ("M-g M-g" . consult-goto-line)
   ("M-g o" . consult-outline) ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark) ("M-g i" . consult-imenu)
   ("M-g s" . consult-eglot-symbols)
   ;; M-s bindings (search-map)
   ("M-s d" . consult-fd)
   ("M-s g" . consult-ripgrep) ("M-s m" . consult-man)
   ("M-s r" . consult-ripgrep)
   ("M-s i" . consult-info) ("M-s l" . consult-line)

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

  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (imenu-max-item-length nil)
  (consult-ripgrep-args
   "rg --null --line-buffered --color=never --max-columns=1000 \
      --path-separator / --smart-case --no-heading --with-filename \
      --line-number --search-zip -U")
  )

(setopt completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))
        completion-ignore-case t
        completion-pcm-leading-wildcard t
        completion-category-defaults nil)

(use-package embark :defer t
  :bind
  (("C-." . embark-act) ("C-;" . embark-act-all)
   ("M-." . embark-dwim) ("C-h B" . embark-bindings)
   ("C-h b" . embark-bindings) ("C-h M" . embark-bindings-in-keymap)

   (:map embark-identifier-map
         ("!" . shell-command-on-region)
         ("ch" . color-name-to-hex)
         ("cr" . color-name-to-rgb)
         ("(" . insert-parentheses)
         ("[" . insert-pair-map) )
   (:map embark-url-map
         ("b" . browse-url-generic)
         ("e" . eww-open-in-new-buffer) )
   (:map embark-file-map
         ("b" . browse-url-of-dired-file))
   (:map embark-expression-map
         ("(" . insert-parentheses)
         ("[" . insert-pair-map))
   (:map embark-region-map
         ("(" . insert-parentheses)
         ("[" . insert-pair-map)
         ("=" . quick-calc))
   )

  :custom
  (prefix-help-command #'embark-prefix-help-command)
  (embark-indicators
   '(embark-minimal-indicator
     embark-highlight-indicator
     embark-isearch-highlight-indicator))
  (embark-confirm-act-all nil)
  )
(add-to-list 'display-buffer-alist `("\\*\\(Embark\\|Completions\\).*\\*"
									 nil (window-parameters (mode-line-format . none))))

(use-package embark-consult :defer t :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia :init (marginalia-mode))

(use-package corfu :init (global-corfu-mode)
  :bind (:map corfu-map ("SPC" . corfu-insert-separator))
  :custom (corfu-auto nil) (corfu-auto-trigger ".")
  :config (corfu-history-mode) (corfu-echo-mode) (corfu-popupinfo-mode)
  (eldoc-add-command #'corfu-insert))

(use-package cape
  :bind ("C-c p" . cape-prefix-map)
  ("M-i" . cape-prefix-map)  ("C-'" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-history)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-abbrev))

(use-package tempel :hook (prog-mode . tempel-abbrev-mode)
  :bind (("M-+" . tempel-complete) ("M-*" . tempel-insert)))

(use-package ibuffer :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :hook (ibuffer-mode . ibuffer-set-filter-groups-by-mode))

(setopt uniquify-buffer-name-style 'forward)

(setopt isearch-lazy-count t
        search-whitespace-regexp ".*?")

(context-menu-mode 1)
(setopt mouse-autoselect-window t)

(winner-mode 1)

(setopt
 scroll-step 3
 scroll-margin 0
 mouse-wheel-progressive-speed nil
 mouse-wheel-scroll-amount '(1 ((control) . 1)  ((shift) . 2) ((meta) . 3))
 scroll-conservatively 101
 scroll-preserve-screen-position t
 pixel-scroll-precision-interpolate-page t)

;; (pixel-scroll-precision-mode 1)

;; (bind-keys ("C-v" . View-scroll-half-page-forward) ("M-v" . View-scroll-half-page-backward))
;; (bind-keys ("C-v" . pixel-scroll-interpolate-down) ("M-v" . pixel-scroll-interpolate-up))
(if d/on-foss (ultra-scroll-mode 1))

(setq repeat-exit-timeout 2) (put 'other-window 'repeat-map nil)
(repeat-mode 1)

(with-eval-after-load 'zone
  (zone-when-idle (* 60 5)))

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
  (diff-font-lock-syntax 'hunk-also))

(use-package envrc :defer 2
  :config (envrc-global-mode 1)
  (advice-add 'org-babel-eval :around #'envrc-propagate-environment))

(use-package esh-mode :ensure nil
  :hook (eshell-mode . (lambda () (setq outline-regexp eshell-prompt-regexp)))
  :custom
  (eshell-hist-ignoredups t)
  (eshell-kill-processes-on-exit 'ask)
  (eshell-aliases-file (expand-file-name "eshell/alias" user-emacs-directory)))

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
   (display-buffer-at-bottom)
   ;; (window-parameters (mode-line-format . none))
   ;; (post-command-select-window . t)
   (window-height . 0.3)))

(setq comint-pager "cat")
(setenv "MANPAGER" "cat") ;; or mxp script

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(use-package flycheck :defer t
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (flycheck-idle-change-delay 3)
  (flycheck-checker-error-threshold 5000)
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-buffer-switch-check-intermediate-buffers t)
  (flycheck-display-errors-delay 0.25))

(use-package reformatter
  :hook
  (python-ts-mode . ruff-format-on-save-mode)
  (ess-r-mode . styler-format-on-save-mode)
  (bash-ts-mode . shell-format-on-save-mode)
  ;; (nix-ts-mode . nixfmt-rfc-format-on-save-mode)

  :config
  (reformatter-define ruff-check-fix :program "ruff"
    :args (list "check" "--fix" "--stdin-filename" input-file "-"))
  (reformatter-define ruff-format :program "ruff"
    :args (list "format" "--stdin-filename" input-file "-"))

  (reformatter-define pyblack-format :program "black"
    :args (list "black" "-"))

  (reformatter-define shell-format :program "shfmt" )

  (reformatter-define nixfmt-rfc :program "nixfmt")

  (reformatter-define eslint-format :program "eslint"
	:args (list "--fix-dry-run" "--stdin" "--stdin-filename" buffer-file-name)
	:stdin t :stdout nil )

  (reformatter-define prettier-format :program "prettier"
	:args (list "--stdin-filepath" buffer-file-name))

  (reformatter-define jinja-format :program "djlint"
	:args '("--reformat" "--quiet" "-"))
  )

(use-package eglot :defer t :ensure nil :unless d/on-droid
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)

  :config
  (fset #'jsonrpc--log-event #'ignore)
  (add-to-list 'eglot-server-programs '(nix-mode . ("nixd")))
  (add-to-list 'eglot-server-programs '(markdown-mode . ("harper-ls" "--stdio")))
  (add-to-list 'eglot-server-programs '(org-mode . ("harper-ls" "--stdio")))
  (add-to-list 'eglot-server-programs '(text-mode . ("harper-ls" "--stdio")))
  ;;   (add-to-list 'eglot-server-programs '(bash-ts-mode . ("bash-language-server")))
  ;;   (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman")))
  )

(use-package flycheck-eglot :unless d/on-droid :after eglot :init (global-flycheck-eglot-mode))

(setopt xref-search-program 'ripgrep
        grep-command "rg ")

(use-package compile
  :hook (compilation-filter . ansi-color-compilation-filter)
  :bind ("M-#" . compile) ; M-! M-# M-& M-\
  ("M-s c" . compile)
  :custom
  (shell-command-switch "-c") ;; -i
  (compilation-scroll-output t)
  )

(use-package project :ensure nil
  :custom (project-compilation-buffer-name-function 'project-prefixed-buffer-name)
  (project-switch-use-entire-map t))

(use-package treesit :ensure nil
  ;; :mode
  ;; (("\\.tsx\\'" . tsx-ts-mode)
  ;;  ("\\.yaml\\'" . yaml-ts-mode) ("\\.toml\\'" . toml-ts-mode) ("\\.jsonrc\\'" . json-ts-mode)
  ;;   ("\\.json\\'" .  json-ts-mode)
  ;;  ("\\.jsx\\'" . tsx-ts-mode)
  ;;  ("\\.Dockerfile\\'" . dockerfile-ts-mode)
  ;;  ("\\.sh\\'" . bash-ts-mode))

  :custom
  (treesit-enabled-modes t)
  (treesit-font-lock-level 4)
  (standard-indent 2)
  ;; (major-mode-remap-alist
  ;;  '((c-mode . c-ts-mode) (c++-mode . c++-ts-mode) (nix-mode . nix-ts-mode)
  ;;    (csharp-mode . csharp-ts-mode) (css-mode . css-ts-mode)
  ;;    (java-mode . java-ts-mode) (js-mode . js-ts-mode) (html-mode . mhtml-ts-mode)
  ;;    (js-json-mode . json-ts-mode) ;; (org-mode . org-ts-mode) ;; not mature yet
  ;;    (python-mode . python-ts-mode) (julia-mode . ess-julia-mode)
  ;;    (typescript-mode . typescript-ts-mode) (sh-mode . bash-ts-mode) (shell-script-mode . bash-ts-mode)
  ;;    (ruby-mode . ruby-ts-mode) (rust-mode . rust-ts-mode)
  ;;    (toml-mode . toml-ts-mode) (yaml-mode . yaml-ts-mode)))
  )

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

(use-package prog-mode :ensure nil :hook (prog-mode . hs-minor-mode) (prog-mode . outline-minor-mode))

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

;; credits to oantolin's config
(bind-keys :prefix-map insert-pair-map
           :prefix "M-]"
           ("d" . delete-pair)
           ([t] . insert-pair))
(global-set-key (kbd "C-M-z") #'delete-pair)
(setopt delete-pair-blink-delay 0.1)

(define-advice insert-pair (:filter-args (args) numeric-prefix)
  (cons (prefix-numeric-value (car args)) (cdr args)))

(use-package paren :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-highlight-openparen t) (show-paren-context-when-offscreen t)
  (show-paren-delay 0) (show-paren-style 'parenthesis) (show-paren-context-when-offscreen 'overlay)
  )

(use-package colorful-mode :unless d/on-droid
  :config (global-colorful-mode))

(use-package rainbow-delimiters :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package combobulate :after treesit
  :preface (setq combobulate-key-prefix "C-c o")
  :hook
  ((python-ts-mode . combobulate-mode) (css-ts-mode . combobulate-mode)
   (bash-ts-mode . combobulate-mode)))

(use-package proced
  :bind ("C-x x p" . 'proced)
  :custom
  (proced-enable-color-flag t)
  (proced-format '(user start time pcpu pmem rss args))
  (proced-sort 'pmem)
  (proced-auto-update-flag t))

(use-package python :ensure nil
  :bind (:map python-mode-map ("C-c C-d" . devdocs-browser-open))
  :custom
  ;; (python-forward-sexp-function nil)
  (python-indent-guess-indent-offset-verbose nil))

;; hacky way to run python tools in any dir without envrc or anything
(defun d/dev-uvx-command ()
  "Prompt for package and command, to run dev environment."
  (interactive)
  (let ((pkg (read-string "Enter package: "))
        (cmd (read-string "Enter Command Args: ")))
    (setq d/dev-uvx-command
          (append '("uvx" "--from") (list pkg) (split-string cmd)))
    (message "Set d/dev-uvx-command to: %S" d/dev-uvx-command)))

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

(unless d/on-droid (use-package nix-ts-mode) )

(use-package js :ensure nil :mode ("\\.jsx\\'" . js-jsx-mode)
  ("\\.vue\\'" . js-ts-mode))

(use-package verb :after org
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(use-package ess-julia :unless d/on-droid
  :hook (ess-julia-mode . (lambda () (setq-local devdocs-browser-active-docs '("Julia"))))
  :bind (:map ess-julia-mode-map ("C-c C-d" . devdocs-browser-open))
  :custom (inferior-julia-args "--color=yes" "You get color in julia inferior process"))

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
  )

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

(use-package nov :mode ("\\.epub\\'" . nov-mode)
  :hook
  (nov-mode . nov-imenu-setup)
  :custom
  (nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title))))

(use-package gnus
  :hook
  (gnus-group-mode . gnus-topic-mode)
  (gnus-summary-mode . turn-on-gnus-mailing-list-mode)
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
  (gnus-auto-expirable-newsgroups
   "nnimap\\+.*:\\[Gmail\\]/\\(Trash\\|Spam\\)")
  (gnus-expiry-wait 7)
  (nnimap-expunge t)

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
	   ;; (nntp "yhetil" (nntp-address "news.yhetil.org"))
     (nnrss "")
     ))

  (gnus-server-alist '(
					   ("archive" nnfolder "archive"
						(nnfolder-directory "~/.config/emacs/feeds/gnews/mail/archive")
						(nnfolder-active-file "~/.config/emacs/feeds/gnews/mail/archive/active")
						(nnfolder-get-new-mail nil) (nnfolder-inhibit-expiry t))
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
  (if d/on-foss (load-file "~/d-sync/feeds/gnews/privmail.el"))
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

(setopt
 fast-read-process-output nil
 gnus-search-use-imap t
 ;; mail-user-agent 'gnus-user-agent ;; why open gnus?
 ;; read-mail-command #'gnus
 gnus-save-newsrc-file nil
 gnus-read-newsrc-file nil
 message-confirm-send t
 message-forward-as-mime t
 message-send-mail-function #'smtpmail-send-it)

(use-package smtpmail :unless d/on-droid
  ;; :after gnus
  :custom
  (starttls-use-gnutls t)
  (send-function 'smtpmail-send-it)
  (message-send-mail-function 'smtpmail-send-it)
  (mail-from-style 'angles)
  (smtpmail-debug-info t)
  (smtpmail-debug-verb t))

(use-package url :ensure nil
  :custom (url-privacy-level 'high) ;; reddit/SO does not like it 'paranoid
  :config (url-setup-privacy-info))

(use-package shr :ensure nil :demand t
  :custom (shr-bullet "⦿ ") (shr-width 100) (shr-max-image-proportion 0.5))

(use-package eww :ensure nil :demand t
  :hook
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
  :config
  (setopt browse-url-browser-function 'ewm-handle-link
          browse-url-secondary-browser-function 'ewm-handle-link))

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
			   ("." . mpc-play-at-point) ("u" . mpc-update)
               ))
  :custom (mpc-browser-tags '(Title))
  :config
  (defun mpc-select-dwim ()
    (interactive)
    (mpc-select-toggle)
    (next-line)
    ))

(defcustom d/font-size (if d/on-droid 170 130)
  "Default font size based on the system.")

;; Dont worry about the font name, I use fork of Iosevka font

;; Set reusable font name variables
(defcustom d/fixed-pitch-font (if d/on-droid "Maple Mono NF" "Maple Mono NF")
  "The font to use for monospaced (fixed width) text.")

(defcustom d/variable-pitch-font (if d/on-droid "Inter" "Inter")
  "The font to use for variable-pitch (documents) text.")

(use-package faces :ensure nil
  :custom-face
  (variable-pitch ((t (:family ,d/variable-pitch-font :height 1.1 :weight normal))))
  (fixed-pitch ((t (:family ,d/fixed-pitch-font :weight normal))))
  (default ((t (:family ,d/fixed-pitch-font :height ,d/font-size :weight normal)))))

(global-font-lock-mode 1)

(use-package modus-themes :ensure nil :demand t
  :init (require-theme 'modus-themes)
  :custom-face
  (minibuffer-nonselected ((t (:inverse-video t))))
  :custom
  (modus-themes-italic-constructs t) (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t)

  (modus-themes-headings
   '((1 . (variable-pitch 1.5))
     (2 . (1.3))
     (agenda-date . (regular 1.1))
     (agenda-structure . (variable-pitch light 1.3))
     (t . (1.1))))

  (modus-vivendi-palette-overrides
   '(
     (bg-main        "#000000") (bg-dim         "#0a0f0a")
     (bg-active      "#111c17") (bg-inactive    "#1b2a24")

     (fg-main     "#ffffff") (fg-dim      "#b4aeae")

     (fg-heading-1  "#ab82ff") (fg-heading-2  "#fab387") (prose-tag "#ffe")
     (mail-subject  "#6ae4b9")
     
     (cursor "#5fd7af") (bg-completion "#248f6c") (bg-hl-line "#142f2b") ;;seagreen
     ;; (cursor "#7aa2f7") (bg-completion "#2a2e3b") (bg-hl-line "#24283b") ;; tokyonight
     ;; (cursor "#528bff") (bg-completion "#3e4451") (bg-hl-line "#2c313c") ;; onedark
     ;; (cursor "#9fc5e8") (bg-completion "#2a2f3a") (bg-hl-line "#212530") ;; Moonlight
     ;; (cursor "#89b4fa") (bg-completion "#292d3e") (bg-hl-line "#222538") ;; Palenight
     ;; (cursor "#d3869b") (bg-completion "#665c54") (bg-hl-line "#3c3836") ;; Gruvbox Dark Hard
     ;; (cursor "#a7c080") (bg-completion "#2a2e26") (bg-hl-line "#232823") ;; Everforest
     ;; (cursor "#f2cdcd") (bg-completion "#6d4f4f") (bg-hl-line "#3e2f2f") ;; Catppuccin Mocha
     ;; (cursor "#e0def4") (bg-completion "#44415a") (bg-hl-line "#2a273f") ;; Rose Pine

     (bg-region     bg-completion) (fg-region unspecified)

     (bg-tab-bar bg-main) (bg-tab-current bg-active) (bg-tab-other bg-dim)
     (fringe unspecified)
     (bg-mode-line-active bg-dim)
     (bg-line-number-active  bg-main) (bg-line-number-inactive  bg-main)
     (fg-line-number-active fg-dim) (fg-line-number-inactive border)
     (border-mode-line-active bg-completion) (border-mode-line-inactive unspecified)
     ))

  :config
  (load-theme 'modus-vivendi t))

(setopt
 mode-line-format
 '("%e"
   mode-line-front-space mode-line-modified
   ;; mode-line-remote
   mode-line-window-dedicated
   "  "
   mode-line-frame-identification mode-line-buffer-identification
   "    "
   mode-line-position mode-line-format-right-align
   (project-mode-line project-mode-line-format)
   (vc-mode vc-mode)
   "  " mode-line-modes
   mode-line-misc-info)

 mode-line-collapse-minor-modes t)
(global-set-key (kbd "<f9>") 'mode-line-invisible-mode)

(use-package olivetti :defer t :custom (olivetti-body-width 100)
  :hook (org-mode Info-mode help-mode gnus-group-mode gnus-article-mode nov-mode markdown-mode))
;; (add-hook 'olivetti-mode-hook #'variable-pitch-mode)

(setopt
 tab-bar-format
 '(tab-bar-format-menu-bar tab-bar-format-history
                           tab-bar-format-tabs-groups tab-bar-separator
                           tab-bar-format-align-right
                           tab-bar-format-global ;; An issue when used in terminal+daemon (cursor wont move properly)
                           )
 tab-bar-close-button-show nil)
(tab-bar-mode t) (tab-bar-history-mode 1)

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
  (org-ellipsis " [...]")
  (org-use-sub-superscripts '{})
  (org-log-done 'note) (orgl-log-reschedule 'note)
  (org-log-into-drawer t)
  (org-export-exclude-tags '("noexport" "ignore") "excludes these tagged heading from export")
  (org-latex-compiler "lualatex" "Lualatex is fast and gets custom font too") ;; i moved to typst
  (org-todo-keywords
   '((sequence "TODO(t)" "ONGO(o)" "WAIT(w)" "|" "DONE(d)" "SKIP(s)") ))
  (org-todo-keyword-faces
   '(("ONGO" . org-agenda-clocking) ("WAIT" . org-sexp-date) ("SKIP" . org-agenda-dimmed-todo-face)))

  (org-priority-highest 1) (org-priority-lowest  5) (org-priority-default 3)

  (org-clock-in-switch-to-state "STARTED")
  
  (org-refile-targets '( (org-default-notes-file :maxlevel . 5) (nil :maxlevel . 6) ))

  (org-directory "~/d-sync/notes/")
  (org-default-notes-file (concat org-directory "d-brain.org")) ;; my second brain in OBTF
  (org-pretty-entities t) (org-hide-emphasis-markers t)
  (org-list-allow-alphabetical t)

  (org-fontify-whole-heading-line t) (org-fontify-quote-and-verse-blocks t)

  (org-hierarchical-todo-statistics nil)

  (org-special-ctrl-k t) (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)

  (org-image-actual-width nil)

  :config
  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)  )

(defun d/org-activity()
  "Make temp activity buffer for org tags."
  (interactive)
  (with-current-buffer "d-brain.org"
    (let ((org-export-select-tags (completing-read-multiple "tags: " (org-get-buffer-tags))))
      (org-export-to-buffer 'org (format "export %s.org" org-export-select-tags)))
    (org-mode) (delete-other-windows)
    ))
(global-set-key (kbd "C-x C-a C-o") #'d/org-activity)

(defun d/insert-bookmark-link (&optional return)
  "Select and insert a link from bookmarks.org."
  (interactive)
  (let* ((file "~/d-sync/notes/bookmarks.org")
         (links (with-temp-buffer
                  (insert-file-contents file)
                  (goto-char (point-min))
                  (let (result)
                    ;; Match org-style [[url][title]]
                    (while (re-search-forward org-link-bracket-re nil t)
                      (push (cons (match-string 2) (match-string 1)) result))
                    ;; Match bare https:// links
                    (goto-char (point-min))
                    (while (re-search-forward "https://[^\s\n]+" nil t)
                      (let ((url (match-string 0)))
                        (unless (assoc url result)
                          (push (cons url url) result))))
                    (nreverse result))))
         (selected (completing-read "Link: " (mapcar #'car links)))
         (url (if (equal selected "pass") (insert-file-contents "~/.local/bin/pass.txt")
                (cdr (assoc selected links)))))
    (when url (if return url (insert url)))))

(use-package org-agenda :ensure nil :demand t
  :init (org-agenda nil "a") ;; open agenda as dashboard
  :bind ( ("C-c a" . org-agenda)
         (:map org-agenda-mode-map
               ("C-x C-k" . org-agenda-exit)))

  ;; :hook (org-agenda-finalize . org-agenda-entry-text-mode)
  :custom
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-window-setup 'only-window)

  (org-agenda-custom-commands
   '(("n" "Next tasks" ((todo "STARTED") (todo "NEXT") (todo "PROJ")))
     ("a" "Agenda and all TODOs" ((agenda "") (alltodo "")))))

  (org-agenda-files
   '("~/d-sync/notes/d-brain.org"
     "~/d-sync/notes/inbox.org" "~/d-sync/notes/foss.org"
     ;; "~/d-git/d-nix/d-setup.org"
     ;; "~/d-git/d-site/README.org"
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
      :empty-lines 1 :empty-lines-after 3)

	 ("f" "Foss Work" entry
      (file+olp+datetree "foss.org")
      "* %<%H:%M> - %? %^G
:PROPERTIES:
:ID:       %(org-id-new)
:FROM:     %a
:END:
%i"
      :empty-lines 1 :empty-lines-after 3)

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
  (setq d/tangled-file-name (expand-file-name (assoc-default :tangle (nth 2 (org-babel-get-src-block-info)))))
  (org-babel-tangle '(4)) (org-edit-special)
  (setq-local buffer-file-name d/tangled-file-name)
  (eglot-ensure) )

(use-package org-id
  :hook (org-insert-heading . org-id-get-create)
  :custom
  (org-id-method 'ts)
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  ;; denote inspired heading id as identifier
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
               ("C-c q v" . org-ql-view))) )

(use-package org-super-agenda :after org
  :hook (org-agenda-mode . org-super-agenda-mode)
  :custom
  (org-super-agenda-hide-empty-groups t)
  (org-super-agenda-header-separator
   (concat (make-string 120 ?─) "\n\n"))
  (org-agenda-block-separator ?─)
  (org-super-agenda-groups
   '(
     (:name "🚨 FOSS ASAP" :and (:todo ("TODO" "ONGO") :time-grid t :regexp "\\<foss@") :order 0)

     (:name "💼 FOSS Work" :and (:todo ("TODO" "ONGO") :regexp "\\<foss@") :order 1)

     (:name "📌 FOSS Meetings / Reports"
            :tag ("foss@meeting" "foss@report"
                  "foss@log" "foss@checkin"
                  "foss@talk")
            :order 2)

     (:name "🚨 ASAP Tasks" :and (:todo ("TODO" "ONGO") :tag "t@asap") :order 3)

     (:name "🎓 Learning Now" :and (:todo ("TODO" "ONGO") :tag ("l@" "msc@")) :order 4)

     (:name "⏳ Waiting" :todo "WAIT" :order 20)

     (:name "📖 Reading" :tag ("t@read" "r@article" "r@book" "b@article" "b@res") :order 5)

     (:name "🏠 Personal & Home" :tag ("h@grocery" "h@bill" "h@plan" "w@birthday" "w@anniversary") :order 6)

     (:name "💪 Health & Fitness" :tag ("g@workout" "g@food") :order 7)

     (:name "💤 Someday / Skipped" :todo "SKIP" :order 21)
     )))

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
  (appt-message-warning-time (* 2 30))
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
               :title msg :timeout 0
               :urgency 'critical))))

(use-package ox :after org
  :custom (org-export-backends '(org odt md man latex icalendar html ascii)))

(defun d/org-export-clean-brain()
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
  :bind ("C-x M-r" . remember) ("C-c r r" . remember) ("C-c r n" . remember-notes)
  :custom
  (initial-buffer-choice 'remember-notes)
	(remember-data-file (expand-file-name (if d/on-foss "foss.org" "inbox.org") org-directory))
	(remember-notes-initial-major-mode 'org-mode))

(use-package calendar :bind ("C-c d d" . calendar))

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

(use-package flycheck-vale :if d/on-foss :config
 (flycheck-vale-setup))

(use-package telega :if d/on-foss
  ;; :hook
  ;; (telega-load . telega-notifications-mode)
  ;; (telega-load . telega-appindicator-mode)
  ;; (telega-root-mode . hl-line-mode)
  :custom-face
  (telega-msg-heading ((t :extend nil :background nil)))
  (telega-msg-user-title ((t :extend nil :background "#111")))
  :config
  (define-key global-map (kbd "C-c t") telega-prefix-map)
  (setopt telega-chat-input-markups '("org")
          )
  )

(use-package mastodon :if d/on-foss
  :custom
  (mastodon-instance-url "https://fosstodon.org")
  (mastodon-active-user "idlip"))

(use-package erc
  :config
  (erc-services-mode) (erc-notifications-mode)
  :custom
  (erc-prompt-for-password nil) (erc-prompt-for-nickserv-password nil) (erc-server-reconnect-timeout 3)
  (erc-autojoin-channels-alist '((Libera.Chat "#emacs" "#phi-mu-lambda" "#systemcrafters")))
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-track-exclude-types
   '("JOIN" "MODE" "NICK" "PART" "QUIT"
     "324" "329" "332" "333" "353" "477"))
  (erc-buffer-display 'buffer) (erc-interactive-display nil)
  (erc-autojoin-timing 'ident) (erc-fill-static-center 0)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-column (- fill-column 1))
  (erc-interpret-mirc-color t)
  (erc-kill-buffer-on-part t) (erc-kill-queries-on-quit t) (erc-kill-server-buffer-on-quit t)
  (erc-nicks-colors 'all) (erc-nicks-track-faces t)
  (erc-save-buffer-on-part t))

(use-package ewm :defer t
  :hook
  (emacs-startup . (lambda () (interactive) (ewm-launch "vicinae server")))
  (emacs-startup . (lambda () (interactive) (ewm-launch "noctalia-shell")))
  :custom
  (ewm-output-config '(("eDP-1" :scale 1.25 :enabled t)
                       ("HDMI-A-1" :scale 1.5)
                       ))
  (ewm-intercept-prefixes '("C-x" "C-u" "C-h" "M-x" "M-y" "M-S-;" "M-S-7" "M-S-3")) ;; (ewm--send-intercept-keys)
  (ewm-input-config ;; (ewm--send-input-config)
   '((touchpad :natural-scroll t :tap t :dwt t :accel-speed 0.5)
     (mouse :accel-profile "flat" :accel-speed 0.6)
      (keyboard :repeat-delay 500 :repeat-rate 100
                :xkb-layouts "us" :xkb-options "ctrl:nocaps")
     (trackpoint :accel-speed 0.5)))
  (ewm-idle 200)
  :bind (:map ewm-mode-map
              ("s-<tab>" . tab-next) ("s-S-<tab>" . tab-previous)
              ("s-n" . tab-next) ("s-p" . tab-previous)
              ("s-j" . next-buffer) ("s-k" . previous-buffer)
              ("s-l" . ewm-next-surface-buffer) ("s-h" . ewm-prev-surface-buffer)
              ("s-Y" . ewm-commit-kill) ("C-x C-c" . nil) ("s-E" . nil)
              ("s-w" . d/insert-bookmark-link) ("s-W" . d/firefox-history-insert)
              ("s-i" . ewm-link-handler-unified) ("s-o" . ewm-handle-link)
              ("s-d" . ewm-launch))
  :config
  ;; (ewm-text-input--auto-enable)
  )

(bind-keys ("C-x C-c" . nil) ("s-E" . nil) ("s-S-e" . nil))

;;; Unified launcher with PATH executables + .desktop actions
(defvar ewm-launcher-cache nil "Cache of (display-string . command) pairs.")

(defun ewm-launcher--desktop-dirs ()
  "Return desktop file directories from XDG_DATA_DIRS."
  (let ((xdg-dirs (split-string (or (getenv "XDG_DATA_DIRS") "") ":" t)))
    (seq-filter #'file-directory-p
                (mapcar (lambda (d) (expand-file-name "applications" d)) xdg-dirs))))

(defun ewm-launcher--parse-desktop-file (file)
  "Extract main entry + Desktop Actions from FILE as (label . command) pairs."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let (main-name main-exec actions)
        ;; Parse main desktop entry
        (when (re-search-forward "^\\[Desktop Entry\\]" nil t)
          (let ((section-end (or (save-excursion (re-search-forward "^\\[" nil t))
                                 (point-max))))
            (when (re-search-forward "^Name=\\(.+\\)" section-end t)
              (setq main-name (string-trim (match-string 1))))
            (goto-char (point-min))
            (when (re-search-forward "^Exec=\\(.+\\)" section-end t)
              (setq main-exec (string-trim 
                               (replace-regexp-in-string " %[UuFfDdNnickvm].*$" "" (match-string 1)))))))
        
        ;; Parse Desktop Actions
        (goto-char (point-min))
        (while (re-search-forward "^\\[Desktop Action \\(.+?\\)\\]" nil t)
          (let ((section-start (point))
                action-name action-exec)
            (let ((section-end (or (save-excursion (re-search-forward "^\\[" nil t))
                                   (point-max))))
              (goto-char section-start)
              (when (re-search-forward "^Name=\\(.+\\)" section-end t)
                (setq action-name (string-trim (match-string 1))))
              (goto-char section-start)
              (when (re-search-forward "^Exec=\\(.+\\)" section-end t)
                (setq action-exec (string-trim 
                                   (replace-regexp-in-string " %[UuFfDdNnickvm].*$" "" (match-string 1)))))
              (when (and action-name action-exec)
                (push (cons (format "%s: %s" main-name action-name) action-exec) actions)))))
        
        ;; Add main entry
        (when (and main-name main-exec)
          (push (cons main-name main-exec) actions))
        
        (nreverse actions)))))

(defun ewm-launcher--build-cache ()
  "Build cache of executables + desktop actions."
  (let (entries seen)
    ;; Scan PATH executables
    (dolist (dir (seq-filter #'file-directory-p exec-path))
      (dolist (file (directory-files dir nil "^[^.]"))
        (let ((full (expand-file-name file dir)))
          (when (and (file-regular-p full)
                     (file-executable-p full)
                     (>= (length file) 3)
                     (not (member file seen)))
            (push file seen)
            (push (cons file file) entries)))))
    
    ;; Scan .desktop files from XDG_DATA_DIRS
    (dolist (dir (ewm-launcher--desktop-dirs))
      (when (file-directory-p dir)
        (dolist (file (directory-files dir t "\\.desktop$"))
          (dolist (action (ewm-launcher--parse-desktop-file file))
            (let ((label (car action)))
              (unless (member label seen)
                (push label seen)
                (push action entries)))))))
    
    (nreverse entries)))

(defun ewm-launcher--execute (cmd method)
  "Execute CMD using METHOD, respecting direnv if active."
  (let ((default-directory (if (bound-and-true-p envrc-mode)
                               default-directory
                             default-directory))
        (process-environment (if (bound-and-true-p envrc-mode)
                                 process-environment
                               process-environment)))
    (pcase method
      ('start-process
       (start-process-shell-command cmd nil cmd))
      
      ('call-process
       (call-process-shell-command cmd))
      
      ('async-shell-buffer
       (async-shell-command cmd))
      
      ('output-echo
       (message "%s" (string-trim (shell-command-to-string cmd))))
      
      ('output-buffer
       (with-current-buffer (get-buffer-create "*ewm-output*")
         (erase-buffer)
         (insert (shell-command-to-string cmd))
         (goto-char (point-min))
         (display-buffer (current-buffer))))

      ('insert-output
       (insert (shell-command-to-string cmd)))
      
      ('compile-buffer
       (compile cmd)))))

(defun ewm-launch (arg)
  "Launch executable or desktop action.
With prefix ARG:
  C-u     - prompt for execution method
  C-u 1   - show output in buffer
  C-u 2   - show output in echo area
  C-u 3   - async shell buffer
  C-u 4   - compile buffer
  C-u 5   - call-process (synchronous)
  none    - start-process (background)"
  (interactive "P")
  (unless ewm-launcher-cache
    (setq ewm-launcher-cache (ewm-launcher--build-cache)))
  
  (let* ((input (completing-read "Run: " ewm-launcher-cache nil nil))
         (cmd (or (cdr (assoc input ewm-launcher-cache)) input))
         (method (cond
                  ;; Universal arg - prompt for method
                  ((equal arg '(4))
                   (intern (completing-read
                            "Execute as: "
                            '("start-process"
                              "call-process"
                              "async-shell-buffer"
                              "output-echo"
                              "output-buffer"
                              "compile-buffer"
                              "insert-output"
                              )
                            nil t)))
                  
                  ;; Numeric args - direct mapping
                  ((equal arg 1) 'output-buffer)
                  ((equal arg 2) 'output-echo)
                  ((equal arg 3) 'async-shell-buffer)
                  ((equal arg 4) 'compile-buffer)
                  ((equal arg 5) 'call-process)
                  ((equal arg 6) 'insert-output)
                  
                  ;; Default - background process
                  (t 'start-process))))
    
    (ewm-launcher--execute cmd method)))

(defun ewm-launcher-refresh ()
  "Rebuild launcher cache."
  (interactive)
  (setq ewm-launcher-cache (ewm-launcher--build-cache))
  (message "Launcher cache refreshed (%d entries)" (length ewm-launcher-cache)))

;;; Link handler

(defvar ewm-link-handlers
  `(("Copy URL" . ,(lambda (url) (kill-new url)))
    ("Open via GlideFox" . "glide")
    ("Download Files via Aria2c" . "aria2c -j 6 -x 16 -c -d ~/Downloads")
    ("Media YT Vid Download" . "yt-dlp --embed-metadata --embed-subs -f 'bestvideo[height<=1080]+bestaudio' -P ~/Videos")
    ("Media Audio Music Download" . "yt-dlp -P ~/Music -icx --embed-metadata")
    ("Podcast Listen Stream Song Music Mpd" . ,(lambda (url)
                                                 (let ((choice (completing-read "Type: " '("Song" "Podcast"))))
                                                   (if (string= choice "Podcast")
                                                       (ewm-launcher--execute (format "mpv --ytdl-format=bestaudio --force-window --geometry=15%% --title=podcast --vid=1 '%s'" url) 'start-process)
                                                     (ewm-launcher--execute (format "mpc add \"$(yt-dlp -f bestaudio -g '%s')\"" url) 'start-process)))))
    ("View Image via swayimg" . ,(lambda (url)
                                   (let ((tmp "/tmp/ewm-image"))
                                     (url-copy-file url tmp t)
                                     (ewm-launcher--execute (format "swayimg %s" tmp) 'start-process))))
    ("Play Watch Stream HQ" . "mpv -quiet")
    ("Misc Download" . "aria2c -j 6 -x 10 -c -d ~/Videos")
    ("Bookmark Url with tags" . "d-bookmark")
    ("Open via Brave" . "brave")
    ("Open via Qutebrowser" . "qutebrowser --target=tab-silent")
    ("Open via Chromium" . "chromium")
    ("Torrent files via Aria" . ,(lambda (url)
                                   (ewm-launcher--execute 
                                    (format "curl http://localhost:6800/jsonrpc -d '{\"jsonrpc\":\"2.0\",\"id\":\"someID\",\"method\":\"aria2.addUri\",\"params\":[\"token:ariatest\",[\"%s\"]]}'" url) 'start-process)))
    ("Youtube Search Play Music" . ,(lambda (url)
                                      (ewm-launcher--execute (format "mpc add \"$(yt-dlp -g 'ytsearch:%s')\"" url) 'start-process)))
    ("Open Search Engine" . "d-search")
    ("Document Pdf/cbz Manga via Sioyek" . "sioyek")
    ("Open via LibreWolf" . "librewolf")
    ("Add Torrent via Transmission" . "transmission-remote -a")
    ("Play Watch Stream LQ" . "mpv --ytdl-format=18 -quiet")
    ("CLI Youtube DL with Format Options" . ,(lambda (url)
                                               (ewm-launcher--execute 
                                                (format "yt-dlp -F '%s' && read -p 'Choose format: ' fmt && yt-dlp -f $fmt '%s'" url url) 'async-shell-buffer)))
    ("Get BibTex biblio reference" . "d-bibtex"))
  "Alist of (label . command-or-function) for link handling.")

(defun ewm-handle-link (url)
  "Handle URL with selected action."
  (interactive
   (list (or (current-kill 0)
             (read-string "URL: "))))
  (let* ((choice (completing-read "Open with: " (mapcar #'car ewm-link-handlers)))
         (handler (cdr (assoc choice ewm-link-handlers))))
    (if (functionp handler)
        (funcall handler url)
      (ewm-launcher--execute (format "%s '%s'" handler url) 'start-process))))

(defun ewm-link-handler-unified ()
  "Select link from multiple sources and handle it."
  (interactive)
  (let* ((clip (current-kill 0))
         (source (completing-read
                  "Link source: "
                  (list (format "Clipboard: %s" clip)
                        "Firefox History"
                        "Bookmarks"
                        "Manual Entry")))
         (url (cond
               ((string-prefix-p "Clipboard:" source) clip)
               ((string= source "Firefox History") (d/firefox-history-insert t))
               ((string= source "Bookmarks") (d/insert-bookmark-link t))
               ((string= source "Manual Entry") (read-string "URL: ")))))
    (when url
      (ewm-handle-link url))))

;; -*- lexical-binding: t; -*-
(with-eval-after-load 'ewm
  (dolist (b '(
               ("s-x"  . "noctalia-shell ipc call sessionMenu toggle")
               ("s-y"  . "vicinae vicinae://extensions/vicinae/clipboard/history")
               ;; ("s-w"  . "d-urls")
               ;; ("s-o"  . "d-stuff")
               ("s-u"  . "d-menu")
               ("s-b"  . "noctalia-shell ipc call controlCenter toggle")
               ("s-D" . "vicinae toggle")
               ("s-b" . "noctalia-shell ipc call controlCenter toggle")
               ;; ("s-i" . "d-stuff")
               ("s-u" . "d-menu")
               ("s-<f5>" . "noctalia-shell ipc call wallpaper random")
               ;; ("s-W"  . "d-passentry")
               ;; ("s-Y"  . "d-passentry")
               ("s-B"  . "d-bookmark")
               ("s-<delete>" . "noctalia-shell ipc call plugin:screen-recorder toggle")
               ("s-T"  . "noctalia-shell ipc call notifications toggleHistory")
               ("C-s-p" . "noctalia-shell ipc call sessionMenu lockAndSuspend")
               ("C-S-s-l" . "noctalia-shell ipc call lockScreen lock")
               ("<print>" . "hyprshot -m region --clipboard-only")
               ("s-<print>" . "hyprshot -m region")
               ("M-<print>" . "hyprshot -m window -m active")
               ("C-<print>" . "hyprshot -m output")
               ("<AudioRaiseVolume>" . "noctalia-shell ipc call volume increase")
               ("<AudioLowerVolume>" . "noctalia-shell ipc call volume decrease")
               ("<AudioMute>" . "noctalia-shell ipc call volume muteOutput")
               ("<MonBrightnessUp>" . "noctalia-shell ipc call brightness increase")
               ("<MonBrightnessDown>" . "noctalia-shell ipc call brightness decrease")
               ))
    (let ((cmd (cdr b)))
      (define-key ewm-mode-map (kbd (car b))
                  (lambda ()
                    (interactive)
                    (ewm-launcher--execute cmd 'start-process))))))

(defun d/firefox-history-insert (&optional return)
  "Insert URL from Firefox history."
  (interactive)
  (require 'sqlite)
  (let* ((cache "/tmp/glidemacs-history")
         (db "~/.glide/glide/oollv4xx.default-glide/places.sqlite"))
    ;; build cache if missing
    (unless (file-exists-p cache)
      (let ((tmp (make-temp-file "ffhist" nil ".sqlite")))
        (copy-file db tmp t)
        (let ((conn (sqlite-open tmp)))
          (with-temp-file cache
            (dolist (row (sqlite-select conn
                                        "SELECT url,title FROM moz_places
                          ORDER BY last_visit_date DESC LIMIT 7000"))
              (let ((url (nth 0 row))
                    (title (or (nth 1 row) "")))
                (insert (format "%s — %s\n" title url)))))
          (sqlite-close conn))))
    (let* ((lines (with-temp-buffer
                    (insert-file-contents cache)
                    (split-string (buffer-string) "\n" t)))
           (choice (completing-read "Firefox history: " lines))
           (url (cadr (split-string choice " — "))))
      (if return url (insert url))
      )))
