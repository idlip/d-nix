;; I use Emacs on android too.
(defvar d/on-droid
  (memq system-type '(android))
  "Check if running on android phone.")

;; to throw custom.el separely
;; useful to use same config for android/pc
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

;; Initialize package sources
(require 'package)

(customize-set-variable 'package-archives
			`(("melpa" . "https://melpa.org/packages/")
			  ,@package-archives))

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

(use-package emacs
  :init
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (fset 'x-popup-menu #'ignore)
  :custom
  (default-frame-alist '((menu-bar-lines . 0)
       (tool-bar-lines . 0)
       (alpha-background . 100)))
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
  (tab-width 2)
  (debug-on-quit nil)
  (initial-major-mode 'org-mode)
  :config
  ;; Terminal emacs doesn't have it
  (when (fboundp 'set-fontset-font)
    ;; a workaround for old charsets
    (set-fontset-font t 'unicode "Noto Color Emoji" nil 'append)
    ))

(use-package frame
  :bind
  ("C-z" . nil)
  ("C-x C-z" . nil)
  :custom
  (initial-frame-alist '((vertical-scroll-bars)))
  (frame-resize-pixelwise t)
  (frame-inhibit-implied-resize t))

(use-package delsel
  :bind
  (:map mode-specific-map
  ("C-g" . minibuffer-keyboard-quit))
  :config
  (delete-selection-mode))

(use-package re-builder
  :custom
  (reb-re-syntax 'string))

(use-package saveplace
  :custom
  (save-place-mode t))

(use-package simple
  :defer 0.1
  :bind (("<f7>" . scratch-buffer)
   ("<escape>" . keyboard-quit)
   ("M-u" . upcase-dwim)
   ("M-l" . downcase-dwim)
   ("M-c" . capitalize-dwim))
  :custom
  (kill-ring-max 30000)
  (column-number-mode 1)
  ;; (interprogram-cut-function (unless d/on-droid wl-copy))
  ;; (interprogram-paste-function (unless d/on-droid wl-paste))
  (kill-do-not-save-duplicates t)

  :config
  (global-visual-line-mode 1)

  ;; For wayland Pgtk build
  ;; credit: yorickvP on Github
  (defun wl-copy (text)
    (setq wl-copy-process (make-process :name "wl-copy"
          :buffer nil
          :noquery t
          :command '("wl-copy" "-f" "-n")
          :connection-type 'pipe))
    (process-send-string wl-copy-process text)
    (process-send-eof wl-copy-process))

  (defun wl-paste ()
    (if (and wl-copy-process (process-live-p wl-copy-process))
  nil ; should return nil if we're the current paste owner
(shell-command-to-string "wl-paste -n | tr -d \r")))


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
  )

(use-package display-line-numbers
  :custom
  (display-line-numbers-type 'relative))

(use-package misc
  :bind ("M-z" . zap-up-to-char))

(use-package ibuffer
  :bind
  ([remap list-buffers] . ibuffer))

(use-package xref
  :custom
  (xref-search-program 'ripgrep))

(use-package paragraphs
  :custom
  (sentence-end-double-space nil)
  (sentence-end "[.?!] "))

(use-package files
  :hook
  (before-save . delete-trailing-whitespace)
  :bind ("<f5>" . d/refresh-buffer)
  :custom
  (require-final-newline t)
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
  (create-lockfiles nil)

  :config
  (defun d/refresh-buffer ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm)))

(use-package vc-backup
  :custom
  (vc-make-backup-files t)
  (vc-follow-symlinks t))

(use-package savehist
  :defer 2
  :init
  (savehist-mode)
  :custom
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring)))

(use-package autorevert
  :defer 0.1)

(use-package recentf
  :demand t
  :custom
  (recentf-auto-cleanup 30)
  :config
  (recentf-mode)
  (run-with-idle-timer 30 t 'recentf-save-list))

(use-package no-littering
  :demand t
  :ensure t
  :custom
  (no-littering-etc-directory (expand-file-name "config/" user-emacs-directory))
  (no-littering-var-directory (expand-file-name "data/" user-emacs-directory))
  :config
  (add-to-list 'recentf-exclude
   (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
   (recentf-expand-file-name no-littering-etc-directory)))

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
         ("C-c f e" . (lambda () (interactive) (find-file "~/d-git/d-nix/d-emacs.org")))
         ("C-c f s" . (lambda () (interactive) (find-file "~/d-git/d-nix/d-setup.org")))
         ("C-c f m" . (lambda () (interactive) (find-file "~/d-git/d-nix/README.org"))))
  (:map dired-mode-map
        ("q" . kill-buffer-and-window)
        ("j" . dired-next-line)
        ("k" . dired-previous-line)
        ("l" . dired-find-file)
        ("h" . dired-up-directory)
        ("b" . d/external-browser))

  :custom
  (dired-listing-switches "-agho --group-directories-first")
  (delete-by-moving-to-trash t)
  (dired-dwim-target t))

;; Battery pack
(unless d/on-droid
  (use-package dirvish
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
     ("TAB" . dirvish-subtree-toggle)
     ("M-f" . dirvish-history-go-forward)
     ("M-b" . dirvish-history-go-backward)
     ("M-l" . dirvish-ls-switches-menu)
     ("M-m" . dirvish-mark-menu)
     ("M-t" . dirvish-layout-toggle)
     ("M-s" . dirvish-setup-menu)
     ("M-e" . dirvish-emerge-menu)
     ("M-j" . dirvish-fd-jump)))
  )

(use-package dired-x
  :ensure nil
  :custom
  ;; Make dired-omit-mode hide all "dotfiles"
  (dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'\\|^\\..*$"))

(use-package tramp
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

;; You will most likely need to adjust this font size for your system!

(defvar d/font-size (if d/on-droid 150 170)
  "Default font size based on the system.")
(defvar d/variable-font-size (if d/on-droid 160 190)
  "Default variable pitch size")

;; Dont worry about the font name, I use fork of Recursive font

;; Set reusable font name variables
(defvar d/fixed-pitch-font "Code D OnePiece"
  "The font to use for monospaced (fixed width) text.")

(defvar d/variable-pitch-font "Code D Ace"
  "The font to use for variable-pitch (documents) text.")

(setq haki-heading-font "Code D Zoro")
(setq haki-sans-font "Code D Haki")
(setq haki-title-font "Code D Law")
(setq haki-link-font "Maple Mono")
(setq haki-code-font "Code D Lip")

(use-package faces
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
  :defer t
  :custom ((font-lock-maximum-decoration t)
     (font-lock-global-modes '(not text-mode))
     (font-lock-verbose nil))
  :config
  (set-language-environment "UTF-8")
  (global-font-lock-mode 1))

(use-package mwheel
  :custom
  (mouse-wheel-scroll-amount '(1
			 ((shift) . 5)
			 ((control))))
  (mouse-wheel-progressive-speed nil))

(use-package pixel-scroll
  :config
  (pixel-scroll-mode))

(use-package tooltip
  :defer t
  :custom
  (tooltip-mode -1))

(use-package time
  :defer t
  :custom
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  (display-time-mode t)
  (display-time-format "%H:%M"))

(use-package winner
  :config
  (winner-mode 1))


(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package rainbow-mode
  :defer t
  :hook '(prog-mode help-mode)
  :bind ("C-c t c" . rainbow-mode))

(use-package so-long
  :config (global-so-long-mode))

(use-package image-mode
  :ensure nil
  :unless d/on-droid
  :bind (:map image-mode-map
	("q" . d/kill-buffer))
  :hook
  (image-mode . (lambda () (olivetti-mode) (setq olivetti-body-width 0.45))))

(use-package select
  :custom
  (selection-coding-system 'utf-8)
  (x-select-request-type 'text/plain\;charset=utf-8)
  (select-enable-clipboard t "Use the clipboard"))

(use-package man
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
  :defer t
  :custom-face
  (woman-bold ((t (:inherit font-lock-type-face :bold t))))
  (woman-italic ((t (:inherit font-lock-keyword-face :underline t)))))

(use-package which-key
  :defer 2
  :unless d/on-droid
  :custom
  (which-key-show-transient-maps t)
  (which-key-side-window-location 'bottom)
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 6)
  (which-key-side-window-slot -10)
  (which-key-side-window-max-height 0.25)
  (which-key-idle-delay 0.8)
  (which-key-max-description-length 25)
  (which-key-allow-imprecise-window-fit t)
  (which-key-separator " → " )
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package helpful
  :hook (helpful-mode . hide-mode-line-mode)
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

;; From kathink. It repeats the seq without modifier
(defun repeated-prefix-help-command ()
  (interactive)
  (when-let* ((keys (this-command-keys-vector))
	(prefix (seq-take keys (1- (length keys))))
	(orig-keymap (key-binding prefix 'accept-default))
	(keymap (copy-keymap orig-keymap))
	(exit-func (set-transient-map keymap t #'which-key-abort)))
    (define-key keymap [remap keyboard-quit]
		(lambda () (interactive) (funcall exit-func)))
    (which-key--create-buffer-and-show nil keymap)))

(setq prefix-help-command #'repeated-prefix-help-command)

(use-package shr-tag-pre-highlight
  :ensure t
  ;;:defer t
  :after shr
  :custom
  (shr-external-rendering-functions
   '((pre . shr-tag-pre-highlight))))

(use-package shr
  :defer t
  :custom
  (shr-use-fonts  t)
  (shr-use-colors nil)
  (shr-indentation 4)
  (shr-max-width 120)
  (shr-max-image-proportion 0.4)
  (shr-width nil))

(use-package shr-color
  :defer t
  :custom
  (shr-color-visible-luminance-min 80 "Improve the contrast"))

(use-package eww
  :defer t
  :custom
  (eww-search-prefix "https://duckduckgo.com/html/?kd=-1&q="))

(use-package browse-url
  :bind
  ([f5] . browse-url))

(use-package window
  :bind ("M-o" . other-window)
  ("C-<tab>" . other-window)
  ("C-x C-k" . d/kill-buffer)

  :custom
  (recenter-positions '(top middle bottom))

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

  (defun d/kill-buffer ()
    "Clear the image cache (to release memory) after killing a pdf buffer."
    (interactive)
    (if (one-window-p) (kill-this-buffer)
(kill-buffer-and-window))
    (when (derived-mode-p 'doc-view-mode) (progn (clear-image-cache) (doc-view-clear-cache))))
  )

(unless d/on-droid
  (use-package olivetti
    :defer t
    :hook ((text-mode         . olivetti-mode)
	   ;; (prog-mode         . olivetti-mode)
	   (Info-mode         . olivetti-mode)
	   ;; (eshell-mode         . olivetti-mode)
	   (helpful-mode         . olivetti-mode)
	   (Info-mode         . olivetti-mode)
	   (org-mode          . olivetti-mode)
	   (ement-room-mode   . olivetti-mode)
	   (dashboard-mode    . olivetti-mode)
	   (eww-mode          . olivetti-mode)
	   (sdcv-mode         . olivetti-mode)
	   (fundamental-mode  . olivetti-mode)
	   (nov-mode          . olivetti-mode)
	   (markdown-mode     . olivetti-mode)
	   (mu4e-view-mode    . olivetti-mode)
	   (elfeed-show-mode  . olivetti-mode)
	   (mu4e-compose-mode . olivetti-mode))
    :custom
    (olivetti-body-width 0.9)
    (olivetti-minimum-body-width 76)
    (olivetti-recall-visual-line-mode-entry-state t)
    :delight " ⊛")

  (use-package doom-modeline
    :init (doom-modeline-mode 1)
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
    (doom-modeline-battery nil)
    (doom-modeline-env-version t)
    (doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
    (doom-modeline-env-load-string "...")

    (doom-modeline-height 30)
    (doom-modeline-buffer-encoding nil))

  (use-package hide-mode-line
    :defer t
    :bind
    ("<f9>" . hide-mode-line-mode))

  )

;; My own theme
(add-to-list 'custom-theme-load-path "~/.config/emacs/var/theme/")
(load-theme 'haki t)
;; (add-hook 'post-command-hook #'haki-modal-mode-line)

;; For foot to show colors properly
(add-to-list 'term-file-aliases '("foot" . "xterm"))

(use-package modus-themes
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t)
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

(use-package nerd-icons
  :custom
  (nerd-icons-font-family d/fixed-pitch-font))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  (kind-icon-default-style '(:padding -0.5 :stroke 0 :margin 0 :radius 0 :height 0.6 :scale 1.0))
  (kind-icon-use-icons nil)
  (kind-icon-mapping
   `(
     (array ,(nerd-icons-codicon "nf-cod-symbol_array") :face font-lock-type-face)
     (boolean ,(nerd-icons-codicon "nf-cod-symbol_boolean") :face font-lock-builtin-face)
     (class ,(nerd-icons-codicon "nf-cod-symbol_class") :face font-lock-type-face)
     (color ,(nerd-icons-codicon "nf-cod-symbol_color") :face success)
     (command ,(nerd-icons-codicon "nf-cod-terminal") :face default)
     (constant ,(nerd-icons-codicon "nf-cod-symbol_constant") :face font-lock-constant-face)
     (constructor ,(nerd-icons-codicon "nf-cod-triangle_right") :face font-lock-function-name-face)
     (enummember ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
     (enum-member ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
     (enum ,(nerd-icons-codicon "nf-cod-symbol_enum") :face font-lock-builtin-face)
     (event ,(nerd-icons-codicon "nf-cod-symbol_event") :face font-lock-warning-face)
     (field ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-variable-name-face)
     (file ,(nerd-icons-codicon "nf-cod-symbol_file") :face font-lock-string-face)
     (folder ,(nerd-icons-codicon "nf-cod-folder") :face font-lock-doc-face)
     (interface ,(nerd-icons-codicon "nf-cod-symbol_interface") :face font-lock-type-face)
     (keyword ,(nerd-icons-codicon "nf-cod-symbol_keyword") :face font-lock-keyword-face)
     (macro ,(nerd-icons-codicon "nf-cod-symbol_misc") :face font-lock-keyword-face)
     (magic ,(nerd-icons-codicon "nf-cod-wand") :face font-lock-builtin-face)
     (method ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
     (function ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
     (module ,(nerd-icons-codicon "nf-cod-file_submodule") :face font-lock-preprocessor-face)
     (numeric ,(nerd-icons-codicon "nf-cod-symbol_numeric") :face font-lock-builtin-face)
     (operator ,(nerd-icons-codicon "nf-cod-symbol_operator") :face font-lock-comment-delimiter-face)
     (param ,(nerd-icons-codicon "nf-cod-symbol_parameter") :face default)
     (property ,(nerd-icons-codicon "nf-cod-symbol_property") :face font-lock-variable-name-face)
     (reference ,(nerd-icons-codicon "nf-cod-references") :face font-lock-variable-name-face)
     (snippet ,(nerd-icons-codicon "nf-cod-symbol_snippet") :face font-lock-string-face)
     (string ,(nerd-icons-codicon "nf-cod-symbol_string") :face font-lock-string-face)
     (struct ,(nerd-icons-codicon "nf-cod-symbol_structure") :face font-lock-variable-name-face)
     (text ,(nerd-icons-codicon "nf-cod-text_size") :face font-lock-doc-face)
     (typeparameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
     (type-parameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
     (unit ,(nerd-icons-codicon "nf-cod-symbol_ruler") :face font-lock-constant-face)
     (value ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-builtin-face)
     (variable ,(nerd-icons-codicon "nf-cod-symbol_variable") :face font-lock-variable-name-face)
     (t ,(nerd-icons-codicon "nf-cod-code") :face font-lock-warning-face)))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package vertico
  :bind (:map vertico-map
	("?" . minibuffer-completion-help)
	("<return>" . vertico-directory-enter)
	("DEL" . vertico-directory-delete-char)
	("M-DEL" . vertico-directory-delete-word)
	("M-j" . vertico-quick-exit)
	("'" . vertico-quick-exit)
	("C-v" . vertico-scroll-up)
	("M-v" . vertico-scroll-down)
	("M-q" . d/vertico-toggle)
	("M-RET" . minibuffer-force-complete-and-exit)
	("M-TAB" . minibuffer-complete))
  :init
  (vertico-mode)
  :custom
  (vertico-scroll-margin 5)
  (vertico-count 10)
  (vertico-resize t)
  (vertico-cycle t)
  (completion-in-region-function
   (lambda (&rest args)
     (apply (if vertico-mode
		#'consult-completion-in-region
	#'completion--in-region)
	    args))))

(use-package consult
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
         ("M-s d" . consult-find)
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
                         eww-bookmarks))))
  (add-to-list 'consult-buffer-sources 'consult--source-eww 'append)

  (defun consult-colors--web-list nil
    "Return list of CSS colors for `d/colors-web'."
    (require 'shr-color)
    (sort (mapcar #'downcase (mapcar #'car shr-color-html-colors-alist)) #'string-lessp))

  (defun d/colors-web (color)
    "Show a list of all CSS colors.\

  You can insert the name (default), or insert or kill the hexadecimal or RGB value of the
  selected color."
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

You can insert the name (default), or insert or kill the hexadecimal or RGB value of the
selected color."
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

  (setq add-unicodes (unless d/on-droid (directory-files "~/d-git/d-bin/treasure/unicodes/" t "i"))))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles orderless basic partial-completion)))
  (orderless-component-separator #'orderless-escapable-split-on-space)
  (orderless-style-dispatchers (list #'+orderless-consult-dispatch
				    #'orderless-affix-dispatch))))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package embark
  :defer t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
(use-package embark-consult
  :defer t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :defer 1
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-preview-current t)    ;; Disable current candidate preview
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-quit-no-match t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
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
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  (corfu-echo-mode)
  (global-corfu-mode))

(eldoc-add-command #'corfu-insert)
(unless (display-graphic-p)
  (corfu-terminal-mode +1))

;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
	 ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; Add extensions
(use-package cape
  :after corfu
  :bind (("C-c p p" . completion-at-point)
	 ("C-c p t" . complete-tag)
	 ("C-c p d" . cape-dabbrev)
	 ("C-c p h" . cape-history)
	 ("C-c p f" . cape-file)
	 ("C-c p k" . cape-keyword)
	 ("C-c p s" . cape-symbol)
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
  ;; (add-to-list 'completion-at-point-functions #'cape-tex)
  ;; (add-to-list 'completion-at-point-functions #'cape-sgml)
  ;; (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  :config

  ;; Silence the pcomplete capf, no errors or messages!
  ;; Important for corfu
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

  ;; Add your own file with all words
  (defcustom cape-dict-file "~/.local/share/dict/vocab"
    "Dictionary word list file."
    :type 'string)

  (defun corfu-enable-always-in-minibuffer ()
    "Enable corfu in minibuffer, if vertico is not active"
    (unless (or (bound-and-true-p mct--active)
		(bound-and-true-p vertico--input)
		(eq (current-local-map) read-passwd-map))
(setq-local corfu-auto t
		  corfu-popupinfo-delay nil
		  corfu-auto-delay 0
		  corfu-auto-prefix 0
		  completion-styles '(orderless basic))
(corfu-mode 1)))
  ;; (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  )

;; Configure Tempel
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
  :after tempel
  )

(unless d/on-droid
(use-package jinx
  :init (global-jinx-mode)
  :bind ("M-$". jinx-correct)))

(use-package org
  :defer t
  :commands (org-capture org-agenda)
  :hook (org-mode . (lambda () (org-indent-mode 1)
                      (org-display-inline-images 0)
                      (variable-pitch-mode 1)))

  :bind (("C-c c c" . org-capture)
         ("C-c c d" . calendar)
         ("C-c t R" . d/bionic-region)
         ("C-c d a" . org-agenda)
         ("C-c t r" . d/bionic-read))

  :custom
  (org-src-window-setup 'current-window)
  (org-startup-indented nil)
  (org-image-actual-width 400)
  (org-startup-folded t)
  (org-ellipsis " ▾")
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-done 'note)
  (org-log-into-drawer t)

  (org-agenda-files
   '("~/d-sync/notes/tasks.org"
     "~/d-git/d-site/README.org"))

  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
     (sequence  "PLAN(p)" "REVIEW(v)" "|" "COMPLETED(c)" "CANC(k@)")))

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


  (org-capture-templates
   `(
     ("t" "Task" entry (file+olp "~/d-sync/notes/tasks.org" "One-Timer")
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

  ;; browser script
  (unless d/on-droid
    (browse-url-browser-function 'browse-url-generic
                                 browse-url-generic-program "d-stuff")
    (browse-url-secondary-browser-function 'browse-url-generic
                                           browse-url-generic-program "d-stuff"))

  :config
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

  ;; This is for managing nixos config
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

  (defun d/narrow-or-widen-dwim ()
    "If the buffer is narrowed, it widens. Otherwise, it narrows to region, or Org subtree."
    (interactive)
    (cond ((buffer-narrowed-p) (widen))
          ((region-active-p) (narrow-to-region (region-beginning) (region-end)))
          ((equal major-mode 'org-mode) (org-narrow-to-subtree))
          (t (error "Please select a region to narrow to"))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (calc . t)
     (latex . t) (C . t)
     (R . t) (shell . t) (python . t)))
  (push '("conf-unix" . conf-unix) org-src-lang-modes)
  )

(unless d/on-droid
  (use-package org-present
    :defer t
    :after org
    :bind (:map org-present-mode-keymap
		("<right>" . d/org-present-next-slide)
		("<left>" . d/org-present-previous-slide)
		("<up>" . d/org-present-up)
		("<f5>" . d/org-present-refresh))
    (:map org-mode-map
	  ("<f8>" . d/org-present-mode))
    :hook ((org-present-mode . d/org-present-enable-hook)
	   (org-present-mode-quit . d/org-present-disable-hook)
	   (org-present-after-navigate-functions . d/org-present-prepare-slide))
    :config


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
		(org-document-info . 2.5)
		(org-document-title . 2.5)
		(org-level-1 . 1.6)
		(org-level-2 . 1.5)
		(org-level-3 . 1.4)
		(org-level-4 . 1.3)
		(org-level-5 . 1.2)
		(org-level-6 . 1.1)
		(org-code . 1.4)
		(header-line . 2.5)
		(org-verbatim . 1.3)
		(variable-pitch . 1.2)
		(org-level-7 . 1.1)))
	(face-remap-add-relative (car face) :height (cdr face)))


(if (package-installed-p 'hide-mode-line)
	  (hide-mode-line-mode 1))

(org-display-inline-images)
(read-only-mode 1))

    (defun d/org-present-prepare-slide (buffer-name heading)
(org-overview)
(org-show-entry)
(org-show-children))

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

(load-theme 'haki t)
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
(org-present-run-after-navigate-functions))


    (defun d/org-present-refresh ()
(interactive)
(d/org-present-mode)
(d/org-present-mode))

    )
  )

(unless d/on-droid
  (use-package denote
    :defer t
    :hook ((find-file-hook . denote-link-buttonize-buffer)

	   (dired-mode . denote-dired-mode))
    :bind
    ("C-c n j" . d/my-journal)
    ("C-c n s" . denote)
    ("C-c n t" . denote-type)
    ("C-c n d" . denote-date)
    ("C-c n n" . denote-subdirectory)
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
    (denote-directory (expand-file-name "~/d-sync/connect/denote"))
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
		   '("n" "New note (with denote.el)" plain
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
    ))

(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  (org-agenda-finalize-hook . org-modern-agenda)

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

  (org-modern-star '("◉" "✪" "◈" "✿" "❂"))
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

(use-package eglot
  :defer t
  :ensure nil
  :unless d/on-droid
  :commands (eglot eglot-format eglot-managed-p eglot--major-mode)
  ;; (((web-mode rust-mode python-mode sh-mode c-mode c++-mode nix-mode) .
  ;; eglot-ensure)
  :custom
  (eglot-sync-connect 1)
  (eglot-connect-timeout 5)
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 45)
  (eglot-auto-display-help-buffer nil)

  :bind
  (:map eglot-mode-map
	("C-c l r" . eglot-rename)
	("C-c l a" . eglot-code-actions)
	("C-c l i" . consult-eglot-symbols)))
;;   :config
;;   (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
;;   (add-to-list 'eglot-server-programs '(bash-ts-mode . ("bash-language-server")))
;;   (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman"))))

;; taken from Robb Enzmann
(defun d/pyrightconfig-write (virtualenv)
  "Write a `pyrightconfig.json' file at the Git root of a project
with `venvPath' and `venv' set to the absolute path of
`virtualenv'.  When run interactively, prompts for a directory to
select."
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

(unless d/on-droid
  (use-package nix-mode
    :mode ("\\.nix\\'" "\\.nix.in\\'")
    :bind (:map nix-mode-map
		("C-c C-e" . nix-eval-line))
    :config
    (defun nix-eval-dwim ()
(interactive)
(let* ((start (line-beginning-position))
	     (end (line-end-position))
	     (region-string (buffer-substring (region-beginning) (region-end)))
	     (msg (format "%s" (if (use-region-p) region-string (buffer-substring start end)))))
	(pop-to-buffer "*Nix-REPL*")
	(insert msg)
	(comint-send-input)
	(other-window 1))))

  (use-package nix-drv-mode
    :ensure nix-mode
    :mode "\\.drv\\'")
  (use-package nix-shell
    :ensure nix-mode
    :commands (nix-shell-unpack nix-shell-configure nix-shell-build))
  (use-package nix-repl
    :ensure nix-mode
    :commands (nix-repl)))

(use-package markdown-mode
  :defer t
  :mode "\\.md\\'"
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
    (if (derived-mode-p 'markdown-view-mode) (markdown-mode) (markdown-view-mode))))

(unless d/on-droid
  (use-package ess
    :defer t
    :custom
    (ess-use-flymake nil)
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
				(ess-R-fl-keyword:F&T . t)))))
(use-package lisp)

(use-package elisp-mode
  :bind
  (:map emacs-lisp-mode-map
	("C-c C-d C-d" . describe-function)
	("C-c C-d d" . describe-function)
	("C-c C-k" . eval-buffer)))

(use-package treesit
  :ensure nil
  :mode ("\\.yaml\\'" . yaml-ts-mode)
  :custom
  (treesit-font-lock-level 4)
  (treesit-font-lock-feature-list t)
  (major-mode-remap-alist
   '((c-mode . c-ts-mode)
     (c++-mode . c++-ts-mode)
     (csharp-mode . csharp-ts-mode)
     (css-mode . css-ts-mode)
     (html-mode . html-ts-mode)
     (java-mode . java-ts-mode)
     (js-mode . js-ts-mode)
     (json-mode . json-ts-mode)
     (makefile-mode . makefile-ts-mode)
     ;; (org-mode . org-ts-mode) ;; not mature yet
     (python-mode . python-ts-mode)
     (typescript-mode . typescript-ts-mode)
     (sh-mode . bash-ts-mode)
     (ruby-mode . ruby-ts-mode)
     (rust-mode . rust-ts-mode)
     (toml-mode . toml-ts-mode)
     (yaml-mode . yaml-ts-mode))))

(unless d/on-droid
  (use-package magit
    :defer t
    :commands (magit-status magit-get-current-branch)
    :custom
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
    (magit-diff-refine-hunk t)))

(use-package ediff
  :custom
  (ediff-keep-variants nil)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-default))

(use-package flycheck
  :defer t
;; :init (global-flycheck-mode))
  :hook (prog-mode . flycheck-mode))

(unless d/on-droid
  (use-package vterm
    :defer t
    :hook (vterm-mode-hook .
			   (lambda ()
			     (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
			     (buffer-face-mode t)))
    :bind
    (("C-c d t" . multi-vterm)
     ("<f12>" . d/vt-toggle)
     ("C-c t v" . d/vt-toggle))
    (:map vterm-mode-map
	  ("<f12>" . d/vt-toggle)
	  ("C-c t v" . d/vt-toggle)
	  ("<f9>" . hide-mode-line-mode)
	  ("C-q" . vterm-send-next-key))
    :custom
    (vterm-shell "/etc/profiles/per-user/i/bin/zsh")

    :config
    (defun d/vt-toggle ()
"Minimal hack to toggle vterm."
(interactive)
(cond
 ((derived-mode-p 'vterm-mode) (if (one-window-p) (switch-to-prev-buffer) (delete-window)))
 ((one-window-p) (progn (split-window-below) (other-window 1) (multi-vterm-next)
			(if (package-installed-p 'hide-mode-line) (hide-mode-line-mode) nil) (shrink-window 7)))
 (t (progn (other-window 1)
		 (if (derived-mode-p 'vterm-mode) (delete-window)
		   (progn (other-window -1) (split-window-below) (other-window 1) (multi-vterm-next) (if (package-installed-p 'hide-mode-line) (hide-mode-line-mode) nil) (shrink-window 7))))))))

  (use-package multi-vterm
    :bind (:map vterm-mode-map
		("M-n" . multi-vterm-next)
		("M-p" . multi-vterm-prev))
    :custom
    (multi-vterm-dedicated-window-height-percent 30))
  )

(unless t
  (use-package meow
    :defer 2
    :custom
    (meow-expand-exclude-mode-list `(org-mode markdown-mode vterm-mode))
    :config
    (defun meow-setup ()
(setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
(setq meow-use-cursor-position-hack t)
(meow-motion-overwrite-define-key
 '("j" . meow-next)
 '("k" . meow-prev)
 '("<escape>" . ignore))
(meow-leader-define-key
 ;; SPC j/k will run the original command in MOTION state.
 '("j" . "H-j")
 '("k" . "H-k")
 ;; Use SPC (0-9) for digit arguments.
 '("1" . meow-digit-argument)
 '("2" . meow-digit-argument)
 '("3" . meow-digit-argument)
 '("4" . meow-digit-argument)
 '("5" . meow-digit-argument)
 '("6" . meow-digit-argument)
 '("7" . meow-digit-argument)
 '("8" . meow-digit-argument)
 '("9" . meow-digit-argument)
 '("0" . meow-digit-argument)
 '("/" . meow-keypad-describe-key)
 '("?" . meow-cheatsheet))
(meow-normal-define-key
 '("0" . meow-expand-0)
 '("9" . meow-expand-9)
 '("8" . meow-expand-8)
 '("7" . meow-expand-7)
 '("6" . meow-expand-6)
 '("5" . meow-expand-5)
 '("4" . meow-expand-4)
 '("3" . meow-expand-3)
 '("2" . meow-expand-2)
 '("1" . meow-expand-1)
 '("-" . negative-argument)
 '(";" . meow-reverse)
 '("," . meow-inner-of-thing)
 '("." . meow-bounds-of-thing)
 '("[" . meow-beginning-of-thing)
 '("]" . meow-end-of-thing)
 '("a" . meow-append)
 '("A" . meow-open-below)
 '("b" . meow-back-word)
 '("B" . meow-back-symbol)
 '("c" . meow-change)
 '("d" . meow-delete)
 '("D" . meow-backward-delete)
 '("e" . meow-next-word)
 '("E" . meow-next-symbol)
 '("f" . meow-find)
 '("g" . meow-cancel-selection)
 '("G" . meow-grab)
 '("h" . meow-left)
 '("H" . meow-left-expand)
 '("i" . meow-insert)
 '("I" . meow-open-above)
 '("j" . meow-next)
 '("J" . meow-next-expand)
 '("k" . meow-prev)
 '("K" . meow-prev-expand)
 '("l" . meow-right)
 '("L" . meow-right-expand)
 '("m" . meow-join)
 '("n" . meow-search)
 '("o" . meow-block)
 '("O" . meow-to-block)
 '("p" . meow-yank)
 '("q" . meow-quit)
 '("Q" . meow-goto-line)
 '("r" . meow-replace)
 '("R" . meow-swap-grab)
 '("x" . meow-kill)
 '("t" . meow-till)
 '("u" . meow-undo)
 '("U" . meow-undo-in-selection)
 '("v" . meow-visit)
 '("w" . meow-mark-word)
 '("W" . meow-mark-symbol)
 '("s" . meow-line)
 '("X" . meow-goto-line)
 '("y" . meow-save)
 '("Y" . meow-sync-grab)
 '("z" . meow-pop-selection)
 '("'" . repeat)
 '("<escape>" . ignore)))

    (setq meow-replace-state-name-list
	  '((normal . "")
	    (motion . "")
	    (keypad . "")
	    (insert . "")
	    (beacon . "")))

    (add-to-list 'meow-mode-state-list '(vterm-mode . insert))
    (add-to-list 'meow-mode-state-list '(eshell-mode . insert))
    (add-to-list 'meow-mode-state-list '(eww-mode . insert))
    (add-to-list 'meow-mode-state-list '(sdcv-mode . motion))


    ;;meow-thing-register THING INNER BOUNDS
    (meow-thing-register 'arrow '(pair ("<") (">")) '(pair ("<") (">")))
    (add-to-list 'meow-char-thing-table '(?a . arrow))

    (setq meow-use-clipboard t)
    (meow-setup)
    (meow-global-mode 1))
  )

(use-package reddigg
  :defer t
  :bind (("C-c d f" . reddigg-view-frontpage)
	 ("C-c d r" . reddigg-view-sub))
  :custom
  (org-link-elisp-confirm-function 'y-or-n-p)
  (reddigg-subs '(emacs linux nixos hyprland bioinformatics fossdroid piracy bangalore india indiaspeaks developersindia manga aww))
  (other-subs '(crazyfuckingvideos nextfuckinglevel manga anime animepiracy fossdroid commandline memes jokes
				   funnymemes rss holup))

  :custom
  (defun reddigg-view-sub ()
    "Prompt SUB and print its post list."
    (interactive)
    (let ((sub (completing-read "subreddit: " (-concat reddigg-subs other-subs))))
(reddigg--view-sub sub))))

(unless d/on-droid
  (use-package hnreader
    :defer t)

  (use-package devdocs-browser
    :custom
    (devdocs-browser-major-mode-docs-alist:
     '((c++-ts-mode "cpp")
 (c-ts-mode "c")
 (python-ts-mode "Python")
 (emacs-lisp-mode "elisp"))))

  ;; (use-package howdoyou)
  ;; (use-package undo-fu
  ;;   :bind ("C-M-r" . undo-fu-only-redo)
  ;;   ("C-z" . undo-fu-only-undo)
  ;;   ("C-S-z" . undo-fu-only-redo-all))

  (use-package mingus
    :defer t
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


  ;; (use-package wikinforg)

  (use-package webpaste
    :defer t
    :bind (("C-c C-p C-b" . webpaste-paste-buffer)
	   ("C-c C-p C-r" . webpaste-paste-region)
	   ("C-c C-p C-p" . webpaste-paste-buffer-or-region))
    :config
    (setq webpaste-provider-priority '("dpaste.org" "dpaste.com" "paste.mozilla.org"))
    ;; Require confirmation before doing paste
    (setq webpaste-paste-confirmation t))

  (use-package sdcv
    :defer t
    :hook (sdcv-mode . hide-mode-line-mode)
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
  )

(use-package undo-fu-session
  :init (undo-fu-session-global-mode)
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package vundo
  :defer t
  :bind (("C-x u" . vundo)
	 ("C-z" . undo-only)
	 ("C-S-z" . undo-redo)
	 ("C-M-r" . undo-redo)))

(unless d/on-droid
  (use-package ement
    :bind ("C-c a m" . d/ement-connect)
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
)

(use-package ox-hugo
  :unless d/on-droid
  :after ox
  :config
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
  )

(when d/on-droid
  ;; access phone storage as default
  ;; Better is to symlink file to ~/ itself
  ;;(setq default-directory "/storage/emulated/0/")
  (customize-set-variable 'touch-screen-precision-scroll t)
  (customize-set-variable 'touch-screen-display-keyboard t)
  (customize-set-variable 'browse-url-android-share t)
  ;; (setq use-dialog-box nil)
  )

;; (if (daemonp)
;;     (add-hook 'server-after-make-frame-hook
;; 	      (lambda ()
;; 		(setq doom-modeline-icon t)
;; 		  (d/set-font-faces)))
;;   (d/set-font-faces))
;; (setq doom-modeline-icon t)
;; (put 'narrow-to-region 'disabled nil)

(use-package shrface
  :hook ((eww-mode . shrface-mode)
	 (elfeed-show-mode . shrface-mode)
	 (nov-mode . shrface-mode))
  :bind (:map shrface-mode-map
	("<tab>" . shrface-outline-cycle)
	("<backtab>" . shrface-outline-cycle-buffer)
	("M-n" . shr-next-link)
	("M-p" . shr-previous-link)
	("C-j" . shrface-next-headline)
	("C-k" . shrface-previous-headline))
  :custom
  (shrface-item-bullet 8226)
  (shrface-bullets-bullet-list org-modern-star)
  (shrface-href-versatile t)
  :config
  (shrface-basic)
  (shrface-trial))

(unless d/on-droid
(use-package nov
  :hook (nov-mode . hide-mode-line-mode)
  (nov-mode . variable-pitch-mode)
  :mode ("\\.epub\\'" . nov-mode)
  :custom
  (nov-text-width nil)
  (nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
  (nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions))
  (nov-variable-pitch t))
)

(use-package doc-view
  :ensure nil
  ;; :mode ("\\.epub\\'" . doc-view-mode)
  :bind (:map doc-view-mode-map
	("M-g M-g" . doc-view-goto-page)
	("<f8>" . doc-view-presentation))
  :custom
  (doc-view-continuous t)
  (doc-view-mupdf-use-svg t)
  (doc-view-image-width 900)
  (large-file-warning-threshold 700000000)
  (image-cache-eviction-delay 3))

(use-package elfeed
  :defer t
  :hook (elfeed-show-mode . d/elfeed-ui)
  :bind ("C-c d e" . d/elfeed-open)
  ("C-c d b" . d/external-browser)
  (:map elfeed-show-mode-map
	("e" . elfeed-open-in-eww)
	("i" . d/bionic-read)
	("r" . elfeed-open-in-reddit)
	("m" . elfeed-toggle-show-star)
	("q" . d/elfeed-quit)
	("C-x C-k" . d/elfeed-quit)
	("P" . d/elfeed-add-podcast)
	("b" . d/external-browser))
  (:map elfeed-search-mode-map
	("m" . elfeed-toggle-star)
	("q" . d/elfeed-quit)
	("C-x C-k" . d/elfeed-quit)
	("U" . elfeed-update)
	("u" . elfeed-update-feed))
  :custom
  ;; (setq-default elfeed-search-filter "@1-week-ago--1-day-ago +unread -news +")
  (elfeed-search-filter "+unread +")
  (elfeed-search-date-format (if d/on-droid `("" 0 :left)  `("%d-%m 📰" 7 :left)))
  (elfeed-search-title-max-width 90
	elfeed-search-trailing-width 0)

  :config
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
    (setq-local header-line-format " ")
    (variable-pitch-mode))

  (defun d/elfeed-open ()
    "Wrapper to load the elfeed db from disk before opening"
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force))
  ;; (elfeed-update))

  ;;write to disk when quiting
  (defun d/elfeed-quit ()
    "Wrapper to save the elfeed db to disk before burying buffer"
    (interactive)
    (elfeed-db-save)
    (quit-window))

  ;; play podcasts
  (defun d/elfeed-add-podcast (enclosure-index)
    "Play the enclosure URL in Mpd using 'mingus'."
    (interactive (list (elfeed--enclosure-maybe-prompt-index elfeed-show-entry)))
    (with-no-warnings
(message (concat "Added: " (car (elt (elfeed-entry-enclosures elfeed-show-entry)
			    (- enclosure-index 1)))))
(mingus-add (car (elt (elfeed-entry-enclosures elfeed-show-entry)
			    (- enclosure-index 1))))))

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

  ;; face for starred articles
  (defface elfeed-search-star-title-face
    '((t :foreground "#f77"))
    "Marks a starred Elfeed entry.")

  (push '(star elfeed-search-star-title-face) elfeed-search-face-alist)
  (defun d/elfeed-org-mark ()
    "use org file as bookmark for elfeed entries.
Usable as favorites or bookmark."
    (when elfeed-show-entry
(let* ((link (elfeed-entry-link elfeed-show-entry))
	     (title (elfeed-entry-title elfeed-show-entry)))
	(org-store-link-props
	 :link link
	 :description title))))

  (add-hook 'org-store-link-functions
	    'private/org-elfeed-entry-store-link)

  (when d/on-droid
(define-key elfeed-show-mode-map (kbd "<volume-up>") #'elfeed-show-prev)
(define-key elfeed-show-mode-map (kbd "<volume-down>") #'elfeed-show-next)))

(use-package link-hint
  :defer t
  :bind
  ("C-c l o" . link-hint-open-link)
  ("C-c l c" . link-hint-copy-link))

(use-package avy
  :defer t
  :bind
  ("M-j" . avy-goto-char-timer)
  ("M-K" . avy-kill-region)
  ("C-S-k" . avy-kill-whole-line)
  :custom
  (avy-background t))

(use-package elfeed-org
  :after elfeed
  :config
  (elfeed-org)
  :custom
  (rmh-elfeed-org-files (list "~/d-git/d-nix/d-emacs.org")))

(defun readable-article ()
  (interactive)
  (eww-readable)
  ;; (d/bionic-read)
  (beginning-of-buffer))

(defun elfeed-open-in-eww ()
  "open elfeed entry in eww."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single))))
    (eww (elfeed-entry-link entry))
    (add-hook 'eww-after-render-hook #'readable-article)))

(defun elfeed-open-in-reddit ()
  "open elfeed entry in reddit"
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single))))
    (reddigg-view-comments (elfeed-entry-link entry)))
  (display-buffer-pop-up-window (reddigg--get-cmt-buffer) nil)
  (other-window))

(use-package eww
  :commands (eww eww-search-words)
  :hook (eww-mode . variable-pitch-mode)
  :bind ("M-s M-w" . eww-search-words)
  (:map eww-mode-map
	("e" . readable-article)
	("Q" . d/kill-buffer)
	("M-v" . d/scroll-up)
	("<return>" . eww-follow-link)
	("C-v" . d/scroll-down)
	("m" . elfeed-toggle-star)
	("b" . d/external-browser))
  :config
  (setq shr-bullet "• "
	shr-folding-mode t
	url-privacy-level '(email agent cookies lastloc))
  (setq url-user-agent "")
  (setq url-privacy-level 'paranoid)
  (url-setup-privacy-info)
  (setq url-mime-accept-string "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8 ")
  (setq url-mime-charset-string nil)
  (setq url-mime-language-string "en-US,en;q=0.5")
  (setq url-mime-encoding-string "gzip, deflate"))

(use-package gnutls
  :defer t
  :custom
  (gnutls-verify-error t))

(defun d/external-browser ()
  (interactive)
  (cond ((image-at-point-p) (kill-new (or (shr-url-at-point nil) (plist-get (cdr (image--get-image)) :file))))
	((or (thing-at-point 'url t) (dired-file-name-at-point) (shr-url-at-point nil)) (link-hint-copy-link-at-point))
	(t (link-hint-copy-link)))
  (let ((url (current-kill 0)))
    (if d/on-droid (browse-url url) (browse-url-generic url))))
