;;; init.el --- Initialization file -*- lexical-binding: t; -*-
;;
;; Filename: init.el
;; Description: Initialize Emacs (The GOAT-Editor)
;; Author: Dilip
;; Copyright © 2023 Dilip
;; Version: 0.7
;; URL: https://github.com/idlip/d-nix
;; Keywords: init emacs
;; Compatibility: emacs-version >= 29.1
;;
;; ---
;;
;;; Commentary:
;;
;; This is the init.el file for Pgtk Emacs (wayland)
;;
;; ---
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;; ---
;;
;;; Code:


;; BetterGC
(defvar better-gc-cons-threshold 134217728 ; 128mb
  "If you experience freezing, decrease this.
If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
            (makunbound 'file-name-handler-alist-original)))
;; -BetterGC

;; AutoGC
(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))
            (defun gc-minibuffer-setup-hook ()
              (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

            (defun gc-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold better-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))
;; -AutoGC

;; Initialize package sources
(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory)
      package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("elpa" . "https://elpa.gnu.org/packages/"))
      package-quickstart nil)

(setq package-archive-priorities
      '(("melpa" .  3)
        ("org" . 2)
        ("elpa" . 1)))

(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil) ; To prevent initializing twice
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))

(use-package use-package
  :ensure nil
  :custom
  (use-package-verbose t)
  (use-package-always-ensure t)  ; :ensure t by default
  (use-package-always-defer nil) ; :defer t by default
  (use-package-expand-minimally t)
  (use-package-enable-imenu-support t))

;; You will most likely need to adjust this font size for your system!

(defvar default-font-size (if d/on-droid 140 170))
(defvar default-variable-font-size (if d/on-droid 140 170))

;; Set reusable font name variables
(defvar d/fixed-width-font "ComicCodeLigatures Nerd Font"
  "The font to use for monospaced (fixed width) text.")

(defvar d/variable-width-font "ComicCodeLigatures Nerd Font"
  "The font to use for variable-pitch (documents) text.")

(setq haki-heading-font "FiraCode Nerd Font")
(setq haki-sans-font (if d/on-droid "FiraCode Nerd Font" "Iosevka Comfy Motion"))
;;  (setq haki-code-font "JetBrainsMono Nerd Font")
(setq haki-title-font "FiraCode Nerd Font")
(setq haki-link-font "VictorMono NF")
(setq haki-code-font "Maple Mono")

(setf use-default-font-for-symbols nil)
(set-fontset-font t 'unicode "Noto Color Emoji" nil 'append)

(defun d/set-font-faces ()
  (message "Setting faces!")
  (set-face-attribute 'default nil :family d/variable-width-font :weight 'medium :height default-font-size)

  ;; Set the fixed pitch face (monospace)
  (set-face-attribute 'fixed-pitch nil :family d/fixed-width-font :height default-font-size)

  ;; Set the variable pitch face (document text)
  (set-face-attribute 'variable-pitch nil :family d/variable-width-font :height default-variable-font-size :weight 'medium)
  (global-font-lock-mode 1)
  (setq font-lock-maximum-decoration t))

(use-package no-littering               ; Keep .emacs.d clean
  :custom
  (no-littering-var-directory (expand-file-name "data/" user-emacs-directory))
  (no-littering-etc-directory (expand-file-name "config/" user-emacs-directory))
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)

  ;; Move this in its own thing
  (setq
   create-lockfiles nil
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)

  (setq
   backup-directory-alist
   `((".*" . ,(no-littering-expand-var-file-name "backup/")))
   auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package gcmh
  :init (gcmh-mode 1)
  :config
  (setq
   gcmh-idle-delay 'auto ; default is 15s
   gcmh-auto-idle-delay-factor 10
   gcmh-high-cons-threshold (* 16 1024 1024)) ; 16mb
  :delight " Ⓖ")

(use-package savehist
  :defer 2
  :init
  (savehist-mode)
  :custom
  (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring)))

(defun split-and-follow-below ()
  "Basically to balance and change cursor to split window"
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-and-follow-right ()
  "Basically to balance and change cursor to split window"
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun d/refresh-buffer ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(defun window-focus-mode ()
  "Make the window focused, it can toggled in and out"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (set-register '_ (list (current-window-configuration)))
      (delete-other-windows))))

(defun d/edit-src-block ()
  "Makes editing src block focused in its respective major mode"
  (interactive)
  (if (org-src-edit-buffer-p)         (org-edit-src-abort)
    (progn (org-edit-special) (window-focus-mode))))

(defun d/insert-unicodes (add-unicodes)
  "Inserts unicode character (emoji/icons) from given files"
  (interactive (list add-unicodes))
  (insert
   (let* ((content
           (mapcar #'(lambda (file) (with-temp-buffer (insert-file-contents file) (split-string (buffer-string) "\n" t))) add-unicodes))
          (options (apply #'append content))
          (selected-item (completing-read "Choose Icon 󰨈: " options))
          (fields (split-string selected-item)))
     (car fields))))
(setq add-unicodes (unless d/on-droid (directory-files "~/d-git/d-bin/treasure/unicodes/" t "i")))

(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'tree))

(dolist (keybind '(("M-o" . other-window)
                   ("C-<tab>" . tab-next)

                   ;; Better scrolling (emacs 29)
                   ("M-v" . d/scroll-up)
                   ("C-v" . d/scroll-down)
                   ;; refresh/re-read buffer
                   ("<f5>" . d/refresh-buffer)
                   ;; insert color or nerd icons
                   ("C-c d i" . d/insert-unicodes)
                   ("C-c d c" . d/insert-colors)
                   ;; better splits
                   ("C-x 2" . split-and-follow-below)
                   ("C-x 3" . split-and-follow-right)
                   ;; regex replace
                   ("M-%" . query-replace-regexp)
                   ;; quick kill
                   ("C-x C-k" . d/kill-buffer)
                   ("C-x k" . kill-buffer)
                   ("<escape>" . keyboard-escape-quit)
                   ;; handy editing
                   ("M-z" . zap-up-to-char)
                   ("M-u" . upcase-dwim)
                   ("M-l" . downcase-dwim)
                   ("M-c" . capitalize-dwim)))
  (global-set-key (kbd (car keybind)) (cdr keybind)))

;; Get rid of annoyance
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-z"))

(use-package which-key
  :defer 2
  :init
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.8
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit t
        which-key-separator " → " )
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

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

(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package rainbow-mode
  :defer t
  :hook (prog-mode . rainbow-mode)
  :bind ("C-c t c" . rainbow-mode))

(unless d/on-droid
  (setq scroll-conservatively 101) ;; value greater than 100 gets rid of half page jumping
  (setq mouse-wheel-scroll-amount nil)
  (setq mouse-wheel-progressive-speed t) ;; accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse


  ;; For wayland Pgtk build
  ;; credit: yorickvP on Github
  (setq wl-copy-process nil)

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

  (setq interprogram-cut-function 'wl-copy)
  (setq interprogram-paste-function 'wl-paste))

(use-package vertico
  :bind (:map vertico-map
              ("?" . minibuffer-completion-help)
              ("RET" . vertico-directory-enter)
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
  (setq vertico-scroll-margin 5)
  (setq vertico-count 10)
  (setq vertico-resize t)
  (setq vertico-cycle t)
  :config
  (setq vertico-buffer-display-action '(display-buffer-in-direction
                                        (direction . right)
                                        (window-width . 0.45)))
  (setq vertico-multiform-categories
        '((file )
          (consult-location )
          (t unobtrusive)))
  (setq vertico-multiform-commands
        '((consult-ripgrep ))))

(defun d/vertico-toggle ()
  "Toggle between vertico-unobtrusive and vertico-mode."
  (interactive)
  (vertico-multiform-vertical 'vertico-unobtrusive-mode))

(use-package emacs
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (setq completion-cycle-threshold 3)
  (setq tab-always-indent t)
  (setq enable-recursive-minibuffers t))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ("C-c t t" . consult-theme)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x C-b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
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
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 1.5 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both  and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  )

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

(use-package orderless
  :demand t
  :config
  (defun +orderless--consult-suffix ()
    "Regexp which matches the end of string with Consult tofu support."
    (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
        (format "[%c-%c]*$"
                consult--tofu-char
                (+ consult--tofu-char consult--tofu-range -1))
      "$"))
  ;; Recognizes the following patterns:
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-consult-dispatch (word _index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" word)
      `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--consult-suffix))))
     ;; File extensions
     ((and (or minibuffer-completing-file-name
               (derived-mode-p 'eshell-mode))
           (string-match-p "\\`\\.." word))
      `(orderless-regexp . ,(concat "\\." (substring word 1) (+orderless--consult-suffix))))))

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        ;; completion-category-overrides '((file (styles orderless partial-completion))) ;; orderless is tried first
        completion-category-overrides '((file (styles partial-completion)) ;; partial-completion is tried first
                                        ;; enable initialism by default for symbols
                                        (command (styles +orderless-with-initialism))
                                        (variable (styles +orderless-with-initialism))
                                        (symbol (styles +orderless-with-initialism)))
        orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
        orderless-style-dispatchers (list #'+orderless-consult-dispatch
                                          #'orderless-affix-dispatch)))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  :init
  (marginalia-mode))

(use-package embark
  :defer t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
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

;; Add extensions
(use-package cape
  :after corfu
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
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
  )

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

;; Configure Tempel
(use-package tempel
  :after corfu
  :hook
  (prog-mode . tempel-abbrev-mode)

  ;; Require trigger prefix before template name when completing.
  :custom
  (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert)))

(use-package tempel-collection
  :after tempel
  )

(use-package org-modern
  :after org
  :config
  ;; (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

  ;; (set-face-attribute 'org-modern-symbol nil :family "Iosevka")

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
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   ;;   org-ellipsis "…"

   ;; Reference:
   ;; Heading: "◉ ○ ✸ ✿"
   ;; Cool-Heading: ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
   ;; Small: ► • ★ ▸
   ;; others: ▼, ↴, ⬎, ⤷,…, and ⋱.
   ;; (org-ellipsis "⤵")

   org-modern-star '("◉" "✪" "◈" "✿" "❂")
   org-modern-hide-stars 'leading
   org-modern-table t
   org-modern-list
   '((?* . "⁍")
     (?- . "❖")
     (?+ . "➤"))

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")

  (global-org-modern-mode))

(unless d/on-droid
(use-package jinx
  :defer t
  :hook (emacs-startup . global-jinx-mode)
  :bind ("M-$". jinx-correct)
  :config
  (add-to-list 'vertico-multiform-categories
               '(jinx grid indexed))
  (vertico-multiform-mode 1))
)

(defun org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (dolist (face '((org-block . 1.0)
                  (org-block-begin-line . 0.9)
                  (org-document-info . 1.5)
                  (org-document-title . 1.7)
                  (org-level-1 . 1.4)
                  (org-level-2 . 1.3)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-code . 1.2)
                  (header-line . 1.0)
                  (org-verbatim . 1.15)
                  (variable-pitch . 1.0)
                  (org-level-7 . 1.1)))
    (set-face-attribute (car face) nil :font d/fixed-width-font :weight 'medium :height (cdr face))))

;; Set faces for heading levels

;; (set-face-attribute (car face) nil :font d/header-font :weight 'regular :height (cdr face)))


(defun org-mode-setup ()
  (org-indent-mode 1)
  (org-display-inline-images 1)
  (variable-pitch-mode 1)
  ;; (org-font-setup)
  (setq
   org-startup-indented nil
   org-image-actual-width 400
   org-startup-folded t)
  )

(use-package org
  :pin org
  :defer t
  :commands (org-capture org-agenda)
  :hook (org-mode . org-mode-setup)
  (org-mode . org-modern-mode)

  :bind (("C-c c c" . org-capture)
         ("C-c c d" . calendar)
         ("C-c t R" . d/bionic-region)
         ("C-c d a" . org-agenda)
         ("C-c t r" . d/bionic-read)
         ("<f6>" . d/edit-src-block)
         :map org-mode-map
         ("C-c o b" . d/edit-src-block))
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  ;; (setq org-log-done 'time)
  (setq org-log-done 'note)
  (setq org-log-into-drawer t)

  ;; browser script
  (unless d/on-droid
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "d-stuff")
    (setq browse-url-secondary-browser-function 'browse-url-generic
          browse-url-generic-program "d-stuff"))

  (setq org-agenda-files
        (if d/on-droid "/storage/emulated/0/sync/org/tasks.org"
          '("~/sync/org/tasks.org"
            "~/d-git/d-site/README.org")))

  ;; (require 'org-habit)
  ;; (add-to-list 'org-modules 'org-habit)
  ;; (setq org-habit-graph-column 60)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence  "PLAN(p)" "REVIEW(v)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
        '(("Archive.org" :maxlevel . 1)
          ("tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
        '((:startgroup)
          (:endgroup)
          ("@work" . ?W)
          ("agenda" . ?a)
          ("linux" . ?l)
          ("planning" . ?p)
          ("note" . ?n)
          ("idea" . ?i)))


  (setq org-capture-templates
        `(
          ("t" "Task" entry (file+olp "~/sync/org/tasks.org" "One-Timer")
           "* TODO %?\n  SCHEDULED:%U\n  %a\n  %i" :empty-lines 1)
          ("w" "Website Todo" entry (file+headline "~/d-git/d-site/README.org" "Ideas - TODO")
           "* TODO %?\n  SCHEDULED:%T\n " :empty-lines 1)

          ("j" "Journal Entries")
          ("jj" "Journal" entry
           (file+olp+datetree "~/docs/org/journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1))))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (calc . t)
     (latex . t)
     (shell .t)
     (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("txt" . "src text"))
  (add-to-list 'org-structure-template-alist '("conf" . "src conf"))
  (add-to-list 'org-structure-template-alist '("nix" . "src nix"))
  (add-to-list 'org-structure-template-alist '("lx" . "src latex"))
  (add-to-list 'org-structure-template-alist '("cal" . "src calc")))

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
  ("C-c n n" . denote)
  ("C-c n N" . denote-type)
  ("C-c n d" . denote-date)
  ("C-c n s" . denote-subdirectory)
  ("C-c n t" . denote-template)
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

  :config
  (setq
   denote-directory (expand-file-name "~/sync/denote")
   denote-known-keywords '("emacs" "blogs" "article")
   denote-infer-keywords t
   denote-sort-keywords t
   denote-file-type nil ; Org is the default, set others here
   denote-prompts '(title keywords)
   denote-excluded-directories-regexp nil
   denote-excluded-keywords-regexp nil
   denote-date-prompt-use-org-read-date t
   denote-allow-multi-word-keywords t
   denote-date-format nil
   denote-backlinks-show-context t)
  denote-dired-directories
  (list denote-directory
        (thread-last denote-directory (expand-file-name "attachments"))
        (expand-file-name "~/sync/org/books/"))

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

  ))

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
  (olivetti-body-width 0.8)
  :delight " ⊛")
)

(unless d/on-droid
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  (setq doom-modeline-time-icon nil)
  (setq doom-modeline-bar-width 7)
  (setq doom-modeline-major-mode-icon t)
  (setq inhibit-compacting-font-caches t)
  :custom ((doom-modeline-height 30)
           (doom-modeline-buffer-encoding nil)))
)

;; to hide during presentation and writing
(use-package hide-mode-line
  :defer t
  :bind
  ("<f9>" . hide-mode-line-mode))

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-mixed-fonts t
      modus-themes-variable-pitch-ui t
      modus-themes-custom-auto-reload t
      modus-themes-disable-other-themes t
      modus-themes-prompts '(italic bold)
      modus-themes-org-blocks 'gray-background
      modus-themes-completions
      '((matches . (extrabold))
        (selection . (semibold italic text-also)))

      modus-themes-org-blocks 'gray-background

      modus-themes-headings
      '((1 . (variable-pitch 1.1))
        (2 . (1.1))
        (agenda-date . (1.2))
        (agenda-structure . (variable-pitch light 1.8))
        (t . (1.1))))

;; My own theme
(add-to-list 'custom-theme-load-path "~/.config/emacs/var/theme/")
(load-theme 'haki t)
(add-hook 'post-command-hook #'haki-meow-mode-line)
;; For foot to show colors properly
(add-to-list 'term-file-aliases '("foot" . "xterm"))

(setq-default scroll-conservatively 10000)
(setq-default scroll-margin 3)

(pixel-scroll-precision-mode t)

(defun d/scroll-down ()
  "Trust me, make scrolling alot smoother. +1 Makes you fall in love with Emacs again!"
  (interactive)
  (pixel-scroll-precision-scroll-down 20))

(defun d/scroll-up ()
  "Trust me, adds a wonderfull smooth scroll. You can do this by trackpad too (laptop)"
  (interactive)
  (pixel-scroll-precision-scroll-up 20))

(unless d/on-droid
  (use-package nix-mode
    :mode "\\.nix\\'"
    :defer t))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;;(add-hook 'prog-mode-hook #'eglot-ensure)

(use-package markdown-mode
  :defer t
  :mode "\\.md\\'"
  :config
  (defun d/set-markdown-header-font-sizes ()
    (dolist (face '((markdown-header-face-1 . 1.3)
                    (markdown-header-face-2 . 1.2)
                    (markdown-header-face-3 . 1.15)
                    (markdown-header-face-4 . 1.1)
                    (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :weight 'normal :font haki-heading-font :height (cdr face))))

  (defun d/markdown-mode-hook ()
    (d/set-markdown-header-font-sizes)))

;; (add-hook 'markdown-mode-hook 'd/markdown-mode-hook)

(use-package eglot
  :defer t
  :ensure nil
  :unless d/on-droid
  :commands (eglot eglot-format eglot-managed-p eglot--major-mode)
  ;; (((web-mode rust-mode python-mode sh-mode c-mode c++-mode nix-mode) .
  ;; eglot-ensure)
  :init
  (setq eglot-sync-connect 1
        eglot-connect-timeout 5
        eglot-autoshutdown t
        eglot-send-changes-idle-time 45
        ;; NOTE We disable eglot-auto-display-help-buffer because :select t in
        ;;      its popup rule causes eglot to steal focus too often.
        eglot-auto-display-help-buffer nil)
  :bind
  (:map eglot-mode-map
        ("C-c l r" . eglot-rename)
        ("C-c l a" . eglot-code-actions)
        ("C-c l i" . consult-eglot-symbols)))
;;   :config
;;   (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
;;   (add-to-list 'eglot-server-programs '(bash-ts-mode . ("bash-language-server")))
;;   (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman"))))

;; (use-package eglot-tempel
;;   :disabled t
;;   :load-path "~/.config/emacs/elpa/eglot-tempel")

(use-package treesit
  :ensure nil
  :custom
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

(use-package nerd-icons
  :custom
  (nerd-icons-font-family d/fixed-width-font))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (setq kind-icon-default-style '(:padding -0.5 :stroke 0 :margin 0 :radius 0 :height 0.6 :scale 1.0))
  (setq kind-icon-use-icons nil)
  (setq kind-icon-mapping
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
          (t ,(nerd-icons-codicon "nf-cod-code") :face font-lock-warning-face))))

(unless d/on-droid
(use-package magit
  :defer t
  :config
  ;; Show word-granularity differences within diff hunks
  (setq magit-diff-refine-hunk t)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
)

(unless d/on-droid
(use-package meow
  :defer 2
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

                                        ;meow-thing-register THING INNER BOUNDS
  (meow-thing-register 'arrow '(pair ("<") (">")) '(pair ("<") (">")))
  (add-to-list 'meow-char-thing-table '(?a . arrow))

  (setq meow-use-clipboard t)
  (meow-setup)
  (meow-global-mode 1))
)

(use-package dired
  :defer t
  :ensure nil
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

  :custom (dired-listing-switches "-agho --group-directories-first")
  :config
  (setq dired-listing-switches "-alt --dired --group-directories-first -h -G")
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode))))

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
  :config
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(file-time file-size collapse subtree-state vc-state git-msg))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")

  ;; with emacs29
  (setq dired-mouse-drag-files t)
  (setq mouse-drag-and-drop-region-cross-program t)
  (setq mouse-1-click-follows-link nil)

  :bind
  (("C-c f d" . dirvish-fd)
   ("C-x C-d" . dirvish)
   ("C-c f t" . dirvish-side)
   :map dirvish-mode-map
   ("<mouse-1>" . 'dirvish-subtree-toggle-or-open)    
   ("<mouse-2>" . 'dired-mouse-find-file-other-window)
   ("<mouse-3>" . 'dired-mouse-find-file)
   ("a"   . dirvish-quick-access)
   ("C-x C-k" . dirvish-quit)
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


(use-package tramp
  :config
  ;; Enable full-featured Dirvish over TRAMP on certain connections
  ;; https://www.gnu.org/software/tramp/#Improving-performance-of-asynchronous-remote-processes-1.
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/ssh:YOUR_HOSTNAME:")
                     "direct-async-process" t))
  ;; Tips to speed up connections
  (setq tramp-verbose 0)
  (setq tramp-chunksize 2000)
  (setq tramp-use-ssh-controlmaster-options nil))


(use-package dired-x
  :ensure nil
  :config
  ;; Make dired-omit-mode hide all "dotfiles"
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..*$")))

(unless d/on-droid
(use-package vterm
  :defer t
  :bind
  (("C-c d t" . multi-vterm)
   ("<f12>" . d/vt-toggle))
  (:map vterm-mode-map
        ("<f12>" . d/vt-toggle)
        ("<f9>" . hide-mode-line-mode)
        ("C-q" . vterm-send-next-key))
  :config
  (setq vterm-shell "/etc/profiles/per-user/i/bin/zsh")
  (defun d/vt-toggle ()
    "Minimal hack to toggle vterm."
    (interactive)
    (cond
     ((derived-mode-p 'vterm-mode) (if (one-window-p) (switch-to-prev-buffer) (delete-window)))
     ((one-window-p) (progn (split-and-follow-below) (multi-vterm-next)
                            (if (package-installed-p 'hide-mode-line) (hide-mode-line-mode) nil) (shrink-window 7)))
     (t (progn (other-window 1)
               (if (derived-mode-p 'vterm-mode) (delete-window) 
                 (progn (split-and-follow-below) (multi-vterm-next) (if (package-installed-p 'hide-mode-line) (hide-mode-line-mode) nil) (shrink-window 7))))))))

(use-package multi-vterm
  :bind (:map vterm-mode-map
              ("M-n" . multi-vterm-next)
              ("M-p" . multi-vterm-prev))
  :config
  (setq multi-vterm-dedicated-window-height-percent 30))
)

;; nixos issue for loading mu4e
;; (add-to-list 'load-path "/etc/profiles/per-user/i/share/emacs/site-lisp/mu4e/")

(unless d/on-droid
  (use-package reddigg
    :defer t
    :bind (("C-c d f" . reddigg-view-frontpage)
           ("C-c d r" . reddigg-view-sub))
    :config
    (setq org-confirm-elisp-link-function nil)
    (setq reddigg-subs '(bangalore india emacs fossdroid piracy aww)))

  (use-package hnreader
    :defer t)

  ;; (use-package howdoyou)
  ;; (use-package undo-fu
  ;;   :bind ("C-M-r" . undo-fu-only-redo)
  ;;   ("C-z" . undo-fu-only-undo)
  ;;   ("C-S-z" . undo-fu-only-redo-all))

  (use-package flycheck
    :defer t
    :hook (prog-mode . flycheck-mode))
  ;; :init (global-flycheck-mode))

  (use-package mingus
    :defer t
    :bind ("C-c d m" . mingus-browse)
    (:map mingus-browse-mode-map
          ("h" . mingus-browse-top-level)
          ("l" . mingus-down-dir-or-play-song))
    :config
    (advice-add 'mingus-playlist-mode :after #'olivetti-mode)
    (advice-add 'mingus-browse-mode :after #'olivetti-mode))
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
          '("wn" "mw-thesaurus" "enjp")
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

(defun shrface-default-keybindings ()
  (interactive)
  "Sets up the default keybindings for `shrface-mode'."
  (define-key shrface-mode-map (kbd "TAB") 'shrface-outline-cycle)
  (define-key shrface-mode-map (kbd "<backtab>") 'shrface-outline-cycle-buffer)
  (define-key shrface-mode-map (kbd "C-t") 'shrface-toggle-bullets)
  (define-key shrface-mode-map (kbd "C-j") 'shrface-next-headline)
  (define-key shrface-mode-map (kbd "C-k") 'shrface-previous-headline)
  (define-key shrface-mode-map (kbd "M-l") 'shrface-links-consult)
  (define-key shrface-mode-map (kbd "M-h") 'shrface-headline-consult))

(use-package shrface
  :defer t
  :hook ((eww-mode . shrface-mode)
         (elfeed-show-mode . shrface-mode)
         (nov-mode . shrface-mode))
  :init
  (setq shrface-item-bullet 8226)
  :bind (:map shrface-mode-map
              ("<tab>" . shrface-outline-cycle)
              ("<backtab>" . shrface-outline-cycle-buffer)
              ("M-l" . shrface-links-consult)
              ("M-h" . shrface-headline-consult)
              ("C-j" . shrface-next-headline)
              ("C-k" . shrface-previous-headline))
  :config
  (shrface-basic)
  (shrface-trial)
  (setq shrface-bullets-bullet-list org-modern-star)
  (setq shrface-href-versatile t))

;; To highligh src blocks in eww
(use-package shr-tag-pre-highlight
  :after shr
  :config

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
      (if light
          (add-face-text-property start end '(:background "#D8DEE9" :extend t))
        (add-face-text-property start end '(:background "#292b2e" :extend t)))
      (shr-ensure-newline)
      (insert "\n")))

  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight))
  (when (version< emacs-version "26")
    (with-eval-after-load 'eww
      (advice-add 'eww-display-html :around
                  'eww-display-html--override-shr-external-rendering-functions))))

(use-package doc-view
  :ensure nil
  :mode ("\\.epub\\'" . doc-view-mode)
  :custom
  (doc-view-continuous t)
  (doc-view-image-width 900))

(defun config-reload ()
  "Uncle dev created a function to reload Emacs config."
  (interactive)
  (load-file (expand-file-name "~/.config/emacs/init.el")))


(defun d/kill-buffer ()
  "Clear the image cache (to release memory) after killing a pdf buffer."
  (interactive)
  (if (one-window-p) (kill-this-buffer)
    (kill-buffer-and-window))
  (clear-image-cache)
  (doc-view-clear-cache))
  ;; (unless d/on-droid (pdf-cache-clear-data)))

(use-package image-mode
  :ensure nil
  :unless d/on-droid
  :bind (:map image-mode-map
              ("q" . d/kill-buffer))
  :hook
  (image-mode . (lambda () (olivetti-mode) (setq olivetti-body-width 0.45))))

(use-package man
  :bind (("C-c m" . consult-man)
         :map Man-mode-map
         ("q" . kill-buffer-and-window)))

;; Bionic Reading

(defvar bionic-reading-face nil "a face for `d/bionic-region'.")
(setq bionic-reading-face 'bold)
;; try
;; 'bold
;; 'error
;; 'warning
;; 'highlight
;; or any value of M-x list-faces-display

(defun d/bionic-read ()
  "Bold the first few chars of every word in current buffer.
      Version 2022-05-21"
  (interactive)
  (read-only-mode -1)
  (d/bionic-region (point-min) (point-max))
  (read-only-mode 1)
  (beginning-of-buffer))

(defun d/bionic-region (Begin End)
  "Bold the first few chars of every word in region.
      Version 2022-05-21"
  (interactive "r")
  (let (xBounds xWordBegin xWordEnd  )
    (save-restriction
      (narrow-to-region Begin End)
      (goto-char (point-min))
      (while (forward-word)
        ;; bold the first half of the word to the left of cursor
        (setq xBounds (bounds-of-thing-at-point 'word))
        (setq xWordBegin (car xBounds))
        (setq xWordEnd (cdr xBounds))
        (setq xBoldEndPos (+ xWordBegin (1+ (/ (- xWordEnd xWordBegin) 2))))
        (put-text-property xWordBegin xBoldEndPos
                           'font-lock-face bionic-reading-face)))))

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
        ("b" . d/external-browser))
  (:map elfeed-search-mode-map
        ("m" . elfeed-toggle-star)
        ("q" . d/elfeed-quit)
        ("C-x C-k" . d/elfeed-quit)
        ("U" . elfeed-update)
        ("u" . elfeed-update-feed))
  :config
  ;; (setq-default elfeed-search-filter "@1-week-ago--1-day-ago +unread -news +")
  (setq-default elfeed-search-filter "+unread +")
  (setq elfeed-search-date-format `("%m-%d 📰" 7 :left))
  (setq elfeed-search-title-max-width 90
        elfeed-search-trailing-width 0)
  (defalias 'elfeed-toggle-show-star
    (elfeed-expose #'elfeed-show-tag 'star))
  (defalias 'elfeed-toggle-star
    (elfeed-expose #'elfeed-search-toggle-all 'star))

  (defun d/elfeed-ui ()
    (interactive)
    (setq-local header-line-format " "))

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

  ;; face for starred articles
  (defface elfeed-search-star-title-face
    '((t :foreground "#f77"))
    "Marks a starred Elfeed entry.")

  (push '(star elfeed-search-star-title-face) elfeed-search-face-alist))

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
  :config
  (setq avy-background t))

(use-package elfeed-org
  :after elfeed
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.config/emacs/elfeed.org")))

(defun readable-article ()
  (interactive)
  (eww-readable)
  ;; (d/bionic-read)
  (beginning-of-buffer))

(defun elfeed-open-in-eww ()
  "open in eww"
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single))))
    (eww (elfeed-entry-link entry))
    (add-hook 'eww-after-render-hook 'readable-article)))

(defun elfeed-open-in-reddit ()
  "open in reddit"
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single))))
    (reddigg-view-comments (elfeed-entry-link entry))))

(use-package eww
  :bind (:map eww-mode-map
              ("e" . readable-article)
              ("Q" . d/kill-buffer)
              ("M-v" . d/scroll-up)
              ("C-v" . d/scroll-down)
              ("F" . d/visit-urls)
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
  (setq url-mime-encoding-string "gzip, deflate")

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
  (add-to-list 'consult-buffer-sources 'consult--source-eww 'append))

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
    (browse-url-generic url)))

;; (advice-add 'eww-readable :after #'d/bionic-read)

(unless d/on-droid
(use-package ement
  :bind (:map ement-room-minibuffer-map
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
  :ensure nil
  :unless d/on-droid
  :after ox)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(setq set-fringe-style "default")        ; Give some breathing room

;; (setq-default mode-line-format nil)

;; (server-start)

(global-visual-line-mode 1)

;; Display messages when idle, without prompting
(setq help-at-pt-display-when-idle t)
(setopt read-quoted-char-radix 16)
(setopt set-mark-command-repeat-pop t)
(setq sentence-end-double-space nil)
(setq sentence-end "[.?!] ")
(setq-default indent-tabs-mode nil)
(setq save-interprogram-paste-before-kill nil) ;; #FIXE
(setq kill-do-not-save-duplicates t)
(setq initial-scratch-message
      ";; Type to your Will !\n\n")

(setq frame-inhibit-implied-resize t)
;;(global-prettify-symbols-mode t)

;; tabs
(setq tab-bar-new-tab-choice "*scratch")
(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil)

(setq vc-follow-symlinks t)

;; Set up the visible bell
(setq visible-bell nil)

;; Wayland
(setq x-select-request-type 'text/plain\;charset=utf-8)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

(electric-pair-mode t)
(setq electric-pair-inhibit-predicate 'ignore)
(setq electric-pair-skip-self t)

(setq recenter-positions '(top middle bottom))
(global-display-line-numbers-mode t)
(setq  display-line-numbers-type 'relative)
(setq text-scale-mode-step 1.1)
(setq frame-resize-pixelwise t)
(global-hl-line-mode 1)
(column-number-mode -1)
(line-number-mode 1)
(delete-selection-mode +1)
(save-place-mode t)

(setq reb-re-syntax 'string)
;;(display-battery-mode t)
;;(setq display-time;5;9~-default-load-average nil)
;;(setq display-time-24hr-format t)
;;(setq display-time-format "%H:%M")
;;(display-time-mode t)
;;(toggle-truncate-lines t)

(setq
 shr-use-fonts  t                          ; No special fonts
 shr-use-colors t                          ; No colours
 shr-indentation 4                           ; Left-side margin
 shr-max-width fill-column
 shr-max-image-proportion 0.4
 shr-width 100                                ; Fold text to 70 columns
 url-privacy-level '(email agent cookies lastloc)
 eww-search-prefix "https://lite.duckduckgo.com/lite/?q=")

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha-background 100)
(add-to-list 'default-frame-alist `(alpha-background . 100))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                vterm-mode-hook
                term-mode-hook
                shell-mode-hook
                olivetti-mode-hook
                eww-mode-hook
                treemacs-mode-hook
                doc-view-mode-hook
                archive-mode-hook
                image-mode-hook
                elfeed-show-mode-hook
                elfeed-search-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; A simple frame title
(setq frame-title-format '("%b")
      icon-title-format frame-title-format)

;; Don't resize the frames in steps; it looks weird, especially in tiling window
;; managers, where it can leave unseemly gaps.
(setq frame-resize-pixelwise t)
(setq pixel-dead-time 1)
(setq confirm-kill-emacs #'yes-or-no-p)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

(setq fill-column 80)

(when d/on-droid
  ;; access phone storage as default
  (setq default-directory "/storage/emulated/0/")
  (setq touch-screen-precision-scroll nil)
  (setq touch-screen-display-keyboard t)
  ;; (setq use-dialog-box nil)
  )

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (setq doom-modeline-icon t)
                (with-selected-frame frame
                  (d/set-font-faces))))
  (d/set-font-faces))
(setq doom-modeline-icon t)
(put 'narrow-to-region 'disabled nil)
