;;; Personal configuration -*- lexical-binding: t -*-

;;; Commentary:

;; No need write this file, do it in literate org file!

;;; Code:

;; Lets garbage collect to make emacs quicker! 

(defun my-cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold  67108864) ; 64M
  (setq gc-cons-percentage 0.1) ; original value
  (garbage-collect))

(run-with-idle-timer 4 nil #'my-cleanup-gc)

(message "  Emacs loaded in %s with %d garbage collections."
         (format "%.2f seconds"
                 (float-time (time-subtract after-init-time before-init-time)))
         gcs-done)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; You will most likely need to adjust this font size for your system!
(defvar default-font-size 170)
(defvar default-variable-font-size 170)

;; Set reusable font name variables
(defvar d/fixed-width-font "ComicCodeLigatures Nerd Font"
  "The font to use for monospaced (fixed width) text.")

(defvar d/variable-width-font "ComicCodeLigatures Nerd Font"
  "The font to use for variable-pitch (document) text.")

(defvar d/header-font "Comic Mono"
  "Font for header level in org-mode." )

(defvar d/sans-font "SF Pro Rounded"
  "Sans font for reading docs or presentation")
(defvar d/jetb-font "JetBrainsMono Nerd Font"
  "Jetbrains font for code/verbatim" )
(defvar d/title-face "Impress BT"
  "Font for title")
(defvar d/link-font "VictorMono Nerd Font"
  "Font for links")
(defvar d/code-font "VictorMono Nerd Font"
  "Font for inline code")


(setf use-default-font-for-symbols nil)
(set-fontset-font t 'unicode "Noto Emoji" nil 'append)

(defun d/set-font-faces ()
  (message "Setting faces!")
  (set-face-attribute 'default nil :font d/variable-width-font :weight 'medium :height default-font-size)

  ;; Set the fixed pitch face (monospace)
  (set-face-attribute 'fixed-pitch nil :font d/fixed-width-font :height default-font-size)

  ;; Set the variable pitch face (document text)
  (set-face-attribute 'variable-pitch nil :font d/variable-width-font :height default-variable-font-size :weight 'medium))

(use-package no-littering
  :config
  ;; no-littering doesn't set this by default so we must place
  ;; auto save files in the same path as it uses for sessions
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  ;;  (setq backup-directory-alist '(("." . "~/.config/emacs/backups")))
  (setq delete-old-versions -1)
  (setq version-control t)
  (setq vc-make-backup-files t))

(setq make-backup-files t          ; backup of a file the first time it is saved.
      backup-by-copying t          ; don't clobber symlinks
      version-control t            ; version numbers for backup files
      vc-make-backup-files t       ; version control for git/vcs dirs
      delete-old-versions t        ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 2          ; oldest versions to keep when a new numbered backup is made 
      kept-new-versions 2          ; newest versions to keep when a new numbered backup is made 
      auto-save-default t          ; auto-save every buffer that visits a file
      auto-save-timeout 20         ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200       ; number of keystrokes between auto-saves (default: 300)
      create-lockfiles nil         ; don't use lockfiles (default: t)
      )

(use-package savehist
  :init
  (savehist-mode)
  :custom
  (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring)))

(defun split-and-follow-horizontally ()
  "Basically to balance and change cursor to split window"
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-and-follow-vertically ()
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
(setq add-unicodes '("~/d-git/d-bin/treasure/unicodes/emoji" "~/d-git/d-bin/treasure/unicodes/icons"))

(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'tree))

(global-set-key (kbd "M-v") #'d/scroll-up)
(global-set-key (kbd "C-v") #'d/scroll-down)
(global-set-key (kbd "<f5>") #'d/refresh-buffer)
(global-set-key (kbd "C-c d i") #'d/insert-unicodes)
(global-set-key (kbd "C-c d c") #'d/insert-colors)

;; Get rid of annoyance
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-z"))

;;(define-key org-mode-map (kbd "C-c C-x C-s") #'org-archive-done-tasks)
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)
(global-set-key [C-tab] 'other-window)

(global-set-key (kbd "C-x C-k") 'd/kill-buffer) ;; My func to clear cache along killing buffer
(global-set-key (kbd "C-x k") 'kill-buffer)
(global-set-key (kbd "M-%") 'query-replace-regexp) ;; Hail regexp searching!

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

(defalias 'yes-or-no-p 'y-or-n-p) ;; Make confirmation messages easy and not a pain.

(use-package which-key
  :defer 0
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
  :defer t
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h x" . helpful-command)
  ("C-c C-d" . helpful-at-point)
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

(setq scroll-conservatively 101) ;; value greater than 100 gets rid of half page jumping
(setq mouse-wheel-scroll-amount nil)
(setq mouse-wheel-progressive-speed t) ;; accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(use-package vertico
  :bind (:map vertico-map
              ("?" . minibuffer-completion-help)
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-d" . vertico-directory-delete-char)
              ("M-RET" . minibuffer-force-complete-and-exit)
              ("M-TAB" . minibuffer-complete))

  :init
  (vertico-mode)
  ;; (vertico-flat-mode 1)
  ;; Different scroll margin
  (setq vertico-scroll-margin 1)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
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

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
(setq completion-styles '(orderless))
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)
;; Use `consult-completion-in-region' if Vertico is enabled.
;; Otherwise use the default `completion--in-region' function.
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

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
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-ripgrep)
         ("M-s m" . consult-man)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
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

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
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

(defun d/consult-first-param-is-initial-text (consult-fn &rest rest)
  "Advising function around CONSULT-FN.

  The CONSULT-FN's first parameter should be the initial text.

  When there's an active region, use that as the first parameter
  for CONSULT-FN.  Otherwise, use an empty string the first
  parameter.  This function handles the REST of the parameters."
  (interactive)
  (apply consult-fn
         (when (use-region-p)
           (buffer-substring
            (region-beginning) (region-end)))
         rest))

(defun d/consult-ripgrep-wrapper (consult-fn &optional dir given-initial)
  "Advising function around CONSULT-FN.

  DIR and GIVEN-INITIAL match the method signature of `consult-wrapper'."
  (interactive "P")
  (let ((initial (list (or given-initial
                           (when (use-region-p)
                             (buffer-substring (region-beginning)
                                               (region-end)))))))
    (apply consult-fn dir initial)))
(advice-add #'consult-line
            :around #'d/consult-first-param-is-initial-text
            '((name . "wrapper")))
(advice-add #'consult-ripgrep
            :around #'d/consult-ripgrep-wrapper
            '((name . "wrapper")))

(defun counsel-colors--web-list nil
  "Return list of CSS colors for `counsult-colors-web'."
  (require 'shr-color)
  (sort (mapcar #'downcase (mapcar #'car shr-color-html-colors-alist)) #'string-lessp))

(defun d/colors-web (color)
  "Show a list of all CSS colors.\

  You can insert the name (default), or insert or kill the hexadecimal or RGB value of the
  selected color."
  (interactive
   (list (consult--read (counsel-colors--web-list)
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

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

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
  (corfu-echo-documentation 0.25)
  (corfu-preview-current 'insert)
  (corfu-preselect-first t)
  (corfu-popupinfo-delay 1.5)
  (corfu-history 1)
  (corfu-scroll-margin 0)
  :bind (:map corfu-map
              ("M-SPC" . corfu-insert-separator)
              ("TAB" . corfu-insert)
              ("<escape>" . corfu-quit)
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous)                
              ("RET" . corfu-insert))
  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  :init
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  (global-corfu-mode))

(setq completion-category-overrides '((eglot (styles orderless))))


(unless (display-graphic-p)
  (corfu-terminal-mode +1))

;; Add extensions
(use-package cape
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

;; Add your own file with all words
(defcustom cape-dict-file "~/.local/share/dict/vocab"
  "Dictionary word list file."
  :type 'string)

(setq-local corfu-auto t
            corfu-auto-delay 1
            corfu-auto-prefix 0
            completion-category-defaults nil
            completion-category-overrides '((file (styles partial-completion)))
            completion-styles '(orderless))

(defun corfu-enable-always-in-minibuffer ()
  "Enable corfi in minibuffer, if vertico is not active"
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (eq (current-local-map) read-passwd-map))
    (setq-local corfu-auto t
                corfu-popupinfo-delay nil
                corfu-auto-delay 0
                corfu-auto-prefix 0
                completion-styles '(basic))
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

;; Configure Tempel
(use-package tempel
  :after corfu
  :hook
  (prog-mode . tempel-setup-capf)
  (text-mode . tempel-setup-capf)
  (prog-mode . tempel-abbrev-mode)

  ;; Require trigger prefix before template name when completing.
  :custom
  (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  )

(use-package tempel-collection
  :ensure t
  :after tempel
  )

(use-package org-modern
  :defer t)
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

 org-modern-star '("◉" "✤" "◈" "✿" "✤")
 org-modern-hide-stars nil
 org-modern-table t
 org-modern-list 
 '((?* . "❉")
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

(global-org-modern-mode)

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind ("M-$". jinx-correct)
  :config
  (set-face-attribute 'jinx-misspelled nil :inherit nil :underline '(:color "gold" :style line :position t)))

(defun org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-block-begin-line . 0.9)                    
                  (org-level-8 . 1.1)))
    (set-face-attribute 'org-document-title nil :font d/title-face :weight 'bold :height 2.5 :width 'extra-expanded)
    (set-face-attribute 'org-level-1 nil :font d/header-font :weight 'medium :height 1.3 :foreground "#b6a0ff")
    (set-face-attribute 'org-level-2 nil :font d/header-font :weight 'medium :height 1.2)
    (set-face-attribute 'org-level-3 nil :font d/header-font :weight 'medium :height 1.1)
    (set-face-attribute 'org-level-4 nil :font d/header-font :weight 'medium :height 1.1)
    (set-face-attribute 'org-level-5 nil :font d/header-font :weight 'medium :height 1.15)

    (set-face-attribute 'variable-pitch nil :height default-variable-font-size :weight 'medium)
    (set-face-attribute 'org-verbatim nil :height '1.15 :font d/jetb-font :weight 'medium)
    (set-face-attribute 'org-code nil :height '1.15 :font d/jetb-font :weight 'medium)
    (set-face-attribute (car face) nil :font d/header-font :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'line-number nil :slant 'normal :weight 'semibold :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :weight 'ultrabold :slant 'normal :inherit 'fixed-pitch ))

(defun org-mode-setup ()
  (org-indent-mode 1)
  (org-display-inline-images 1)
  (variable-pitch-mode 1)
  (org-font-setup)
  (setq
   org-startup-indented nil
   org-image-actual-width 300
   org-startup-folded t)
  )

(use-package org
  :pin org
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
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "d-stuff")
  (setq browse-url-secondary-browser-function 'browse-url-generic
        browse-url-generic-program "d-stuff")

  (setq org-agenda-files
        '("~/sync/org/tasks.org"
          "~/d-git/d-site/README.org"))

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
         (org-present-after-navigate-functions . d/org-present-prepare-slide)))


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
  :lighter "d/org-present-mode"
  (if d/org-present-mode
      (org-present)
    (org-present-quit)))

(defun d/org-present-enable-hook ()
  (setq d/org-present--inhibit-message inhibit-message
        d/org-present--echo-keystrokes echo-keystrokes
        d/org-present--visual-line-mode visual-line-mode
        d/org-present--org-ellipsis org-ellipsis
        d/org-present--org-indent-mode org-indent-mode)
  (org-indent-mode 1)

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
              org-ellipsis "⤵")

  (dolist (face '((org-block . 1.0)
                  (org-block-begin-line . 0.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute 'org-document-title nil :font d/title-face :weight 'bold :height 2.5 :width 'extra-expanded)
    (set-face-attribute 'org-document-info nil :font d/link-font :slant 'italic :weight 'bold :height 2.5 :width 'extra-expanded)
    (set-face-attribute 'org-level-1 nil :font d/header-font :weight 'medium :height 1.6 :foreground "#b6a0ff")
    (set-face-attribute 'org-level-2 nil :font d/header-font :weight 'medium :height 1.5)
    (set-face-attribute 'org-level-3 nil :font d/header-font :weight 'medium :height 1.4)
    (set-face-attribute 'org-level-4 nil :font d/header-font :weight 'medium :height 1.3)
    (set-face-attribute 'org-level-5 nil :font d/header-font :weight 'medium :height 1.25)

    (set-face-attribute 'org-verbatim nil :font d/jetb-font :weight 'medium :height 1.3)
    (set-face-attribute 'org-code nil :font d/code-font :weight 'medium :height 1.4)


    (set-face-attribute 'header-line nil :background nil :height 2.5)
    (set-face-attribute 'variable-pitch nil :font d/variable-width-font :height 1.2 :weight 'medium)
    (set-face-attribute (car face) nil :font d/fixed-width-font :weight 'medium :height (cdr face)))


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

  (set-face-attribute 'header-line nil :height '1.0 :background)

  (org-indent-mode d/org-present--org-indent-mode)

  (if (package-installed-p 'hide-mode-line)
      (hide-mode-line-mode 0))

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
  "Go to next sibling."
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

(use-package denote
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
        (expand-file-name "~/sync/org/books/")))

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

(use-package olivetti
  :defer t
  :hook ((text-mode         . olivetti-mode)
         ;; (prog-mode         . olivetti-mode)
         (Info-mode         . olivetti-mode)
         (eshell-mode         . olivetti-mode)
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

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  (setq doom-modeline-time-icon nil)
  (setq doom-modeline-bar-width 2)
  (setq doom-modeline-major-mode-icon t)
  :custom ((doom-modeline-height 8)
           (doom-modeline-buffer-encoding nil)))

;; to hide during presentation and writing
(use-package hide-mode-line
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
(load-theme 'modus-vivendi t)

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

(use-package nix-mode
  :mode "\\.nix\\'"
  :defer t)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;;(add-hook 'prog-mode-hook #'eglot-ensure)
(add-hook 'prog-mode-hook #'flycheck-mode)

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
      (set-face-attribute (car face) nil :weight 'normal :font d/header-font :height (cdr face))))

  (defun d/markdown-mode-hook ()
    (d/set-markdown-header-font-sizes))

  (add-hook 'markdown-mode-hook 'd/markdown-mode-hook))

(use-package eglot
  :init
  (setq eglot-sync-connect 1
        eglot-connect-timeout 10
        eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        ;; NOTE We disable eglot-auto-display-help-buffer because :select t in
        ;;      its popup rule causes eglot to steal focus too often.
        eglot-auto-display-help-buffer nil)
  :config
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
  (add-to-list 'eglot-server-programs '(bash-ts-mode . ("bash-language-server")))
  (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman")))

  :hook
  (nix-mode . eglot-ensure)
  (bash-ts-mode . eglot-ensure)
  (markdown-mode-hook . eglot-ensure))

(defun my/eglot-capf ()
  (setq-local completion-at-point-functions
              (list (cape-super-capf
                     #'eglot-completion-at-point
                     #'tempel-expand
                     #'cape-file))))

(add-hook 'eglot-managed-mode-hook #'my/eglot-capf)

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (setq kind-icon-default-style '(:padding -0.5 :stroke 0 :margin 0 :radius 0 :height 0.6 :scale 1.0)))

(use-package magit
  :defer t
  :config
  ;; Show word-granularity differences within diff hunks
  (setq magit-diff-refine-hunk t)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; We need to define setup for keyboard layout

(require 'meow)

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
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(setq meow-replace-state-name-list
      '((normal . "")
        (motion . "󱖲")
        (keypad . "󰥻")
        (insert . "")
        (beacon . "")))

;meow-thing-register THING INNER BOUNDS
;(meow-thing-register 'arrow '(pair ("<") (">")) '(pair ("<") (">")))
;(add-to-list 'meow-char-thing-table '(?a . arrow))
(set-face-attribute 'meow-normal-indicator nil :foreground "#87ceeb" :height 1.0 :weight 'ultra-heavy)
(set-face-attribute 'meow-motion-indicator nil :foreground "#ffd700" :height 1.0 :weight 'ultra-heavy)
(set-face-attribute 'meow-keypad-indicator nil :foreground "#00ffff" :height 1.0 :weight 'ultra-heavy)
(set-face-attribute 'meow-insert-indicator nil :foreground "#00ff00" :height 1.0 :weight 'ultra-heavy)
(set-face-attribute 'meow-beacon-indicator nil :foreground "#ff00ff" :height 1.0 :weight 'ultra-heavy)

(meow-setup)
(meow-global-mode 1)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)
         ("C-c f f" . window-focus-mode)
         ("C-c f e" . (lambda () (interactive) (find-file (expand-file-name "~/d-git/d-nix/d-emacs.org"))))
         ("C-c f s" . (lambda () (interactive) (find-file (expand-file-name "~/d-git/d-nix/d-setup.org"))))
         ("C-c f m" . (lambda () (interactive) (find-file (expand-file-name "~/d-git/d-nix/README.org"))))
         ("C-x C-d" . dired))
  (:map dired-mode-map
        ("q" . kill-buffer-and-window)
        ("l" . dired-single-buffer)
        ("n" . dired-single-buffer)
        ("p" . dired-single-up-directory)
        ("h" . dired-single-up-directory)
        ("j" . dired-next-line)
        ("k" . dired-previous-line)
        ("b" . d/external-browser))

  :custom ((dired-listing-switches "-agho --group-directories-first")))
(setq dired-listing-switches "-alt --dired --group-directories-first -h -G")
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))

(use-package all-the-icons
  :bind ("C-x 8 i" . all-the-icons-insert))

(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package vterm
  :defer t
  :bind ("C-c d t" . vterm)
  :config
  (setq vterm-shell "/etc/profiles/per-user/i/bin/zsh"))

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

(use-package undo-fu-session
  :init (undo-fu-session-global-mode)
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package vundo
  :bind (("C-x u" . vundo)
         ("C-z" . undo-only)
         ("C-S-z" . undo-redo)
         ("C-M-r" . undo-redo)))

(use-package flycheck
  :defer t)
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
  :ensure t
  :bind (("C-c C-p C-b" . webpaste-paste-buffer)
         ("C-c C-p C-r" . webpaste-paste-region)
         ("C-c C-p C-p" . webpaste-paste-buffer-or-region))
  :config
  (setq webpaste-provider-priority '("dpaste.org" "dpaste.com" "paste.mozilla.org"))
  ;; Require confirmation before doing paste
  (setq webpaste-paste-confirmation t))

(use-package sdcv
  :defer t
  :config
  (setq sdcv-say-word-p t)
  (setq sdcv-dictionary-data-dir "~/d-git/d-bin/treasure/dict/") 
  (setq sdcv-dictionary-simple-list   
        '("wn" "enjp" "thesaurus"))
  :bind ("C-c d d" . sdcv-search-input)
  (:map sdcv-mode-map
        ("q" . kill-buffer-and-window)
        ("n" . sdcv-next-dictionary)
        ("TAB" . hide-entry)
        ("<backtab>" . show-entry)
        ("p" . sdcv-previous-dictionary)))

(use-package pdf-tools
  :defer t
  :init
  (pdf-tools-install)
  :bind (:map pdf-view-mode-map
              ("h" . pdf-annot-add-highlight-markup-annotation)
              ("t" . pdf-annot-add-text-annotation)
              ("D" . pdf-annot-delete)
              ("i" . pdf-view-midnight-minor-mode)
              ("Q" . d/kill-buffer))

  :config
  (setq pdf-tools-enabled-modes         ; simplified from the defaults
        '(pdf-history-minor-mode
          pdf-isearch-minor-mode
          pdf-links-minor-mode
          pdf-outline-minor-mode
          pdf-misc-size-indication-minor-mode
          pdf-occur-global-minor-mode))
  (setq pdf-view-display-size 'fit-page) ;;fit-height
  (setq pdf-view-continuous t)
  (setq pdf-cache-image-limit 3)
  (setq large-file-warning-threshold 700000000)
  (setq pdf-cache-prefetch-delay 0.5)
  (setq image-cache-eviction-delay 3)
  (setq pdf-annot-activate-created-annotations t)
  (setq pdf-view-use-dedicated-register nil)
  (setq pdf-view-max-image-width 2000)
  (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "M-g g") 'pdf-view-goto-page)
  (setq pdf-outline-imenu-use-flat-menus t)
  (setq pdf-view-resize-factor 1.1))


(defun d/kill-buffer ()
  "Clear the image cache (to release memory) after killing a pdf buffer."
  (interactive)
  (if (one-window-p) (kill-this-buffer)
    (kill-buffer-and-window))
  (clear-image-cache t)
  (pdf-cache-clear-data))

(define-key image-mode-map (kbd "q") 'd/kill-buffer)

;; For Comic Manga
(add-hook 'image-mode-hook (lambda ()
                             (olivetti-mode)
                             (setq olivetti-body-width 0.45)))

(use-package man
  :bind (("C-c m" . consult-man)
         :map Man-mode-map
         ("q" . kill-buffer-and-window)))

(defun config-reload ()
  "Uncle dev created a function to reload Emacs config."
  (interactive)
  (load-file (expand-file-name "~/.config/emacs/init.el")))

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
    :bind ("C-c d e" . elfeed)
    ("C-c d b" . d/external-browser)
    (:map elfeed-show-mode-map
          ("e" . elfeed-open-in-eww)
          ("i" . d/bionic-read)
          ("r" . elfeed-open-in-reddit)
          ("m" . elfeed-toggle-show-star)
          ("b" . d/external-browser))
    (:map elfeed-search-mode-map
          ("m" . elfeed-toggle-star)
          ("U" . elfeed-update)
          ("u" . elfeed-update-feed))
    :config
    ;; (setq-default elfeed-search-filter "@1-week-ago--1-day-ago +unread -news +")
    (setq-default elfeed-search-filter "+unread +")
    (defalias 'elfeed-toggle-show-star
      (elfeed-expose #'elfeed-show-tag 'star))    
    (defalias 'elfeed-toggle-star
      (elfeed-expose #'elfeed-search-toggle-all 'star))

    (defun d/elfeed-ui ()
      (interactive)
      (setq-local header-line-format " ")

      (set-face-attribute 'header-line nil :background nil :height 0.9)

      ;; For sides
      (set-face-attribute 'message-header-name nil :font d/header-font :height '0.8 :background)
      ;; For Title
      (set-face-attribute 'message-header-subject nil :font d/title-face :height '1.80 :background)
      ;; For tags..
      (set-face-attribute 'message-header-other nil :font d/jetb-font :height '1.0 :background)
      ;; For Author
      (set-face-attribute 'message-header-to nil :font d/sans-font :slant 'italic :height '1.50 :background)
      (set-face-attribute 'shr-link nil :font d/link-font :slant 'italic :weight 'semibold :width 'medium :height '1.0 :background))

    ;; face for starred articles
    (defface elfeed-search-star-title-face
      '((t :foreground "#f77"))
      "Marks a starred Elfeed entry.")

    (push '(star elfeed-search-star-title-face) elfeed-search-face-alist))

  (use-package link-hint
    :defer t
    :ensure t
    :bind
    ("C-c l o" . link-hint-open-link)
    ("C-c l c" . link-hint-copy-link))

  (use-package avy
    :defer t
    :bind
    ("M-j" . avy-goto-char-timer)
    ("M-K" . avy-kill-region)
    ("C-S-k" . avy-kill-whole-line))

  (use-package elfeed-org
    :after elfeed
    :config
    (elfeed-org)
    (setq rmh-elfeed-org-files (list "~/.config/emacs/elfeed.org")))

  (defun readable-article ()
    (interactive)
    (eww-readable)
    ;; (d/bionic-read)
    (beginning-of-buffer)
    (d/eww-rename-buffer))

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
                ("C-f" . shr-next-link)
                ("C-b" . shr-previous-link)
                ("F" . d/visit-urls)
                ("U" . elfeed-update)
                ("b" . d/external-browser)))
(with-eval-after-load "shr"
    (defun shr-put-image (spec alt &optional flags)
      "Insert image SPEC with a string ALT.  Return image.
SPEC is either an image data blob, or a list where the first
element is the data blob and the second element is the content-type.
Hack to use `insert-sliced-image' to avoid jerky image scrolling."
      (if (display-graphic-p)
          (let* ((size (cdr (assq 'size flags)))
                 (data (if (consp spec)
                           (car spec)
                         spec))
                 (content-type (and (consp spec)
                                    (cadr spec)))
                 (start (point))
                 (image (cond
                         ((eq size 'original)
                          (create-image data nil t :ascent 100
                                        :format content-type))
                         ((eq content-type 'image/svg+xml)
                          (create-image data 'svg t :ascent 100))
                         ((eq size 'full)
                          (ignore-errors
                            (shr-rescale-image data content-type
                                               (plist-get flags :width)
                                               (plist-get flags :height))))
                         (t
                          (ignore-errors
                            (shr-rescale-image data content-type
                                               (plist-get flags :width)
                                               (plist-get flags :height)))))))
            (when image
              (let* ((image-pixel-cons (image-size image t))
                     (image-pixel-width (car image-pixel-cons))
                     (image-pixel-height (cdr image-pixel-cons))
                     (image-scroll-rows (round (/ image-pixel-height (default-font-height)))))
                ;; When inserting big-ish pictures, put them at the
                ;; beginning of the line.
                (when (and (> (current-column) 0)
                           (> (car (image-size image t)) 400))
                  (insert "\n"))

                (insert-sliced-image image (or alt "*") nil image-scroll-rows 1)
                ;; (if (eq size 'original)
                ;;     (insert-sliced-image image (or alt "*") nil image-scroll-rows 1)
                ;;   (insert-image image (or alt "*")))

                (put-text-property start (point) 'image-size size)
                (when (and shr-image-animate
                           (cond ((fboundp 'image-multi-frame-p)
                                  ;; Only animate multi-frame things that specify a
                                  ;; delay; eg animated gifs as opposed to
                                  ;; multi-page tiffs.  FIXME?
                                  (cdr (image-multi-frame-p image)))
                                 ((fboundp 'image-animated-p)
                                  (image-animated-p image))))
                  (image-animate image nil 60))))
            image)
        (insert (or alt "")))))

(defun d/external-browser ()
  (interactive)
  (cond ((or (thing-at-point-url-at-point) (shr-url-at-point nil) (dired-file-name-at-point) (link-hint--org-link-at-point-p))
   (link-hint-copy-link-at-point))
  (t (link-hint-copy-link)))
(let ((url (current-kill 0)))
  (browse-url-generic url)))

(defun d/eww-rename-buffer ()
  "Rename EWW buffer using page title or URL.
    To be used by `eww-after-render-hook'."
  (let ((name (if (eq "" (plist-get eww-data :title))
                  (plist-get eww-data :url)
                (plist-get eww-data :title))))
    (rename-buffer (substring (format "*%s # eww*" name)0 32) t)))

(add-hook 'eww-after-render-hook #'d/eww-rename-buffer)
(advice-add 'eww-back-url :after #'d/eww-rename-buffer)
(advice-add 'eww-forward-url :after #'d/eww-rename-buffer)
;; (advice-add 'eww-readable :after #'d/bionic-read)

(use-package ement
  :hook (ement-room-compose . ement-room-compose-org)
  :bind (:map ement-room-minibuffer-map
              ("<f6>" . ement-room-compose-from-minibuffer))
  (:map ement-room-mode-map
        ("M-<" . ement-room-scroll-down-command))
  :config
  (setq ement-room-send-message-filter 'ement-room-send-org-filter)
  (setq ement-room-message-format-spec "%S> %L%B%r%R%t")
  (setq ement-room-list-avatars nil))

(use-package ox-hugo
  :after ox)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(setq set-fringe-style "default")        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; (setq-default mode-line-format nil)

;; (server-start)

;; Display messages when idle, without prompting
(setq help-at-pt-display-when-idle t)

(setq use-dialog-box nil)
(setq sentence-end-double-space nil)
(setq sentence-end "[.?!] ")  

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
 shr-max-image-proportion 0.2
 shr-width 100                                ; Fold text to 70 columns
 eww-search-prefix "https://lite.duckduckgo.com/lite/?q=")

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha-background 82)
(add-to-list 'default-frame-alist `(alpha-background . 82))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                vterm-mode-hook
                term-mode-hook
                shell-mode-hook
                olivetti-mode-hook
                treemacs-mode-hook
                pdf-view-mode-hook
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
(setq pixel-dead-time 10000)
(setq confirm-kill-emacs #'yes-or-no-p)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

(setq fill-column 100)

(add-to-list 'custom-theme-load-path "~/d-git/d-theme/")

(set-face-attribute 'corfu-border nil  :background "#bcd2ee")
(setq doom-modeline-icon t)
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (setq doom-modeline-icon t)
                (with-selected-frame frame
                  (d/set-font-faces))))
    (d/set-font-faces))
 (put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
   '(((:application eshell)
      eshell-connection-default-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((eshell-connection-default-profile
      (eshell-path-env-list))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(custom-safe-themes
   '("1021e43a7d494af9927db1531e10c2b168eedcaff0b7f510995db7d305288519" "6454421996f0508c38215a633256e36c19a28591542fb0946cfc40f1dceb89cf" default))
 '(package-selected-packages
   '(nano-modeline ef-themes gruvbox-theme kaolin-themes doom-themes autothemer vterm vundo undo-fu-session flycheck helpful ox-pandoc no-littering rainbow-delimiters rainbow-mode vertico orderless consult marginalia embark olivetti org-modern cape markdown-mode nix-mode rust-mode lua-mode all-the-icons-dired async dired-hide-dotfiles dired-single reddigg hnreader mingus pdf-tools which-key magit org-present org-mime corfu-terminal beframe denote tempel-collection sdcv elfeed-org link-hint powerthesaurus jinx doom-modeline hide-mode-line el-fetch ox-hugo htmlize ement kind-icon speed-type aria2 meow webpaste hydra evil-collection treesit-auto modalka mini-frame nerd-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
