(use-package time
  :ensure nil
  :hook
  (after-init . display-time)
  :custom
  (display-time-default-load-average nil)
  (display-time-24hr-format t))

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
  (tramp-use-ssh-controlmaster-options nil)

(unless d/on-droid
  (defcustom tramp-nspawn-machinectl-program "machinectl" ;; like nixos-container
    "Name of the machinectl program."
    :type 'string)

  (defconst tramp-nspawn-method "nspawn"
    "Tramp method name to use to connect to systemd-nspawn containers.")

  (defun tramp-nspawn--completion-function (&rest _args)
    "List systemd-nspawn containers available for connection.

This function is used by ‘tramp-set-completion-function’, please
see its function help for a description of the format."
    (let* ((raw-list (shell-command-to-string
                      (concat tramp-nspawn-machinectl-program
                              " list -q")))
           (lines (cdr (split-string raw-list "\n")))
           (first-words (mapcar (lambda (line) (car (split-string line)))
                                lines))
           (machines (seq-take-while (lambda (name) name) first-words)))
      (mapcar (lambda (m) (list nil m)) machines)))


  ;; todo: check tramp-async-args and tramp-direct-async
  (defun tramp-nspawn--add-method ()
    "Add Tramp method handler for nspawn containers."
    (push `(,tramp-nspawn-method
            (tramp-login-program ,tramp-nspawn-machinectl-program)
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
    (tramp-set-completion-function tramp-nspawn-method
                                   '((tramp-nspawn--completion-function ""))))

  (add-hook 'after-init-hook 'tramp-nspawn-setup)

  ))

(use-package battery
  :ensure nil
  :hook
  (after-init . display-battery-mode)
  :custom
  ;; better to keep charge between 40-80
  (battery-load-low '40)
  (battery-load-critical '29))

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

;; Initialize package sources
(require 'package)

(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/"))

(unless (bound-and-true-p package--initialized)
  (customize-set-variable 'package-enable-at-startup nil)
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

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
  :bind
  ("C-z" . nil) ;; avoid suspend-emacs
  ("C-x C-z" . nil)
  ;; panes
  ("M-o" . other-window)
  ("C-x C-k" . d/kill-buffer)
  ("C-x n n" . d/narrow-or-widen-dwim)

  (:map mode-specific-map
        ("C-g" . minibuffer-keyboard-quit))

  :custom
  (inhibit-startup-screen t "Don't show splash screen")

  (initial-major-mode 'org-mode)
  (initial-scratch-message
   "#+title: Scratch Buffer\n\nFor random thoughts.\n\n")

  (use-short-answers t)
  (enable-recursive-minibuffers t "Allow minibuffer commands in the minibuffer")
  (indent-tabs-mode nil "Spaces!")
  (tab-always-indent 'complete)
  (tab-width 2)
  (reb-re-syntax 'string)

  (history-delete-duplicates t)
  ;; window/pane
  (recenter-positions '(top middle bottom))
  ;; pane
  (frame-resize-pixelwise t)
  (frame-inhibit-implied-resize t)

  (sentence-end-double-space nil)
  (sentence-end "[.?!,;-]")

  ;; select
  (selection-coding-system 'utf-8)

  :config
  (delete-selection-mode)

  (with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))
  (prefer-coding-system 'utf-8)
  ;; Uppercase is same as lowercase
  (define-coding-system-alias 'UTF-8 'utf-8)
  (modify-all-frames-parameters
   '((alpha-background . 100)
     (right-divider-width . 0)
     (internal-border-width . 0)))

  ;; balance windows when split (https://zck.org/balance-emacs-windows)
  (seq-doseq (fn (list #'split-window #'delete-window))
    (advice-add fn :after #'(lambda (&rest args) (balance-windows))))
  )

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
  (when (derived-mode-p 'doc-view-mode) (progn (clear-image-cache) (doc-view-clear-cache)))
  (when (derived-mode-p 'pdf-view-mode) (progn (clear-image-cache) (pdf-cache-clear-data)))
  (if (one-window-p) (kill-this-buffer)
    (kill-buffer-and-window)))

(use-package saveplace
  :ensure nil
  :hook
  (after-init . save-place-mode))

(use-package simple
  :ensure nil
  :bind
  ("<f7>" . scratch-buffer)
  ("<escape>" . keyboard-quit)
  ("M-^" . d/join-lines)
  ("M-z" . zap-up-to-char)

  ("M-%" . query-replace-regexp)

  ("M-c" . d/flex)
  ("M-l" . downcase-dwim)

  ("M-@" . d/mark-word)
  ("M-h" . d/mark-paragraph)

  :custom
  (kill-ring-max 30000)
  (column-number-mode 1)
  (kill-do-not-save-duplicates t)

  :config
  ;; (global-hl-line-mode 1)
  (global-visual-line-mode 1)
  (auto-fill-mode 1))

(defun d/join-lines (specify-separator)
  "Join lines in the active region by a separator, by default a comma.
Specify the separator by typing C-u before executing this command."
  (interactive "P")
  (require 's)
  (unless (region-active-p)
    (delete-indentation)
    (message "select a region of lines first."))
  (let*
      ((separator (if (not specify-separator) ","
                    (read-string "Separator: ")))
       (text (buffer-substring-no-properties
              (region-beginning)
              (region-end)))
       (lines (split-string text "\n"))
       (result (s-join separator lines)))
    (delete-region (region-beginning) (region-end))
    (insert result)))

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

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode)
  :custom
  (display-line-numbers-type 'relative))

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
  ("C-M-r" . undo-redo)

  :custom
  (vundo-compact-display t)
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-window-max-height 5))

(use-package vc-backup
  ;; to have auto VC track of files without in git
  ;; C-x v =
  :demand t
  :custom
  (vc-make-backup-files t)
  (vc-follow-symlinks t))

(use-package autorevert
  :init
  (global-auto-revert-mode))

(use-package savehist
  :ensure nil
  :defer 2
  :init
  (savehist-mode)
  :custom
  (history-length t)
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring)))

(use-package recentf
  :ensure nil
  :demand t
  :custom
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 100)
  :config
  (recentf-mode))

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
         ("C-c f e" . (lambda () (interactive) (find-file "~/.config/emacs/lisp/")))
         ("C-c f s" . (lambda () (interactive) (find-file "~/d-git/d-nix/")))
         ("C-c f m" . (lambda () (interactive) (find-file "~/d-git/d-nix/README.org"))))
  (:map dired-mode-map
        ("q" . kill-buffer-and-window)
        ("j" . dired-next-line)
        ("k" . dired-previous-line)
        ("l" . dired-find-file)
        ("h" . dired-up-directory)
        ("b" . embark-act)
        ("e" . dired-do-eww))

  :custom
  (dired-listing-switches "-agho --group-directories-first")
  (delete-by-moving-to-trash t)
  (dired-dwim-target t)
  ;; (dired-kill-when-opening-new-dired-buffer t) ;; in case sinlge buffer is preferred
  )

(use-package dired-x
  :ensure nil
  :custom
  ;; Make dired-omit-mode hide all "dotfiles"
  (dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'\\|^\\..*$"))

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

  :bind
  (:map vertico-map
	    ("<return>" . vertico-directory-enter)
	    ("DEL" . vertico-directory-delete-char)
	    ("M-DEL" . vertico-directory-delete-word)
	    ("M-j" . vertico-quick-exit)
	    ("C-v" . vertico-scroll-up)
	    ("M-v" . vertico-scroll-down)
	    ("M-q" . d/vertico-toggle)
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
     ;; (jinx-correct reverse)
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
     (consult-org-heading reverse)
     (consult-dff unobtrusive)
     (embark-find-definition reverse)
     (embark-act grid)
     (xref-find-definitions reverse)))

  (vertico-multiform-categories
   '((file grid reverse)
     (consult-grep buffer)
     (jinx grid)
     (embark-bindings grid reverse)
     (embark-keybinding grid)
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

  :bind
  (
   ("C-c d i" . d/insert-unicodes)
   ("C-c d c" . d/insert-colors)

   ;; C-c bindings (mode-specific-map)
   ("C-c h" . consult-history)
   ("C-c M-x" . consult-mode-command)
   ("C-c k" . consult-kmacro)
   ("C-c t t" . consult-theme)
   ;; C-x bindings (ctl-x-map)
   ("C-x M-:" . consult-complex-command)
   ("C-x M-x" . consult-mode-command)
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
   ("M-g f" . consult-flymake)
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
   ;; ("C-s" . consult-line)
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
   ("M-r" . consult-history)

   :map org-mode-map
   ("M-g o" . consult-org-heading)
   ("M-g a" . consult-org-agenda))

  :hook (completion-list-mode . consult-preview-at-point-mode)

  :custom
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key "<")
  (consult-ripgrep-args "rg --follow --null --line-buffered --no-ignore --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip")

  (consult-customize
   consult-theme :preview-key '(:debounce 2.5 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  :config
  (advice-add #'register-preview :override #'consult-register-window))

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

(defun color-name-to-hex (NAME)
  "Return hexadecimal value of color with NAME.
Return nil if NAME does not designate a valid color."
  (insert
   (when-let*
       ((rgb (color-name-to-rgb NAME))
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

(use-package consult
  :config
  (defvar consult--source-eww
    (list
     :name     "Eww Buffer"
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

(use-package consult
  :config
  (defvar  consult--source-org
    (list :name     "Organize"
          :category 'buffer
          :narrow   ?o
          :face     'org-list-dt
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :items   '(lambda () (consult--buffer-query :mode '(org-mode org-agenda-mode) :as #'buffer-name))))
  (add-to-list 'consult-buffer-sources 'consult--source-org 'append))

(use-package consult
  :config
  (defvar consult--source-gnus
    (list :name     "Gnus"
          :hidden   t
          :narrow   ?g
          :category 'buffer
          :state    #'consult--buffer-state
          :items    '(lambda () (consult--buffer-query :mode '(gnus-article-mode gnus-summary-mode gnus-group-mode) :as #'buffer-name))))
  (add-to-list 'consult-buffer-sources 'consult--source-gnus 'append))

(use-package consult
  :config
  (defvar consult--source-eshell
    (list :name     "Eshells"
          :category 'buffer
          :narrow   ?s
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :new
          (lambda (name)
            (with-current-buffer (get-buffer-create name)
              ;;(insert "#+title: " name "\n\n")
              (eshell-mode)
              (consult--buffer-action (current-buffer))))
          :items '(lambda () (consult--buffer-query :mode 'eshell-mode :as #'buffer-name))))
    (add-to-list 'consult-buffer-sources 'consult--source-eshell 'append))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles orderless basic partial-completion)))))

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
  (embark-keymap-prompter-key "`")
  (embark-indicators
   '(embark-minimal-indicator  ; default is embark-mixed-indicator
     embark-highlight-indicator
     embark-isearch-highlight-indicator))

  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  )

;; credits to karthinks
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

  :bind
  ("C-c p p" . completion-at-point)
  ("C-c p t" . complete-tag)
  ("C-c p d" . cape-dabbrev)
  ("C-c p h" . cape-history)
  ("C-c p f" . cape-file)
  ("C-c p k" . cape-keyword)
  ("C-c p s" . cape-elisp-symbol)
  ("C-c p e" . cape-elisp-block)
  ("C-c p a" . cape-abbrev)
  ("C-c p l" . cape-line)
  ("C-c p w" . cape-dict)
  ("C-c p \\" . cape-tex)
  ("C-c p _" . cape-tex)
  ("C-c p ^" . cape-tex)
  ("C-c p &" . cape-sgml)
  ("C-c p r" . cape-rfc1345)

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

(use-package eglot-tempel
  :after eglot)

(use-package tab-bar
  :unless d/on-droid
  :bind
  ("C-<tab>" . tab-next)
  ;; :hook
  ;; (server-after-make-frame . (lambda () (tab-new) (tab-new) (tab-new)))
  :init
  (tab-bar-mode))

(use-package mwheel
  :ensure nil
  :bind
  ([mouse-9] . [prior]) ;; binds mouse fwd button to page up
  ([mouse-8] . [next]) ;; mouse bwd button to page down
  :custom
  (mouse-autoselect-window t)
)

(use-package disable-mouse
  :unless d/on-droid
  :bind
  ([f10] . disable-mouse-mode))

(use-package xt-mouse
  :init
  (xterm-mouse-mode))

(use-package pixel-scroll
  :ensure nil
  :commands
  (pixel-scroll-precision-scroll-down pixel-scroll-precision-scroll-up)
  :bind
  ("C-v" . pixel-scroll-interpolate-down)
  ("M-v" . pixel-scroll-interpolate-up)
  :custom
  (pixel-scroll-precision-interpolate-page t)
  :init
  (pixel-scroll-precision-mode 1))

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

(defun d/pixel-recenter-top-bottom ()
  "Similar to `recenter-top-bottom' but with pixel scrolling."
  (interactive)
  (let* ((current-row (cdr (nth 6 (posn-at-point))))
         (target-row (save-window-excursion
                       (recenter-top-bottom)
                       (cdr (nth 6 (posn-at-point)))))
         (distance-in-pixels (* (- target-row current-row) (line-pixel-height))))
    (pixel-scroll-precision-interpolate distance-in-pixels)))

(use-package frame
  :custom
  (use-system-tooltips t))

(use-package repeat
  :custom
  (repeat-exit-timeout 5)
  :config
  (repeat-mode 1))

(use-package winner
  :ensure nil
  :hook after-init
  :bind-keymap
  ("C-c w" . windmove-rmaps)
  :bind
  (:repeat-map windmove-rmaps
               ("h" . windmove-left)
               ("j" . windmove-down)
               ("k" . windmove-up)
               ("l" . windmove-right)
               ("n" . winner-undo)
               ("p" . winner-redo)
               )
  :commands (winner-undo winnner-redo))

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

(defun d/afk-mode ()
  (interactive)
  (zone)
  (zone-when-idle 80))

(use-package activities
  :unless d/on-droid
  :init
  (activities-mode)
  (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)

  :bind
  (("C-x C-a C-n" . activities-new)
   ("C-x C-a C-d" . activities-define)
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a b" . activities-switch-buffer)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a l" . activities-list)))

(use-package man
  :ensure nil
  :demand t
  :custom
  (Man-notify-method 'pushy "show manpage HERE")
  :custom-face
  (Man-overstrike ((t (:inherit font-lock-type-face :bold t))))
  (Man-underline ((t (:inherit font-lock-keyword-face :underline t))))

  :bind
  (("C-c m" . consult-man)
   :map Man-mode-map
   ("q" . kill-buffer-and-window)))

(use-package woman
  :ensure nil
  :defer t
  :custom-face
  (woman-bold ((t (:inherit font-lock-type-face :bold t))))
  (woman-italic ((t (:inherit font-lock-keyword-face :underline t)))))

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

(use-package magit
  :defer t
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-refine-hunk t)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function #'magit-restore-window-configuration))

(use-package ediff
  :ensure nil
  :custom
  (ediff-keep-variants nil)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package envrc
  :defer 2
  :config
  (envrc-global-mode)
  (advice-add 'org-babel-eval :around #'envrc-propagate-environment))

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
  ("C-c d s" . eshell)
  (:map eshell-mode-map
        ("C-S-l" . d/clear-eshell))

  :custom
  (process-adaptive-read-buffering nil)
  (eshell-scroll-to-bottom-on-input t)
  (eshell-directory-name (expand-file-name "eshell" user-emacs-directory))
  (eshell-aliases-file (expand-file-name "alias" eshell-directory-name))
  (eshell-highlight-prompt t)
  (eshell-hist-ignoredups t)
  (eshell-cd-on-directory t)
  (eshell-visual-command nil)
  (eshell-buffer-name "eshell-terminal")
  (eshell-pushd-dunique t)
  (eshell-last-dir-unique t)
  (eshell-last-dir-ring-size 32)
  (eshell-list-files-after-cd nil)
  (eshell-cd-shows-directory t)
  (eshell-prefer-lisp-functions nil)

  (eshell-kill-processes-on-exit 'ask)

  (eshell-prompt-function
   (lambda nil
     (concat
      "\n"
      ;; (propertize "  " 'face '(:inherit region))
      "  "
      " "
      (propertize (replace-regexp-in-string "~" " " (eshell/pwd)) 'face '(:foreground "lightblue1"))
      (when (package-installed-p 'magit) (propertize (if (magit-get-current-branch) (concat "   " (magit-get-current-branch)) "") 'face '(:foreground "orangered1")))
      (when (package-installed-p 'envrc) (propertize (if (string= envrc--status 'none) "" "   ") 'face '(:foreground "mediumspringgreen")))
      ;; (propertize (concat "   " (format-time-string "%H:%M" (current-time))) 'face '(:foreground "lightcyan1"))
      (propertize "\n 󰘧 " 'face '(:foreground "palegreen"))
      )))
  (eshell-prompt-regexp "^ 󰘧 "))

(defun d/clear-eshell ()
  (interactive)
  (eshell-send-input (eshell/clear 1)))

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

(use-package comint
  :bind
  ("M-g r" . d/comint-page-output)
  :custom
  (comint-pager "cat")
  :config
  (setenv "MANPAGER" "cat"))

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
  (python-shell-dedicated nil)
  (python-shell-interpreter "python")
  (python-shell-interpreter-args "")
  ;; (python-forward-sexp-function nil)
  (python-indent-guess-indent-offset-verbose nil))

(use-package ess
  :defer t
  :unless d/on-droid
  :custom
  (ess-use-company nil)
  (ess-eval-visibly nil)
  (ess-ask-for-ess-directory t)
  (ess-style 'RStudio)

  (ess-use-eldoc t)
  (ess-eldoc-show-on-symbol t)

  (ess-R-font-lock-keywords
   '((ess-R-fl-keyword:keywords . t)
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

  (inferior-R-font-lock-keywords
   '((ess-S-fl-keyword:prompt . t)
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
  :unless d/on-droid
  :bind
  (:map ess-mode-map
        ("C-;" . ess-insert-assign))
  (:map inferior-ess-r-mode-map
        ("C-;" . ess-insert-assign))

  :custom
  (ess-indent-with-fancy-comments nil))

(use-package nix-mode
  :defines (nix-mode-map)
  :functions
  (comint-send-input)
  :bind (:map nix-mode-map
              ("C-c C-e" . nix-eval-line)))

(use-package nix-ts-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'"))

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

(use-package ess-julia
  :unless d/on-droid
  :hook (ess-julia-mode . (lambda () (setq-local devdocs-browser-active-docs '("Julia"))))
  :bind
  (:map ess-julia-mode-map
        ("C-c C-d" . devdocs-browser-open))
  :custom
  (inferior-julia-args "--color=yes" "You get color in julia inferior process"))

(use-package julia-mode
  :unless d/on-droid)

(use-package executable
  :ensure nil
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package flymake
  :ensure nil
  :hook
  (prog-mode . flymake-mode)
  :custom
  (python-flymake-command '("ruff" "--quiet" "--stdin-filename=stdin" "-")))

(use-package reformatter
  :hook
  (python-ts-mode . ruff-format-on-save-mode)
  (nix-mode . alejandra-format-on-save-mode)
  (ess-r-mode . styler-format-on-save-mode)
  :config
  (reformatter-define ruff-format
    :program "ruff"
    :args (list "format" "--stdin-filename" input-file "-"))

  (reformatter-define pyblack-format
    :program "python"
    :args (list "-m" "black" "-"))

  (reformatter-define alejandra-format
    :program "alejandra"
    :group 'nix-mode
    :lighter " AL")

  (reformatter-define styler-format
    :program "Rscript"
    :args (list "--vanilla" "-e" "con <- file(\"stdin\")
out <- styler::style_text(readLines(con))
close(con)
out")
    :lighter " styler")

  (reformatter-define shell-format
    :program "shfmt"
    )

  (reformatter-define nixfmt-rfc
    :program "nixfmt")
  )

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
  (eglot-events-buffer-size 0)

  :bind
  (:map eglot-mode-map
        ("C-c l r" . eglot-rename)
        ("C-c C-d" . eldoc)
        ("C-c l a" . eglot-code-actions)
        ("C-c l i" . consult-eglot-symbols))
  :config
  (fset #'jsonrpc--log-event #'ignore)
  (add-to-list 'eglot-server-programs '(nix-mode . ("nixd")))
  ;;   (add-to-list 'eglot-server-programs '(bash-ts-mode . ("bash-language-server")))
  ;;   (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman")))
  )

(use-package eglot-booster
  :after eglot
  :config
  (eglot-booster-mode))

(use-package xref
  :ensure nil
  :custom
  (xref-search-program 'ripgrep))

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
  (standard-indent 2)
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
     ("python" . python-ts-mode)))

  (devdocs-browser-data-directory (expand-file-name "var/devdocs" user-emacs-directory)))

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

(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-delay 0.1)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-style 'parenthesis)
  (show-paren-context-when-offscreen t))

(use-package colorful-mode
  :unless d/on-droid
  :config
  (global-colorful-mode))

(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(defconst jetbrains-ligature-mode--ligatures
  '("-->" "//" "/**" "/*" "*/" "<!--" ":=" "->>" "<<-" "->" "<-"
    "<=>" "==" "!=" "<=" ">=" "=:=" "!==" "&&" "||" "..." ".."
    "|||" "///" "&&&" "===" "++" "--" "=>" "|>" "<|" "||>" "<||"
    "|||>" "<|||" ">>" "<<" "::=" "|]" "[|" "{|" "|}"
    "[<" ">]" ":?>" ":?" "/=" "[||]" "!!" "?:" "?." "::"
    "+++" "??" "###" "##" ":::" "####" ".?" "?=" "=!=" "<|>"
    "<:" ":<" ":>" ">:" "<>" "***" ";;" "/==" ".=" ".-" "__"
    "=/=" "<-<" ">" "<=<" ""
    ">=>" ">>=" ">>-" ">-" "<~>" "-<" "-<<" "=<<" "---" "<-|"
    "<=|" "/\\" "\\/" "|=>" "|~>" "<~~" "<~" "~~" "~~>" "~>"
    "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</>" "</" "/>"
    "<->" "..<" "~=" "~-" "-~" "~@" "^=" "-|" "_|_" "|-" "||-"
    "|=" "||=" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#="
    "&="))

(sort jetbrains-ligature-mode--ligatures (lambda (x y) (> (length x) (length y))))

;; (dolist (pat jetbrains-ligature-mode--ligatures)
;;   (set-char-table-range composition-function-table
;;                         (aref pat 0)
;;                         (nconc (char-table-range composition-function-table (aref pat 0))
;;                                (list (vector (regexp-quote pat)
;;                                              0
;;                                              'compose-gstring-for-graphic)))))

(use-package combobulate
  :after treesit
  :preface
  (setq combobulate-key-prefix "C-c o")
  :hook
  ((python-ts-mode . combobulate-mode)
   (js-ts-mode . combobulate-mode)
   (html-ts-mode . combobulate-mode)
   (css-ts-mode . combobulate-mode)
   (yaml-ts-mode . combobulate-mode)
   (typescript-ts-mode . combobulate-mode)
   (json-ts-mode . combobulate-mode)
   (tsx-ts-mode . combobulate-mode)))

(defvar insert-pair-map
  (let ((map (make-sparse-keymap)))
    (define-key map [t] #'insert-pair)
    map))

(global-set-key (kbd "C-M-q") insert-pair-map)

(setf insert-pair-alist
      '((?\( ?\)) (?r ?\( ?\)) (?R "( " " )")
        (?\[ ?\]) (?b ?\[ ?\]) (?B "[ " " ]")
        (?\{ ?\}) (?c ?\{ ?\}) (?C "{ " " }")
        (?\< ?\>) (?o ?\< ?\>) (?O "< " " >")
        (?\" ?\") (?s ?\" ?\")
        (?\' ?\') (?t ?\' ?\')
        (?\` ?\') (?v ?\` ?\`)))

(defun +insert-pair-default-a (&rest args)
  (unless (caar args)
    (setf (caar args) 1))
  args)

(advice-add #'insert-pair :filter-args #'+insert-pair-default-a)

(use-package doc-view
  :ensure nil
  ;; :mode ("\\.epub\\'" . doc-view-mode)
  :bind (:map doc-view-mode-map
              ("M-g M-g" . doc-view-goto-page)
              ("<f8>" . doc-view-presentation)
              ("j" . doc-view-next-line-or-next-page)
              ("k" . doc-view-previous-line-or-previous-page)
              ("C-v" . doc-view-scroll-up-or-next-page)
              ("M-v" . doc-view-scroll-down-or-previous-page)
              ("I" . d/doc-view-theme)
              )
  :hook
  (doc-view-mode . (lambda () (setq-local pixel-scroll-precision-mode nil)))
  :custom-face
  (doc-view-svg-face ((t (:background "#000000" :foreground "#ffffff"))))
  :custom
  (doc-view-continuous t)
  (doc-view-mupdf-use-svg t)
  (doc-view-scale-internally t)
  (doc-view-image-width 900)
  (large-file-warning-threshold 700000000)
  (image-cache-eviction-delay 5))

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

(use-package saveplace-pdf-view
  :unless d/on-droid
  :demand t)

(use-package nov
  :functions
  (toggle-mode-line)

  :hook
  (nov-mode . (lambda () (toggle-mode-line) (variable-pitch-mode)))
  (nov-mode . shrface-mode)

  :mode ("\\.epub\\'" . nov-mode)
  :custom
  (nov-text-width 80)
  (nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
  (nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions))
  (nov-variable-pitch t))

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

(use-package gnus
  :hook
  (gnus-group-mode . gnus-topic-mode)
  (gnus-summary-mode . turn-on-gnus-mailing-list-mode)
  :bind
  (("C-c d g" . gnus)
   (:map gnus-summary-mode-map
         ("-" . gnus-summary-hide-thread)
         ("+" . gnus-summary-show-thread)
         ))

  :custom
  (gnus-directory (expand-file-name "feeds/gnews" user-emacs-directory))
  (gnus-startup-file (expand-file-name "newsrc" gnus-directory))

  (gnus-widen-article-window t)

  (gnus-select-method
   '(nnnil ""))

  (gnus-secondary-select-methods
   '((nntp "feedbase"
           (nntp-open-connection-function nntp-open-tls-stream) ; feedbase does not do STARTTLS (yet?)
           (nntp-port-number 563) ; nntps
           (nntp-address "feedbase.org"))
   (nntp "gwene" (nntp-address "news.gwene.org"))
   (nnrss "")
   ))

  ;; refer: https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/gnus-guide-en.org
  (gnus-thread-sort-functions
   '(gnus-thread-sort-by-most-recent-date))
  (gnus-use-cache t)
  (gnus-thread-hide-subtree t)


  ;; (gnus-summary-line-format "%U%R%z%d %I%(%[ %F %] %s %)\n")

  ;;; --- credits to u/unhammer
  ;; Save time by not checking for new groups (I'm already subscribed to what I want,
  ;; can always manually M-x gnus-find-new-newsgroups to check new groups)
  (gnus-check-new-newsgroups nil)
  (gnus-check-bogus-newsgroups nil)
  ;; By default only check groups this level or lower on startup
  ;; (use `C-u g' or `C-c M-g' to activate all groups):
  (gnus-activate-level 2)

  (gnus-auto-center-summary nil)
  (gnus-nov-is-evil nil)
  (gnus-show-threads t)
  (gnus-use-cross-reference nil)
  ;;;; Async prefetch – useful for newsgroups, maybe not so much for Maildir:
  ;; https://www.gnu.org/software/emacs/manual/html_mono/gnus.html#Asynchronous-Fetching
  (gnus-asynchronous t)

  ;;; credits - https://libreddit.kavin.rocks/r/emacs/comments/1cfv84p/tipps_on_gnus_summary_formatting/ - u/ballfresno
  ;; (gnus-summary-line-format "%1{%U%R%O %4k%} %3{%&user-date;%*%ud│%}%I%(%-16,16f%) %4{%s%}\n")
  (gnus-user-date-format-alist
   '(((gnus-seconds-today) . " %k:%M")
     ((+ (gnus-seconds-today) (* 24 3600)) . " %l %p")
     (604800 . " %a")
     (31536000 . "%e %b")
     (t . " %Y")))

  ;;; credits - https://github.com/jbranso/.emacs.d/blob/master/lisp/init-gnus.org
  (gnus-sum-thread-tree-indent "  ")
  (gnus-sum-thread-tree-root "● ")
  (gnus-sum-thread-tree-false-root "◯ ")
  (gnus-sum-thread-tree-single-indent "📰")
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

  (gnus-group-line-format "%P│%M%S%4y %B%(%G%)\n")

  (gnus-topic-line-format "%i╭ %(%{✪ %n%}%) %A   %g %v\n")


  (gnus-summary-display-arrow t)


  (gnus-face-1 'italic)
  (gnus-face-2 'bold)
  (gnus-face-3 'bold-italic)

  :config
  (setopt
   nnrss-group-alist
   '(
     ("manga" "https://nyaa.si/?page=rss&c=3_1&f=0")
     ))

  )

(setopt user-mail-address "idlip@protonmail.com"
        user-full-name "Dilip")

(use-package gnus
  :unless d/on-droid
  :config
  (add-to-list 'gnus-secondary-select-methods
               '(nnimap "protonmail"
                        (nnimap-stream plain)
                        (nnimap-address "127.0.0.1")
                        (nnimap-server-port 1143))))

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
  (load-file "~/d-sync/feeds/gnews/privmail.el"))

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

(use-package pubmed
  :commands (pubmed-search pubmed-advanced-search)
  :bind
  ("C-c s p" . pubmed-search)
  ("M-s p" . pubmed-search)
  (:map pubmed-mode-map
        ("o" . pubmed-save-note-file)
        ("d" . pubmed-save-file)
        ("f" . pubmed-get-fulltext))

  :custom
  (pubmed-fulltext-functions
   '(pubmed-pmc
     ;; pubmed-scihub
     ;; pubmed-springer
     pubmed-openaccessbutton
     pubmed-unpaywall
     ;; pubmed-dissemin
     ))

  (pubmed-default-directory "~/d-sync/reads/")
  )

(defun pubmed-open (url)
  "Open the fulltext PDF of URL."
  (let* ((filename (url-file-nondirectory url))
         (path (concat (temporary-file-directory) filename ".pdf")))
    (when path
      (condition-case err
          (url-copy-file url path 1)
        (file-already-exists
         (message "%s" (error-message-string err))))
      (funcall #'pubmed--open-file path))))

(defun pubmed-save-as (url)
  "Prompt for filename and save the fulltext PDF of URL."
  (let* ((default-filename (read-file-name "Article File name: ")))

    (condition-case err
        (progn
          (url-copy-file url default-filename t)
          ;; (call-interactively 'denote-rename-file)
          )

      (file-already-exists
       (message "%s" (error-message-string err))
       ))))

(defun pubmed-save-file ()
  "Function that can save the pubmed file with prompting a directory."
  (interactive)
  (let ((pubmed-fulltext-action 'pubmed-save-as))
    (pubmed-get-fulltext)))

(defun pubmed-save-note-file ()
  (interactive)
  (let ((pubmed-fulltext-action 'pubmed-save-as))
    (pubmed-get-fulltext))
  (org-capture nil "jj")
  (org-noter)
  )

(defun pubmed-copy-url ()
  (interactive)
  (let ((pubmedids (if (use-region-p)
                       (pubmed-get-uids-in-region)
                     (pubmed-get-uids)
                     )))
    (dolist (uids pubmedids)
      (kill-new (format "https://pubmed.ncbi.nlm.nih.gov/%s" uids)))))

(use-package url
  :ensure nil
  :custom
  (url-user-agent "")
  (url-privacy-level 'paranoid)
  ;; (url-mime-accept-string "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8 ")
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
  (shr-bullet "⁍ ")
  (shr-folding-mode t)
  (shr-max-width 80)
  (shr-max-image-proportion 0.9)
  (shr-width 80))

(use-package shr-color
  :ensure nil
  :defer t
  :custom
  (shr-color-visible-luminance-min 40 "Improve the contrast"))

(use-package shrface
  :hook
  (eww-after-render . shrface-mode)
  (devdocs-browser-eww-mode . shrface-mode)
  (gnus-article-mode . shrface-mode)

  :bind
  (:map shrface-mode-map
        ("<tab>" . shrface-outline-cycle)
        ("<backtab>" . shrface-outline-cycle-buffer)
        ("M-n" . shr-next-link)
        ("M-p" . shr-previous-link)
        ("M-l" . (lambda () (interactive) (shrface-links-consult) (embark-act)))
        ("M-h" . mark-paragraph)
        ("C-j" . shrface-next-headline)
        ("C-k" . shrface-previous-headline))
  :custom
  (shrface-bullets-bullet-list '("󰓏" "󰚀" "󰫤"  "󰴈" "" "󰄄"))
  (shrface-href-versatile t)

  :init
  (setopt shrface-supported-faces-alist
          '((em . shrface-tag-em) (u . shrface-tag-u) (strong . shrface-tag-strong)
            (svg . shrface-tag-svg) (h1 . shrface-tag-h1) (h2 . shrface-tag-h2)
            (h3 . shrface-tag-h3) (h4 . shrface-tag-h4) (h5 . shrface-tag-h5)
            (h6 . shrface-tag-h6) (p . shrface-tag-p)
            (li . shrface-tag-li) (dt . shrface-tag-dt) (figure . shrface-tag-figure)))

  :config
  (defun shrface-shr-item-bullet ()
    "Build a `shr-bullet' based on `shrface-item-bullet'."
    (setq shr-bullet "⁍ "))

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
     (or (and (fboundp mode)
              (with-demoted-errors "Error while fontifying: %S"
                (shr-tag-pre-highlight-fontify code mode)))
         code))
    (shr-ensure-newline)
    (setq end (point))
    (add-face-text-property start end '(:inherit (fixed-pitch org-block)))
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
  (eww-search-prefix "https://searx.be/?q="))

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
    (setopt browse-url-browser-function 'browse-url-generic
            browse-url-generic-program "d-stuff"
            browse-url-secondary-browser-function 'browse-url-default-browser)))

(defun d/external-browser ()
  (interactive)
  (cond ((image-at-point-p) (kill-new (or (shr-url-at-point nil) (plist-get (cdr (image--get-image)) :file))))
        ((or (thing-at-point 'url t) (dired-file-name-at-point) (shr-url-at-point nil)) (link-hint-copy-link-at-point))
        (t (link-hint-copy-link)))
  (let ((url (current-kill 0)))
    (if d/on-droid (browse-url url) (browse-url-generic url))))

(use-package mpc
  :bind
  ("C-c d m" . mpc)
  (:map mpc-mode-map
        ("SPC" . mpc-playlist-add)
        ("RET" . mpc-select)
        ("C-k" . mpc-playlist-delete)
        ("m" . mpc-select-dwim)
        ("C-r" . mpc-songs-search)
        ("f" . mpc-ffwd)
        ("b" . mpc-rewind)
        )

  :custom
  (mpc-browser-tags '(Title|Album|Playlist))

  :config
  (defun mpc-select-dwim ()
    (interactive)
    (mpc-select-toggle)
    (next-line)
    )
 )

(use-package ready-player
  :unless d/on-droid
  :demand t
  :config
  (ready-player-mode))

(use-package image-mode
  :ensure nil
  :defines (d/on-droid olivetti-body-width)
  :unless d/on-droid
  :bind (:map image-mode-map
              ("q" . d/kill-buffer))
)

(use-package reddigg
  :defer t

  :defines
  (other-subs reddigg-subs )
  :commands (reddigg-view-sub)
  :functions
  (-concat reddigg--view-sub)

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

(use-package hnreader
  :defer t
  :unless d/on-droid)

(use-package howdoyou
  :defer t
  :unless d/on-droid)

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

(require 'alert)
(when d/on-droid
  ;; Org-alert functions

  (defun alert-android-notifications-notify (info)
    "Send notifications using `android-notifications-notify'.
`android-notifications-notify' is a built-in function in the native Emacs
Android port."
    (let ((title (or (plist-get info :title) "Android Notifications Alert"))
          (body (or (plist-get info :message) ""))
          (urgency (cdr (assq (plist-get info :severity)
                              alert-notifications-priorities)))
          (icon (or (plist-get info :icon) alert-default-icon))
          (replaces-id (gethash (plist-get info :id) alert-notifications-ids)))
      (android-notifications-notify
       :title title
       :body body
       :urgency urgency
       :icon icon
       :replaces-id replaces-id)))


  (alert-define-style 'android-notifications :title "Android Notifications"
                      :notifier #'alert-android-notifications-notify
                      )
  (setq alert-default-icon "ic_popup_reminder")
  )

(defvar d/font-size (if d/on-droid 170 240)
  "Default font size based on the system.")
(defvar d/variable-font-size (if d/on-droid 180 280)
  "Default variable pitch size")

;; Dont worry about the font name, I use fork of Recursive font

;; Set reusable font name variables
(defvar d/fixed-pitch-font "Code OnePiece"
  "The font to use for monospaced (fixed width) text.")

(defvar d/variable-pitch-font "Code Haki"
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

(defun d/change-font ()
  "Function to prompt for font change in simple way."
  (interactive)
  (let ((fpitch (completing-read "Fixed Pitch Font: " '("Iosevka Comfy" "JetBrainsMono Nerd Font" "Julia Mono" "Code OnePiece")))
        (vpitch (completing-read "Variable Font: " '("Merriweather" "Code Haki" "Iosevka Comfy Duo"))))
    (set-face-attribute 'fixed-pitch nil :font fpitch)
    (set-face-attribute 'variable-pitch nil :family vpitch))
  )

(use-package font-lock
  :ensure nil
  :defer t
  :custom ((font-lock-maximum-decoration t)
           (font-lock-global-modes '(not text-mode))
           (font-lock-verbose nil))
  :config
  (set-language-environment "UTF-8")
  (global-font-lock-mode 1))

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

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package haki-theme
  :demand t
  :load-path "~/.config/emacs/var/theme"
  :custom
  (haki-heading-font "Code D Ace")
  (haki-sans-font "Code D Haki")
  (haki-title-font "Code D Ace")
  ;; (haki-link-font "")
  ;; (haki-code-font "Code D Lip")
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
  :hook
  (org-mode text-mode Info-mode helpful-mode ement-room-mode
            shrface-mode
            gnus-article-mode
            sdcv-mode nov-mode elfeed-show-mode markdown-mode)
  :custom
  (olivetti-body-width 0.92)
  (olivetti-minimum-body-width 40)
  (olivetti-recall-visual-line-mode-entry-state t)
  )

;; new way of using mode-line with `mini-echo-mode`
(use-package mini-echo
  :unless d/on-droid
  :defer 1
  ;; :load-path "~/d-git/forks/mini-echo"
  :custom
  (mini-echo-window-divider-args '(t 0 0) "no indicator border")
  (mini-echo-separator " ")
  (mini-echo-buffer-status-style 'both)
  (mini-echo-default-segments
   '(:long ("time" "battery" "buffer-name"
            "envrc" "project" "eglot"
            "buffer-position"
            ;; "buffer-size"
            "flymake" "selection-info"
            "narrow"
            )
           :short ("buffer-name-short"
                   "selection-info" "narrow" "macro" "repeat")))

  (mini-echo-rules
   '((special-mode :both (("buffer-size" . 0)))
     (prog-mode :both (("vcs" . 1)))
     (dired-mode :both (("buffer-size" . 0)))))

  :config
  (setopt mini-echo--toggled-segments
          '(("battery" . t)
            ("flymake" . t)
            ;; ("elfeed". t)
            ;; ("pdf-tools". t)
            ("doc-view" . t)
            ("time" . t)))

  (mini-echo-mode 1))

(unless d/on-droid
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
    (concat (nerd-icons-mdicon "nf-md-battery")
            (string-trim (mini-echo-segment--extract battery-mode-line-string) "\\[" "\\]")))

  (mini-echo-define-segment "which-function"
    "Return the function at point using `which-function-mode'"
    :fetch
    (propertize
     (concat "󰡱 :"
             (string-trim
              (which-function)))
     'face 'which-func))

  (mini-echo-define-segment "doc-view"
    "Return the page number of the current document in Doc-view."
    :fetch
    (when (derived-mode-p 'doc-view-mode)
      (propertize
       (concat "  "
               (number-to-string (doc-view-current-page)) "/" (number-to-string (doc-view-last-page-number)))
       'face 'doc-view-svg-face)))



  (mini-echo-define-segment "pdf-tools"
    "Return the page number of current pdf in pdf-view."
    :fetch
    (when (derived-mode-p 'pdf-view-mode)
      (propertize
       (concat "  "
               (number-to-string (pdf-view-current-page)) "/" (number-to-string (pdf-cache-number-of-pages)))
       'face 'pdf-occur-page-face)))
  )

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
  (dashboard-center-content t)
  (dashboard-set-navigator t) ;; a custom made navigator
  (dashboard-projects-backend 'project-el)
  (dashboard-display-icons-p t)
  (dashboard-show-shortcuts nil)
  (dashboard-set-init-info nil)
  (dashboard-icon-type 'nerd-icons)

  (dashboard-agenda-prefix-format " %?-12t% s")
  (dashboard-agenda-time-string-format "%Y-%m-%d %H:%M")
  (dashboard-agenda-sort-strategy '(time-up))

  (dashboard-items
   '(
     ;; (recents . 4)
     (agenda . 15)
     ;; (projects . 3)
     (bookmarks . 5)
     ))

  (dashboard-startupify-list
   '(dashboard-insert-page-break
     dashboard-insert-banner dashboard-insert-newline dashboard-insert-banner-title
     dashboard-insert-newline dashboard-insert-navigator
     ;; dashboard-insert-newline dashboard-insert-init-info
     dashboard-insert-items dashboard-insert-newline
     ;; dashboard-insert-footer
     ))

  (dashboard-navigator-buttons
   `(;; line1
     ((,(nerd-icons-faicon "nf-fa-newspaper_o")
       " News"
       "Opens Gnus"
       (lambda (&rest _) (gnus)) nil "" " |")

      (,(nerd-icons-mdicon "nf-md-notebook")
       " Notes"
       "Denote Tree"
       (lambda (&rest _) (find-file "~/d-sync/notes/journal.org")) warning "" " |")

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
       (lambda (&rest _) (find-file "~/d-sync/reads")) nil "" "")
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
       (lambda (&rest _) (when d/on-droid (d/key-droid)) (org-agenda)) warning "" " |")

      (,(nerd-icons-mdicon "nf-md-bookmark")
       " Bookmark"
       "Open Bookmark File"
       (lambda (&rest _) (d/open-bookmark)) error "" "")

      )
     ;; Empty line
     ;; (("" "\n" "" nil nil "" ""))

     ;; Keybindings
     ))

  ;; (dashboard-footer-messages '("Power Maketh Man Beneath" "Manners Maketh Man" "Tasks, Break, Action Works all the time" "Stop thinking, Just do it"))
  ;; (dashboard-set-footer nil) ;; deprecated

  :config
  (dashboard-setup-startup-hook))

(use-package proced
  :bind ("C-x x p" . 'proced)
  :init (setq proced-auto-update-interval 1
              proced-enable-color-flag 1
              proced-format 'medium
              proced-sort 'rss)
  :hook
  (proced-mode . (lambda ()
                   (interactive)
                   (proced-toggle-auto-update 1))))

(use-package alert
  :custom
    ;; Use different backends depending on the platform
  (alert-default-style (if d/on-droid
                           'android-notifications
                         'libnotify))
  (alert-libnotify-additional-args '("-u" "critical" "-t" "100"))
  (alert-fade-time 100))

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
  :hook
  (org-mode . variable-pitch-mode)
  (org-mode . org-indent-mode)

  :bind
  ("C-c t R" . d/bionic-region)
  ("C-c t i" . d/set-timer)
  ("C-c t r" . d/bionic-read)
  (:map org-mode-map
        ("C-x n n" . d/narrow-or-widen-dwim)
        ("C-c l" . org-store-link)
        ("M-n" . org-shiftdown)
        ("M-p" . org-shiftup)
        )

  :custom
  (org-ellipsis " ⮟")
  (org-log-done 'note)
  (org-log-into-drawer t)
  (org-export-exclude-tags '("noexport" "ignore") "excludes these tagged heading from export")
  (org-latex-compiler "lualatex" "Lualatex is fast and gets custom font too")
  (org-link-file-path-type 'relative)

  (org-todo-keywords
   '((sequence "TODO(t)" "|" "DONE(d!)")
     (sequence "REVIEW(v)" "|" "CANC(k@)")))

  (org-refile-targets
   '(
     (org-default-notes-file :maxlevel . 6)
     (nil :maxlevel . 6)
     ))

  (org-tag-alist
   '((:startgroup)
     (:endgroup)
     ("project" . ?p)
     ("task" . ?t)
     ("devel" . ?d)
     ("note" . ?n)
     ("idea" . ?i)))

  (fill-column 80)
  (org-directory "~/d-sync/notes/")
  (org-default-notes-file (concat org-directory "journal.org"))
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


  (push '("conf-unix" . conf-unix) org-src-lang-modes)
  (defalias 'd/set-timer (symbol-function 'org-timer-set-timer))
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
;; (global-set-key (kbd "C-c r d") 'd/desk-ready)

;; credits to @xenodium
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

(use-package org-modern
  :commands
  (global-org-modern-mode)
  :hook (org-mode org-agenda-finalize)

  :custom
  ;; nerd-icons: "" "󰓏" "󰚀" "󰴈" "" "󰄄"

  ;; (org-modern-star '("◉" "✪" "◈" "✿" "❂"))
  ;; (org-modern-star '("" "󰓏" "󰚀" "󰴈" "" "󰄄"))
  (org-modern-replace-stars (string-replace " " "" "󰓏 󰚀 󰫤 󰴈  󰄄"))
  (org-modern-hide-stars 'leading)
  (org-modern-star 'replace)
  (org-modern-table nil) ;; issue with variable-pitch font

  (org-modern-list
   '((?* . "")
     (?- . "")
     (?+ . "")))

  (org-modern-checkbox '((?X . "")
                         (?- . "")
                         (?  . "")))

  (org-modern-keyword
   '(("options" . "")
     ("title" . "")
     ("author" . "󱆀") ("email" . "")
     ("startup" . "")
     ("property" . "")
     ("date" . "")
     ("tags" . "")
     ("todo" . "")
     (t . t)))

  (org-modern-block-name
   '(
     ("src" . ("" ""))
     ("example" . "")
     ("html" . "")
     ("quote" . ("" ""))
     (t . t)))

  (org-modern-internal-target '("  " t " "))

  :config
  (global-org-modern-mode))

(use-package org-agenda
  :ensure nil
  :after org
  :bind
  ("C-c d a" . org-agenda)
  ("C-c a a" . org-agenda)
  (:map org-agenda-mode-map
        ("C-x C-k" . org-agenda-exit))

  ;; :hook (org-agenda-finalize . org-agenda-entry-text-mode)
  :custom
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-log-mode-items '(closed clock state))
  ;; (org-agenda-todo-ignore-scheduled 'future)
  ;; TODO entries that can't be marked as done b/c of children are shown as dimmed in agenda view
  (org-agenda-dim-blocked-tasks nil)
  (org-agenda-inhibit-startup t)
  (org-agenda-ignore-properties '(effort appt category))
  ;; Start the week view on whatever day im on
  (org-agenda-start-on-weekday nil)
 ;; Agenda styling
  (org-agenda-tags-column 0)
  (org-agenda-block-separator ?─)
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")


  (org-agenda-files
   '("~/d-sync/notes/journal.org"
     "~/d-git/d-site/README.org"
     )))

(use-package org-capture
  :ensure nil
  :after org
  :defines
  (my-org-agenda-headlines)
  :bind
  ("C-c c" . org-capture)
  :custom
  ;; dont create a bookmark when calling org-capture
  (org-capture-bookmark nil)
  ;; also don't create bookmark in other things
  (org-bookmark-names-plist nil)
  (org-default-notes-file (concat org-directory "brain.org"))
  (org-capture-templates
   `(
     ;; ("a" "Agenda" entry (file+function "~/d-sync/notes/agenda.org" (lambda () (completing-read "Heading: " my-org-agenda-headlines)))
     ;;  "** TODO %?%^g\n  SCHEDULED:%U\n  %a\n  %i" :empty-lines 1 :clock-in t :clock-resume t)

     ;; ("n" "Notes")
     ;; ("nn" "Note to Brain" entry
     ;;  (file+headline org-default-notes-file "Notes")
     ;;  "** %?\n %U\n %i\n %a")
     ;; ("nt" "Note to Thought" entry
     ;;  (file+headline org-default-notes-file "Thoughts")
     ;;  "** %?\n %U\n %i\n %a")

     ;; ("nr" "Reading note" entry
     ;;  (file "~/d-sync/notes/reading.org")
     ;;  "** %?\n %U\n %i\n %a\n -")

     ;; ("nd" "Development note" entry
     ;;  (file "~/d-sync/notes/development.org")
     ;;  "** %?\n %U\n %i\n %a\n")

     ("c" "Contacts" entry (file "~/d-sync/notes/contacts.org")
      "* %(tempel-insert 'contact)")

     ("l" "Link" entry
      (file+headline "~/d-sync/notes/bookmarks.org" "gnus") "* %a\n")

     ("j" "Journal Entry" entry
      (file+datetree "~/d-sync/notes/journal.org")
      "\n* %<%H:%M> - %? %^G\n %a \n\n"
      :clock-in t :clock-resume t
      :empty-lines 1)

     ("t" "Tasks for the Day" checkitem
      (file+datetree "~/d-sync/notes/journal.org")
      "[ ] %?\n"
      )

     ("i" "Inbox Rough Notes" entry
      (file "~/d-sync/notes/inbox.org")
      "** %?\n %U\n %i %a\n - ")

     ))
  :config
  (setq my-org-agenda-headlines `(projects university tasks one-timer)))

(use-package org-list
  :custom
  (org-list-demote-modify-bullet '(("+" . "-") ("-" . "+"))))

(use-package org-src
  :ensure nil
  :after org
  :bind
  (:map org-mode-map
        ("C-c ;" . d/org-babel-edit))
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
  (org-babel-tangle '(4))
  (org-edit-special)

  ;; Now we should be in the special edit buffer with python-mode. Set
  ;; the buffer-file-name to the tangled file so that pylsp and
  ;; plugins can see an actual file.
  (setq-local buffer-file-name d/tangled-file-name)
  (eglot-ensure)
  )

(use-package org-id
  :commands org-id-create
  :custom
  (org-id-method 'ts)
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  ;; (org-id-ts-format (string-replace "here" (car (split-string (org-entry-get nil "ITEM") " ")) "%Y%m%dT%H%M%S-here"))
)

(use-package org-clock
  :ensure nil
  :after org
  :commands
  (org-clock-jump-to-current-clock
   org-clock-report)
  :custom
  ;; Save clock history accross emacs sessions (read var for required info)
  (org-clock-persist nil)
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

(use-package ob-core
  :ensure nil
  :after org
  :custom
  ;; Don't ask every time when I run a code block
  (org-confirm-babel-evaluate t)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (calc . t)
     (latex . t) (C . t)
     (R . t)
     (shell . t) (python . t)
     (julia . t))))

(use-package org-ql
  :after org
  :bind
  (:map org-mode-map
        ("C-c q f" . org-ql-find)
        ("C-c q s" . org-ql-search)
        ("C-c q l" . org-ql-open-link)
        ("C-c q v" . org-ql-view)))

(use-package org-super-agenda
  :after org
  :hook
  (org-agenda-mode . org-super-agenda-mode))

(use-package org-super-links
  :unless d/on-droid
  :bind
  (:map org-mode-map
        ("C-c s s" . org-super-links-link)
        ("C-c s l" . org-super-links-store-link)
        ("C-c s C-l" . org-super-links-insert-link)
        ("C-c s d" . org-super-links-quick-insert-drawer-link)
        ("C-c s i" . org-super-links-quick-insert-inline-link)
        ("C-c s C-d" . org-super-links-delete-link))
  :custom
  (org-super-links-related-into-drawer "REFERENCE")
  (org-super-links-link-prefix 'org-super-links-link-prefix-timestamp)
  (org-super-links-backlink-into-drawer "CITATION"))

(use-package org-fold
  :after org
  :custom
  (org-fold-show-context-detail
   '((agenda . local) (tags-tree . local) (bookmark-jump . lineage)
     (isearch . lineage) (default . ancestors)))
  ;; (org-fold-core-style 'overlays)
  )

(use-package org-alert
  :config
  (setopt org-alert-interval 300)
  (setopt org-alert-notification-title "Org Alert Reminder")
  (setopt org-alert-time-match-string
   "\\(?:SCHEDULED\\|DEADLINE\\):.*?<.*?\\([0-9]\\{2\\}:[0-9]\\{2\\}\\).*>")
  (org-alert-enable))

(use-package org-noter
  :unless d/on-droid
  :after org
  :custom
  (org-noter-auto-save-last-location t)
  (org-noter-default-notes-file-names '("journal.org"))
  (org-noter-notes-search-path '("~/d-sync/notes"))
  (org-noter-notes-window-location 'horizontal-split)
  )

;; empty function so org-noter does not scroll the display which hides the left margin, like -+ indicator
(defun org-noter--set-notes-scroll () )

(use-package ox
  :after org
  :custom
  (org-export-backends '(org odt md man latex icalendar html ascii)))

(use-package remember
  :ensure nil
  :bind
  ("C-c r r" . remember)
  ("C-c r n"))

(use-package calendar
  :bind
  ("C-c d d" . calendar)
  :custom
  (diary-file "~/d-sync/notes/diary"))

(use-package markdown-mode
  :defer t
  :defines (markdown-mode-map)
  :functions (markdown-view-mode)
  :mode
  "\\.md\\'"
  "\\.Rmd\\'"
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

(use-package jinx
  :unless d/on-droid
  ;; :init (global-jinx-mode)
  :hook org-mode
  :bind ("M-$". jinx-correct))

(use-package ispell
  :demand t
  :custom
  (ispell-alternate-dictionary (expand-file-name "~/.config/enchant/en_US.dic")))

(use-package flymake-languagetool
  :unless d/on-droid
  :disabled
  :hook
  (text-mode . flymake-languagetool-load)
  :custom
  (flymake-languagetool-server-command '("languagetool-http-server")))

(use-package speed-type
  :unless d/on-droid
  :hook
  (speed-type-mode . olivetti-mode))
