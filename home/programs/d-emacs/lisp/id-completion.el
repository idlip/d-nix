;;; id-completion.el --- completions everywhere -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package dabbrev
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
  (dabbrev-upcase-means-case-search t))

(use-package hippie-exp
  :bind
  ("M-/" . hippie-expand))

(use-package vertico
  :defines
  (vertico-map)
  :functions
  (vertico-mode )

  :bind (:map vertico-map
	          ("?" . minibuffer-completion-help)
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
      (marginalia-annotator-registry (command marginalia-annotate-binding)))
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

  (setq add-unicodes (unless d/on-droid (directory-files "~/d-git/d-bin/treasure/unicodes/" t "i"))))

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
  (corfu-history-mode
   corfu-popupinfo-mode
   corfu-echo-mode
   global-corfu-mode
   corfu-terminal-mode
   corfu-mode)

  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto nil)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-preview-current t)    ;; Disable current candidate preview
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-quit-no-match t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.2)
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
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

  ;; Add your own file with all words
  (setq cape-dict-file "~/.local/share/dict/vocab")

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


(provide 'id-completion)
;;; id-completion.el ends here
