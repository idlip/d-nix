;;; id-code.el --- code with lsp -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package executable
  :ensure nil
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package loaddefs
  :ensure nil
  :hook
  (prog-mode . electric-pair-local-mode))

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
  (devdocs-browser-major-mode-docs-alist '((c++-ts-mode "cpp")
                                           (c-ts-mode "c")
                                           (go-ts-mode "go")
                                           (python-ts-mode "Python")
                                           (emacs-lisp-mode "elisp")
                                           (rust-ts-mode "rust")
                                           (cmake-mode "CMake")))

  (devdocs-browser-highlight-lang-mode-alist '(("c" . c-ts-mode)
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

(provide 'id-code)
;;; id-code.el ends here
