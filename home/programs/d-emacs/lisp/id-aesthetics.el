;;; id-aesthetics.el --- aesthetic elegant looks -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package olivetti
  :defer t
  :hook
  (org-mode text-mode Info-mode helpful-mode ement-room-mode
            eww-mode sdcv-mode nov-mode elfeed-show-mode markdown-mode)
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

(provide 'id-aesthetics.el)
;;; id-aesthetics.el ends here
