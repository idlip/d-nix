;;; id-eshell.el --- get me terminal -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package eat
  :hook
  (eshell-load . eat-eshell-mode)
  (eat-mode . hide-mode-line-mode)
  :bind ("C-c d t" . eat)
  (:map eat-mode-map
        ("C-x C-q" . d/eat-read-write)
	    ("<f9>" . hide-mode-line-mode))
  (:map eat-semi-char-mode-map
        ("M-s" . nil))

  :config
  (defun d/eat-read-write ()
    (interactive)
    (if eat--semi-char-mode (eat-emacs-mode) (eat-semi-char-mode))
    ))

(use-package eshell)

(use-package em-smart
  :custom
  (eshell-where-to-jump 'begin)
  (eshell-review-quick-commands nil)
  (eshell-smart-space-goes-to-end t))


(provide 'id-eshell)
;;; id-eshell.el ends here
