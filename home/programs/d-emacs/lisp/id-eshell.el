;;; id-eshell.el --- get me terminal -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package eshell
  :ensure nil
  :custom
  (eshell-buffer-maximum-lines 10000)
  (eshell-scroll-to-bottom-on-input t)
  (eshell-highlight-prompt nil)
  (eshell-history-size 1024)
  (eshell-hist-ignoredups t)
  (eshell-cd-on-directory t)
  (eshell-list-files-after-cd nil)
  (eshell-pushd-dunique t)
  (eshell-last-dir-unique t)
  (eshell-last-dir-ring-size 32)
  (eshell-list-files-after-cd nil))

(use-package eat
  :hook
  (eshell-load . eat-eshell-mode)
  (eat-mode . hide-mode-line-mode)
  :bind
  ("C-c d t" . eat)
  ("<f12>" . d/eat-toggle)
  (:map eat-mode-map
        ("C-x C-q" . d/eat-read-write)
        ("<f12>" . d/eat-toggle)
	    ("<f9>" . hide-mode-line-mode))
  (:map eat-semi-char-mode-map
        ("M-o" . nil)
        ("M-s" . nil))

  :config
  (defun d/eat-read-write ()
    (interactive)
    (if eat--semi-char-mode (eat-emacs-mode) (eat-semi-char-mode))
    )

  (defun d/eat-toggle ()
    "Minimal hack to toggle vterm."
    (interactive)
    (cond
     ((derived-mode-p 'eat-mode) (if (one-window-p) (switch-to-prev-buffer) (delete-window)))
     ((one-window-p) (progn (split-window-below) (other-window 1) (eat)
			                (shrink-window 7)))
     (t (progn (other-window 1)
		       (if (derived-mode-p 'eat-mode) (delete-window)
		         (progn (other-window -1) (split-window-below) (other-window 1) (eat) (shrink-window 7))))))))

(use-package eshell)

(use-package em-smart
  :custom
  (eshell-where-to-jump 'begin)
  (eshell-review-quick-commands nil)
  (eshell-smart-space-goes-to-end t))


(provide 'id-eshell)
;;; id-eshell.el ends here
