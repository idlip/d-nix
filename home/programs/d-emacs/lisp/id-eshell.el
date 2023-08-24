;;; id-eshell.el --- get me terminal -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package eshell
  :ensure nil
  :hook
  (eshell-mode . hide-mode-line-mode)
  (eshell-mode . (lambda () (setq outline-regexp eshell-prompt-regexp)))
  :bind
  ("<f12>" . d/eshell-toggle)
  (:map eshell-mode-map
        ("C-S-l" . d/clear-eshell))

  :custom
  (eshell-scroll-to-bottom-on-input t)
  (eshell-highlight-prompt t)
  (eshell-hist-ignoredups t)
  (eshell-cd-on-directory t)
  (eshell-pushd-dunique t)
  (eshell-last-dir-unique t)
  (eshell-last-dir-ring-size 32)
  (eshell-list-files-after-cd nil)
  (eshell-prefer-lisp-functions nil)

  (eshell-prompt-function
   (lambda nil
     (concat
      "\n"
      (propertize (eshell/pwd) 'face `(:foreground "lightblue1"))
      (propertize "\n  " 'face `(:foreground "palegreen"))
      )))
  (eshell-prompt-regexp "^  ")

  :config
  (defun d/clear-eshell ()
    (interactive)
    (eshell-send-input (eshell/clear-scrollback)))

  (defun d/eshell-toggle ()
    "Minimal hack to toggle eshell."
    (interactive)
    (cond
     ((derived-mode-p 'eshell-mode) (if (one-window-p) (switch-to-prev-buffer) (delete-window)))
     ((one-window-p) (progn (split-window-below) (other-window 1) (eshell)
			                (shrink-window 7)))
     (t (progn (other-window 1)
		       (if (derived-mode-p 'eshell-mode) (delete-window)
		         (progn (other-window -1) (split-window-below) (other-window 1) (eshell) (shrink-window 7))))))))

(use-package em-hist
  :bind
  (:map eshell-hist-mode-map
        ("M-s" . nil)
        ("M-s r" . consult-ripgrep)
        ("M-s s" . consult-history))
  :custom
  (eshell-buffer-maximum-lines 10000)
  (eshell-history-size 10000))

(use-package eat
  :commands (eshell d/eshell-toggle d/eat-toggle)
  :hook
  (eshell-load . eat-eshell-mode)
  (eat-mode . hide-mode-line-mode)
  :bind
  ("C-c d t" . d/eat-toggle)
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
    "Minimal hack to toggle eat."
    (interactive)
    (cond
     ((derived-mode-p 'eat-mode) (if (one-window-p) (switch-to-prev-buffer) (delete-window)))
     ((one-window-p) (progn (split-window-below) (other-window 1) (eat)
			                (shrink-window 7)))
     (t (progn (other-window 1)
		       (if (derived-mode-p 'eat-mode) (delete-window)
		         (progn (other-window -1) (split-window-below) (other-window 1) (eat) (shrink-window 7))))))))

(use-package em-smart
  :custom
  (eshell-where-to-jump 'begin)
  (eshell-review-quick-commands nil)
  (eshell-smart-space-goes-to-end t))


(provide 'id-eshell)
;;; id-eshell.el ends here
