;;; id-eshell.el --- get me terminal -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package esh-mode
  :ensure nil
  :defines
  (eshell-prompt-regexp)
  :commands
  (eshell-send-input eshell/clear)
  :hook
  (eshell-mode . toggle-mode-line)
  (eshell-mode . electric-pair-local-mode)
  (eshell-mode . (lambda () (setq outline-regexp eshell-prompt-regexp) (setq-local corfu-auto nil)
                   (corfu-mode)))
  :bind
  ("<f12>" . d/eshell-toggle)
  (:map eshell-mode-map
        ("C-S-l" . d/clear-eshell))

  :custom
  (eshell-scroll-to-bottom-on-input t)
  (eshell-directory-name (expand-file-name "eshell" user-emacs-directory))
  (eshell-aliases-file (expand-file-name "alias" eshell-directory-name))
  (eshell-highlight-prompt t)
  (eshell-hist-ignoredups t)
  (eshell-cd-on-directory t)
  (eshell-visual-command nil)
  (eshell-pushd-dunique t)
  (eshell-last-dir-unique t)
  (eshell-last-dir-ring-size 32)
  (eshell-list-files-after-cd nil)
  (eshell-cd-shows-directory t)
  (eshell-prefer-lisp-functions nil)

  (eshell-prompt-function
   (lambda nil
     (concat
      "\n"
      (propertize (concat "  " (eshell/pwd)) 'face `(:foreground "lightblue1"))
      (when (package-installed-p 'magit) (propertize (if (magit-get-current-branch) (concat "   " (magit-get-current-branch)) "") 'face '(:foreground "orangered1")))
      (when (package-installed-p 'envrc) (propertize (if (string= envrc--status 'none) "" "   ") 'face '(:foreground "mediumspringgreen")))
      (propertize (concat "   " (format-time-string "%H:%M" (current-time))) 'face '(:foreground "lightcyan1"))
      (propertize "\n 𝝺 " 'face `(:foreground "palegreen"))
      )))
  (eshell-prompt-regexp "^ 𝝺 ")

  :config
  (defun d/clear-eshell ()
    (interactive)
    (eshell-send-input (eshell/clear t)))

  (defun d/eshell-toggle ()
    "Minimal hack to toggle eshell."
    (interactive)
    (cond
     ((derived-mode-p 'eshell-mode) (if (one-window-p) (switch-to-prev-buffer) (delete-window)))
     ((one-window-p) (progn (select-window (split-window-below)) (shrink-window 7) (eshell)))
     (t (progn (other-window 1)
		       (if (derived-mode-p 'eshell-mode) (delete-window)
		         (progn (other-window -1) (select-window (split-window-below)) (shrink-window 7) (eshell))))))))

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
  :ensure nil
  :custom
  (eshell-where-to-jump 'begin)
  (eshell-review-quick-commands nil)
  (eshell-smart-space-goes-to-end t))


(provide 'id-eshell)
;;; id-eshell.el ends here
