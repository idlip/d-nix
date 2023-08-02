;;; id-vterm.el --- vterm -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package vterm
  :defer t
  :hook (vterm-mode-hook .
			                   (lambda ()
			                     (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
			                     (buffer-face-mode t)))
  :bind
  (("C-c d t" . multi-vterm)
   ("<f12>" . d/vt-toggle)
   ("C-c t v" . d/vt-toggle))
  (:map vterm-mode-map
	      ("<f12>" . d/vt-toggle)
	      ("C-c t v" . d/vt-toggle)
	      ("<f9>" . hide-mode-line-mode)
	      ("C-q" . vterm-send-next-key))
  :custom
  (vterm-shell "/etc/profiles/per-user/i/bin/zsh")

  :config
  (defun d/vt-toggle ()
    "Minimal hack to toggle vterm."
    (interactive)
    (cond
     ((derived-mode-p 'vterm-mode) (if (one-window-p) (switch-to-prev-buffer) (delete-window)))
     ((one-window-p) (progn (split-window-below) (other-window 1) (multi-vterm-next)
			                      (if (package-installed-p 'hide-mode-line) (hide-mode-line-mode) nil) (shrink-window 7)))
     (t (progn (other-window 1)
		           (if (derived-mode-p 'vterm-mode) (delete-window)
		             (progn (other-window -1) (split-window-below) (other-window 1) (multi-vterm-next) (if (package-installed-p 'hide-mode-line) (hide-mode-line-mode) nil) (shrink-window 7))))))))

(use-package multi-vterm
  :bind (:map vterm-mode-map
		          ("M-n" . multi-vterm-next)
		          ("M-p" . multi-vterm-prev))
  :custom
  (multi-vterm-dedicated-window-height-percent 30))


(provide 'id-vterm)
;;; id-vterm.el ends here
