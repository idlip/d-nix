;;; id-theme.el --- haki -*- lexical-binding: t -*-
;;; Commentary:

;; I use my own made theme, called Haki.
;; Refer: https://github.com/idlip/haki

;;; Code:


(add-to-list 'custom-theme-load-path "~/.config/emacs/var/theme/")
(load-theme 'haki t)
;; (add-hook 'post-command-hook #'haki-modal-mode-line)

;; For foot to show colors properly
(add-to-list 'term-file-aliases '("foot" . "xterm"))

(use-package modus-themes
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t)
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

(provide 'id-theme)
;;; id-theme.el ends here
