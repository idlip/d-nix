;;; id-enhance.el --- GUI enhancements -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:


(use-package mwheel
  :custom
  (mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
  (mouse-wheel-progressive-speed nil)
  (scroll-margin 4)
  (scroll-conservatively 101))

(use-package pixel-scroll
  :bind
  (("C-v" . d/scroll-down)
   ("M-v" . d/scroll-up))

  :config
  (pixel-scroll-precision-mode)
  (defun d/scroll-down ()
    "Trust me, make scrolling alot smoother.
+1 Makes you fall in love with Emacs again!"
    (interactive)
    (pixel-scroll-precision-scroll-down 20))

  (defun d/scroll-up ()
    "Trust me, adds a wonderfull smooth scroll.
You can do this by trackpad too (laptop)"
    (interactive)
    (pixel-scroll-precision-scroll-up 20)))

(use-package tooltip
  :defer t
  :custom
  (tooltip-mode -1))

(use-package time
  :defer t
  :custom
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  (display-time-mode t)
  (display-time-format "%H:%M"))

(use-package winner
  :config
  (winner-mode 1))


(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package rainbow-mode
  :defer t
  :hook '(prog-mode help-mode)
  :bind ("C-c t c" . rainbow-mode))

(use-package so-long
  :config (global-so-long-mode))

(use-package image-mode
  :ensure nil
  :unless d/on-droid
  :bind (:map image-mode-map
  ("q" . d/kill-buffer))
  :hook
  (image-mode . (lambda () (olivetti-mode) (setq olivetti-body-width 0.45))))

(use-package select
  :custom
  (selection-coding-system 'utf-8)
  (x-select-request-type 'text/plain\;charset=utf-8)
  (select-enable-clipboard t "Use the clipboard"))

(provide 'id-enhance.el)
;;; id-enhance.el ends here
