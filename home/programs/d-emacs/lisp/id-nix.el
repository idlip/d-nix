;;; id-nix.el --- nix language -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'")
  :bind (:map nix-mode-map
		          ("C-c C-e" . nix-eval-line))
  :config
  (defun nix-eval-dwim ()
    (interactive)
    (let* ((start (line-beginning-position))
	         (end (line-end-position))
	         (region-string (buffer-substring (region-beginning) (region-end)))
	         (msg (format "%s" (if (use-region-p) region-string (buffer-substring start end)))))
	    (pop-to-buffer "*Nix-REPL*")
	    (insert msg)
	    (comint-send-input)
	    (other-window 1))))

(use-package nix-drv-mode
  :ensure nix-mode
  :mode "\\.drv\\'")
(use-package nix-shell
  :ensure nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build))
(use-package nix-repl
  :ensure nix-mode
  :commands (nix-repl))


(provide 'id-nix.el)
;;; id-nix.el ends here
