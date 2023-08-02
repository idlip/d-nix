;;; id-markdown.el --- universal markdown -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package markdown-mode
  :defer t
  :mode "\\.md\\'"
  :hook (markdown-mode . variable-pitch-mode)
  :bind (:map markdown-mode-map
	            ("<f8>" . d/markdown-toggle))
  :config
  (defun d/set-markdown-header-font-sizes ()
    (dolist (face '((markdown-header-face-1 . 1.3)
		                (markdown-header-face-2 . 1.2)
		                (markdown-header-face-3 . 1.15)
		                (markdown-header-face-4 . 1.1)
		                (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :weight 'normal :font haki-heading-font :height (cdr face))))

  (defun d/markdown-mode-hook ()
    (d/set-markdown-header-font-sizes))

  (defun d/markdown-toggle ()
    "Toggle view mode and editing mode"
    (interactive)
    (if (derived-mode-p 'markdown-view-mode) (markdown-mode) (markdown-view-mode))
    (variable-pitch-mode 1)))


(provide 'id-markdown)
;;; id-markdown.el ends here
