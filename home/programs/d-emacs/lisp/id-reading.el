;;; id-reading.el --- now study -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package shrface
  :hook ((eww-mode . shrface-mode)
	       (elfeed-show-mode . shrface-mode)
	       (nov-mode . shrface-mode))
  :bind (:map shrface-mode-map
	            ("<tab>" . shrface-outline-cycle)
	            ("<backtab>" . shrface-outline-cycle-buffer)
	            ("M-n" . shr-next-link)
	            ("M-p" . shr-previous-link)
	            ("C-j" . shrface-next-headline)
	            ("C-k" . shrface-previous-headline))
  :custom
  (shrface-item-bullet 8226)
  (shrface-bullets-bullet-list org-modern-star)
  (shrface-href-versatile t)
  :config
  (shrface-basic)
  (shrface-trial))

(use-package nov
  :hook (nov-mode . hide-mode-line-mode)
  (nov-mode . variable-pitch-mode)
  :mode ("\\.epub\\'" . nov-mode)
  :custom
  (nov-text-width nil)
  (nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
  (nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions))
  (nov-variable-pitch t))

(use-package doc-view
  :ensure nil
  ;; :mode ("\\.epub\\'" . doc-view-mode)
  :bind (:map doc-view-mode-map
	            ("M-g M-g" . doc-view-goto-page)
	            ("<f8>" . doc-view-presentation))
  :custom
  (doc-view-continuous t)
  (doc-view-mupdf-use-svg t)
  (doc-view-image-width 900)
  (large-file-warning-threshold 700000000)
  (image-cache-eviction-delay 3))

(provide 'id-reading.el)
;;; id-reading.el ends here
