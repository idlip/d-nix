;;; id-reading.el --- now study -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

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

(provide 'id-reading)
;;; id-reading.el ends here
