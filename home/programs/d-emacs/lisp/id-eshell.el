;;; id-eshell.el --- get me terminal -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package eat
  :hook
  (eshell-load . eat-eshell-mode))

(provide 'id-eshell.el)
;;; id-eshell.el ends here
