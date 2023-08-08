;;; id-eshell.el --- get me terminal -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package eat
  :hook
  (eshell-load . eat-eshell-mode)
  :bind ("C-c d t" . eat)
  :config
  (add-to-list 'eat-semi-char-non-bound-keys '[134217843]))


(provide 'id-eshell)
;;; id-eshell.el ends here
