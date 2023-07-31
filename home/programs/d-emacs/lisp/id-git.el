;;; id-git.el --- version control -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:


(use-package magit
  :defer t
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-refine-hunk t))

(use-package ediff
  :custom
  (ediff-keep-variants nil)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-default))

(provide 'id-git.el)
;;; id-git.el ends here
