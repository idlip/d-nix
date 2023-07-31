;;; id-embark.el --- OP than you imagine -*- lexical-binding: t -*-
;;; Commentary:

;; This package alone is so powerful that it is overlooked,
;; as I find it not easy to grok.
;; Refer: https://karthinks.com/software/fifteen-ways-to-use-embark

;;; Code:

(use-package embark
  :defer t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
(use-package embark-consult
  :defer t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(provide 'id-embark.el)
;;; id-embark.el ends here
