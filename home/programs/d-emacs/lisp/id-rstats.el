;;; id-rstats.el --- R -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package ess
  :defer t

  :custom
  (ess-use-flymake t)
  (ess-use-company nil)
  (ess-eval-visibly nil)
  (ess-ask-for-ess-directory nil)

  (ess-use-eldoc t)
  (ess-eldoc-show-on-symbol t)

  (ess-R-font-lock-keywords '((ess-R-fl-keyword:keywords . t)
				              (ess-R-fl-keyword:constants . t)
				              (ess-R-fl-keyword:modifiers . t)
				              (ess-R-fl-keyword:fun-defs . t)
				              (ess-R-fl-keyword:assign-ops . t)
				              (ess-R-fl-keyword:%op% . t)
				              (ess-fl-keyword:fun-calls . t)
				              (ess-fl-keyword:numbers . t)
				              (ess-fl-keyword:operators . t)
				              (ess-fl-keyword:delimiters . t)
				              (ess-fl-keyword:= . t)
				              (ess-R-fl-keyword:F&T . t)))

  (inferior-R-font-lock-keywords '((ess-S-fl-keyword:prompt . t)
                                   (ess-R-fl-keyword:keywords . t)
                                   (ess-R-fl-keyword:constants . t)
                                   (ess-R-fl-keyword:modifiers . t)
                                   (ess-R-fl-keyword:messages . t)
                                   (ess-R-fl-keyword:fun-defs . t)
                                   (ess-R-fl-keyword:assign-ops . t)
                                   (ess-fl-keyword:matrix-labels . t)
                                   (ess-fl-keyword:fun-calls . t)
                                   (ess-fl-keyword:numbers . t)
                                   (ess-fl-keyword:operators . t)
                                   (ess-fl-keyword:delimiters . t)
                                   (ess-fl-keyword:= . t)
                                   (ess-R-fl-keyword:F&T . t)))

)

(use-package ess-r-mode
  :bind
  (:map ess-mode-map
        ("C-;" . ess-insert-assign)))



(provide 'id-rstats)
;;; id-rstats.el ends here
