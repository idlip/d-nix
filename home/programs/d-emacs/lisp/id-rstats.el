;;; id-rstats.el --- R -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package ess
  :defer t
  :custom
  (ess-use-flymake nil)
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
				                      (ess-R-fl-keyword:F&T . t))))


(provide 'id-rstats.el)
;;; id-rstats.el ends here
