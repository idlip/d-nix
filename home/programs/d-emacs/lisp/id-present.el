;;; id-present.el --- Presentation in minimal way -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package org-present
  :defer t
  :after org
  :bind (:map org-present-mode-keymap
		      ("<right>" . d/org-present-next-slide)
		      ("<left>" . d/org-present-previous-slide)
		      ("<up>" . d/org-present-up)
		      ("C-c j" . d/org-present-next-slide)
		      ("C-c k" . d/org-present-previous-slide)
		      ("C-c h" . d/org-present-up)
		      ("<f5>" . d/org-present-refresh))
  (:map org-mode-map
	    ("<f8>" . d/org-present-mode))
  :hook ((org-present-mode . d/org-present-enable-hook)
	     (org-present-mode-quit . d/org-present-disable-hook))
  :config

  (defun org-present-header-line ()
    ;; (let* ((levelhead (concat "org-level-" (org-current-level)))
    ;;        )
    (setq-local header-line-format (concat "   󰨖 " (propertize (org-get-title) 'face 'org-document-title)  " : " (propertize (if (org-get-heading) (nth 4 (org-heading-components)) " ") 'face 'org-level-1)  "       -         " (propertize "Dilip" 'face 'org-document-info)))

    )

  (defvar d/org-present-org-modern-keyword '(("title"       . "")
					                         ("description" . "")
					                         ("subtitle"    . "")
					                         ("date"        . "")
					                         ("author"      . "")
					                         ("email"       . "")
					                         ("language"    . "")
					                         ("options"     . "")
					                         (t . t)))

  (define-minor-mode d/org-present-mode
    "Toggle Presentation Mode."
    :global nil
    :lighter "d/org-present-mode"
    (if d/org-present-mode
	    (org-present)
	  (org-present-quit)))

  (defun d/org-present-enable-hook ()
    (setq d/org-present--inhibit-message inhibit-message
	      d/org-present--echo-keystrokes echo-keystrokes
	      d/org-present--visual-line-mode visual-line-mode
	      d/org-present--org-ellipsis org-ellipsis)
    ;; d/org-present--org-indent-mode org-indent-mode)
    ;; (org-indent-mode 1)

    ;; Disable 'org-modern-mode' to setup adjustment if it's installed
    (if (package-installed-p 'org-modern)
	    (org-modern-mode 0))

    (if (package-installed-p 'org-modern)
	    (setq-local d/org-present--org-modern-hide-stars org-modern-hide-stars
		            d/org-present--org-modern-keyword org-modern-keyword
		            d/org-present--org-modern-block-fringe org-modern-block-fringe

		            org-modern-hide-stars 'leading
		            org-modern-block-fringe t
		            org-modern-keyword d/org-present-org-modern-keyword))

    (display-line-numbers-mode 0)

    (if (package-installed-p 'org-modern)
	    (org-modern-mode 1))

    (setq-local inhibit-message t
		        echo-keystrokes nil
		        cursor-type t
		        org-image-actual-width 300
		        header-line-format " "
		        org-ellipsis "󱞤")

    (dolist (face '((org-block . 1.0)
		            (org-block-begin-line . 0.1)
		            (org-document-info . 1.2)
		            (org-document-title . 1.2)
		            (org-level-1 . 1.2)
		            (org-level-2 . 1.2)
		            (org-level-3 . 1.1)
		            (org-level-4 . 1.1)
		            (org-level-5 . 1.1)
		            (org-level-6 . 1.1)
		            (org-code . 1.15)
		            (header-line . 1.0)
		            (org-verbatim . 1.15)
		            (variable-pitch . 1.1)
		            (org-level-7 . 1.1)))
	  (face-remap-add-relative (car face) :height (cdr face)))


    (if (package-installed-p 'hide-mode-line)
	    (hide-mode-line-mode 1))
    (org-present-header-line)

    (olivetti-set-width 0.80)

    (org-display-inline-images)
    (read-only-mode 1))

  (defun d/org-present-prepare-slide ()
    (org-overview)
    (org-show-entry)
    (org-show-children)
    (org-present-header-line))

  (defun d/org-present-disable-hook ()
    (setq-local header-line-format nil
		        face-remapping-alist '((default variable-pitch default))
		        org-adapt-indentation nil
		        visual-line-mode d/org-present--visual-line-mode
		        org-ellipsis d/org-present--org-ellipsis
		        inhibit-message d/org-present--inhibit-message
		        echo-keystrokes d/org-present--echo-keystrokes)
    (org-present-small)


    ;; (org-indent-mode d/org-present--org-indent-mode)

    (if (package-installed-p 'hide-mode-line)
	    (hide-mode-line-mode 0))

    ;; (load-theme 'haki t)
    (org-mode-restart)
    (org-remove-inline-images))

  (defun d/org-present-up ()
    "Go to higher heading from current heading."
    (interactive)
    (widen)
    (org-up-heading-safe)
    (org-present-narrow)
    (org-present-run-after-navigate-functions)
    (org-present-header-line))


  (defun d/org-present-next-slide ()
    "Go to next sibling."
    (interactive)
    (widen)
    (unless (org-goto-first-child)
	  (org-get-next-sibling))
    (org-present-narrow)
    (org-fold-hide-sublevels 5)
    (org-show-entry)
    (org-present-run-after-navigate-functions)
    (org-present-header-line))


  (defun d/org-present--last-child ()
    "Find last child of current heading."
    (when (org-goto-sibling) (d/org-present--last-child))
    (when (org-goto-first-child) (d/org-present--last-child)))


  (defun d/org-present-previous-slide ()
    "Go to previous sibling."
    (interactive)
    (widen)
    (when (org-current-level)
	  (org-back-to-heading)
	  (if (and (org-get-previous-sibling) (org-current-level))
	      (when (org-goto-first-child)
	        (d/org-present--last-child))))
    (org-present-narrow)
    (org-fold-hide-sublevels 5)
    (org-show-entry)
    (org-present-run-after-navigate-functions)
    (org-present-header-line))

  (defun d/org-present-refresh ()
    (interactive)
    (d/org-present-mode)
    (d/org-present-mode))

  )


(provide 'id-present)
;;; id-present.el ends here
