;;; id-aesthetics.el --- aesthetic elegant looks -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package olivetti
  :defer t
  :disabled t
  :hook
  (org-mode text-mode Info-mode helpful-mode ement-room-mode
            sdcv-mode nov-mode elfeed-show-mode markdown-mode)
  :custom
  (olivetti-body-width 0.92)
  (olivetti-minimum-body-width 40)
  (olivetti-recall-visual-line-mode-entry-state t)
  :delight " ⊛")

(use-package doom-modeline
  ;; :disabled t ;; some flycheck error, until next upgrade
  :functions
  (doom-modeline-mode)
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-bar-width 7)
  (doom-modeline-major-mode-icon t)
  (inhibit-compacting-font-caches t)
  (doom-modeline-support-imenu t)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-time-icon t)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-buffer-name t)
  (doom-modeline-highlight-modified-buffer-name t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count t)
  (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-number-limit 99)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-workspace-name nil)
  (doom-modeline-persp-name nil)
  (doom-modeline-display-default-persp-name nil)
  (doom-modeline-persp-icon t)
  (doom-modeline-lsp t)
  (doom-modeline-github t)
  (doom-modeline-modal t)
  (doom-modeline-modal-icon t)
  (doom-modeline-battery t)
  (doom-modeline-env-version t)
  (doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
  (doom-modeline-env-load-string "...")

  (doom-modeline-height 30)
  (doom-modeline-buffer-encoding nil))

(use-package frame
  :ensure nil
  :bind
  ("<f9>" . toggle-mode-line)
  :config
  (defun toggle-mode-line ()
    "Toggle the modeline on and off."
    (interactive)
    (setq mode-line-format
          (if (equal mode-line-format nil)
              (default-value 'mode-line-format)))
    (redraw-display)))

(use-package dashboard
  :functions (dashboard-setup-startup-hook)

  :bind (:map dashboard-mode-map
              ("n" . 'dashboard-next-line)
              ("p" . 'dashboard-previous-line)
              )

  :custom
  (initial-buffer-choice 'dashboard-open)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-banner-logo-title "Let's Get to More 🔱 Tasks Today!")
  ;; (dashboard-startup-banner "~/.config/emacs/var/butterfly.png")
  (dashboard-startup-banner 'logo)
  (dashboard-image-banner-max-width 100) ;; 100 for logo
  (dashboard-center-content nil)
  (dashboard-set-navigator t) ;; a custom made navigator
  (dashboard-projects-backend 'project-el)
  (dashboard-display-icons-p t)
  (dashboard-show-shortcuts nil)
  (dashboard-set-init-info nil)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-items '((recents . 5)
                     (agenda . 10)
                     (projects . 2)
                     (bookmarks . 3)))
  (dashboard-modify-heading-icons '((recents . "file-text")
				                    (bookmarks . "book")))

  (dashboard-navigator-buttons
   `(;; line1
     ((," "
       "News"
       "Opens Elfeed"
       (lambda (&rest _) (d/elfeed-open)) nil "" " |")

      (,"󰠮 "
       "Notes"
       "Denote Tree"
       (lambda (&rest _) (call-interactively #'denote-open-or-create)) warning "" " |")

      (," "
       "Project"
       "Open Project finder"
       (lambda (&rest _) (project-find-file)) error "" " |")

      (," "
       "Terminal"
       "Open Eshell/Eat"
       (lambda (&rest _) (if (string= (completing-read "Hello : " '("eat" "eshell")) "eat")
                             (eat) (eshell))) warning "" " |")

      (,"󰌱 "
       "Library"
       "Books and Docs"
       (lambda (&rest _) (find-dired "~/d-sync/reads" "")) nil "" "")
      )
     ;; line 2
     (
      (," "
       "Music"
       "Play Jazz/Rhythm"
       (lambda (&rest _) (mingus)) error "" " |")

      (," "
       "Geek"
       "Browse Info"
       (lambda (&rest _) (reddigg-view-sub)) warning "" " |")

      (,"󰘳 "
       "M-x"
       "Execute Command"
       (lambda (&rest _) (call-interactively #'execute-extended-command)) nil "" " |")

      (," "
       "Agenda"
       "TODO Agenda"
       (lambda (&rest _) (org-agenda)) warning "" " |")

      (,"󰃀 "
       "Bookmark"
       "Open Bookmark File"
       (lambda (&rest _) (find-file "~/d-sync/notes/bookmarks.org")) error "" "")

      )
     ;; Empty line
     ;; (("" "\n" "" nil nil "" ""))

     ;; Keybindings
     ))

  (dashboard-footer-messages '("Power Maketh Man Beneath" "Manners Maketh Man" "Tasks, Break, Action Works all the time" "Stop thinking, Just do it"))

  :config
  (dashboard-setup-startup-hook))

(provide 'id-aesthetics)
;;; id-aesthetics.el ends here
