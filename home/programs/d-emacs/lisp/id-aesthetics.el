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
  :disabled t
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

;; new way of using mode-line with `mini-echo-mode`
(use-package mini-echo
  :unless d/on-droid
  :load-path "~/d-git/forks/mini-echo"
  :demand t

  :config

  (defface mini-echo-elfeed
    '((t (:inherit elfeed-search-unread-count-face)))
    "Face for mini-echo segment of word count."
    :group 'mini-echo)

  ;; add elfeed unread counts
  (mini-echo-define-segment "elfeed"
    "Return unread feeds counts from elfeed."
    :fetch
    (propertize
     (let ((bufn "*elfeed-search*"))
       (if (get-buffer bufn)
           (concat "󰎕 "

                   (string-trim-right
                    (with-current-buffer bufn (elfeed-search--count-unread))
                    "/.*")) "")) 'face 'mini-echo-elfeed))

  (mini-echo-define-segment "battery"
    "Return the battery status.
Display format is inherited from `battery-mode-line-format'."
    :setup (display-battery-mode 1)
    :fetch
    (propertize
     (concat "󰁿"
     (string-trim
      (battery-format "%p%"
                      (funcall battery-status-function))))
                'face 'mini-echo-battery))

  (setopt mini-echo--toggled-segments '(("battery" . t) ("elfeed". t) ("time" . t)))

  (mini-echo-mode 1))

(use-package frame
  :ensure nil
  :bind
  ("<f9>" . toggle-mode-line)
  :config
  (defun toggle-mode-line ()
    "toggle the modeline on and off."
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
  (dashboard-banner-logo-title "let's get to more 🔱 tasks today!")
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
     ((,(nerd-icons-faicon "nf-fa-newspaper_o")
       " News"
       "Opens Elfeed"
       (lambda (&rest _) (d/elfeed-open)) nil "" " |")

      (,(nerd-icons-mdicon "nf-md-notebook")
       " Notes"
       "Denote Tree"
       (lambda (&rest _) (call-interactively #'denote-open-or-create)) warning "" " |")

      (,(nerd-icons-faicon "nf-fa-gitlab")
       " Project"
       "Open Project finder"
       (lambda (&rest _) (project-find-file)) error "" " |")

      (,(nerd-icons-octicon "nf-oct-terminal")
       " Terminal"
       "Open Eshell/Eat"
       (lambda (&rest _) (if (string= (completing-read "Hello : " '("eat" "eshell")) "eat")
                             (eat) (eshell))) warning "" " |")

      (,(nerd-icons-mdicon "nf-md-library")
       " Library"
       "Books and Docs"
       (lambda (&rest _) (find-dired "~/d-sync/reads" "")) nil "" "")
      )
     ;; line 2
     (
      (,(nerd-icons-faicon "nf-fa-music")
       " Music"
       "Play Jazz/Rhythm"
       (lambda (&rest _) (if d/on-droid (d/key-droid) (mingus))) error "" " |")

      (,(nerd-icons-faicon "nf-fa-reddit_alien")
       " Geek"
       "Browse Info"
       (lambda (&rest _) (reddigg-view-sub)) warning "" " |")

      (,(nerd-icons-mdicon "nf-md-apple_keyboard_command")
       " M-x"
       "Execute Command"
       (lambda (&rest _) (call-interactively #'execute-extended-command)) nil "" " |")

      (,(nerd-icons-sucicon "nf-seti-todo")
       " Agenda"
       "TODO Agenda"
       (lambda (&rest _) (org-agenda)) warning "" " |")

      (,(nerd-icons-mdicon "nf-md-bookmark")
       " Bookmark"
       "Open Bookmark File"
       (lambda (&rest _) (d/open-bookmark)) error "" "")

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
