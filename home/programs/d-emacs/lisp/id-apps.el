;;; id-apps.el --- do it all in emacs -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package reddigg
  :defer t

  :defines
  (other-subs reddigg-subs )

  :functions
  (-concat reddigg--view-sub)

  :bind (("C-c d f" . reddigg-view-frontpage)
	       ("C-c d r" . reddigg-view-sub))
  :custom
  (org-link-elisp-confirm-function 'y-or-n-p)
  (reddigg-subs '(emacs linux nixos hyprland bioinformatics onepiece fossdroid piracy bangalore india indiaspeaks developersindia manga aww))
  :config
  (setq other-subs '(crazyfuckingvideos nextfuckinglevel manga anime animepiracy fossdroid commandline memes jokes funnymemes rss holup unexpected todayilearned lifeprotips askreddit julia))

  (defun reddigg-view-sub ()
    "Prompt SUB and print its post list."
    (interactive)
    (let ((sub (completing-read "subreddit: " (-concat reddigg-subs other-subs '("frontpage" "comments")))))
      (cond ((string= sub "frontpage") (reddigg-view-frontpage))
            ((string= sub "comments") (reddigg-view-comments))
            (t (reddigg--view-sub sub)))))
  (defun reddigg--ensure-modes ()
    "Get a bunch of modes up and running."
    (if (equal major-mode 'org-mode)
        (org-set-startup-visibility)
      (org-mode)
      (font-lock-flush))
    (visual-line-mode)
    (jinx-mode -1)
    (view-mode 1)))

(use-package hnreader
  :defer t
  :unless d/on-droid)

(use-package howdoyou
  :defer t
  :unless d/on-droid)

(use-package devdocs-browser
  :unless d/on-droid
  :custom
  (devdocs-browser-major-mode-docs-alist:
   '((c++-ts-mode "cpp")
     (c-ts-mode "c")
     (python-ts-mode "Python")
     (emacs-lisp-mode "elisp"))))

;; (use-package howdoyou)
;; (use-package undo-fu
;;   :bind ("C-M-r" . undo-fu-only-redo)
;;   ("C-z" . undo-fu-only-undo)
;;   ("C-S-z" . undo-fu-only-redo-all))

(use-package mingus
  :unless d/on-droid
  :commands (d/elfeed-add-podcast)
  :defines
  (mingus-browse-mode-map)

  :bind ("C-c d m" . mingus-browse)
  (:map mingus-browse-mode-map
	      ("h" . mingus-browse-top-level)
	      ("l" . mingus-down-dir-or-play-song))
  :custom
  (mingus-mode-always-modeline t)
  (mingus-mode-line-string-max 15)
  (mingus-mode-line-show-volume nil)
  (mingus-mode-line-show-elapsed-time nil)
  (mingus-mode-line-show-elapsed-percentage t)
  (mingus-mode-line-show-consume-and-single-status nil))


;; (use-package wikinforg)

(use-package webpaste
  :defer t
  :defines
  (webpaste-provider-priority webpaste-paste-confirmation)

  :bind (("C-c C-p C-b" . webpaste-paste-buffer)
	       ("C-c C-p C-r" . webpaste-paste-region)
	       ("C-c C-p C-p" . webpaste-paste-buffer-or-region))
  :config
  (setq webpaste-provider-priority '("dpaste.org" "dpaste.com" "paste.mozilla.org"))
  ;; Require confirmation before doing paste
  (setq webpaste-paste-confirmation t))

(use-package sdcv
  :defer t
  :unless d/on-droid
  :defines
  (sdcv-say-word-p sdcv-dictionary-data-dir
                   sdcv-dictionary-simple-list
                   sdcv-popup-function sdcv-buffer-name
                   sdcv-mode-map)


  :hook (sdcv-mode . toggle-mode-line)
  :config
  (setq sdcv-say-word-p t
	    sdcv-dictionary-data-dir "~/d-git/d-bin/treasure/dict/"
	    sdcv-dictionary-simple-list
	    '("wn" "mw-thesaurus" "dict")
	    sdcv-popup-function 'popup-tip
	    sdcv-buffer-name "StarDict")
  :bind (("C-c d w" . sdcv-search-input)
	     ("C-c d d" . sdcv-search-input+))
  (:map sdcv-mode-map
	    ("q" . kill-buffer-and-window)
	    ("n" . sdcv-next-dictionary)
	    ("TAB" . hide-entry)
	    ("<backtab>" . show-entry)
	    ("p" . sdcv-previous-dictionary)))

(use-package proced
  :bind ("C-x x p" . 'proced)
  :init (setq proced-auto-update-interval 1
              proced-enable-color-flag 1
              proced-format 'medium
              proced-sort 'rss)
  :hook (proced-mode . (lambda ()
                         (interactive)
                         (proced-toggle-auto-update 1))))

(use-package speed-type
  :unless d/on-droid
  :hook
  (speed-type-mode . olivetti-mode))

(provide 'id-apps)
;;; id-apps.el ends here
