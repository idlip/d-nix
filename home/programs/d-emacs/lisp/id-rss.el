;;; id-rss.el --- el feeds -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package elfeed
  :hook
  (elfeed-show-mode . d/elfeed-ui)

  :bind ("C-c d e" . d/elfeed-open)
  ("C-c d b" . embark-act)
  (:map elfeed-show-mode-map
        ("e" . elfeed-open-in-eww)
        ("i" . d/bionic-read)
        ("r" . elfeed-open-in-reddit)
        ("m" . elfeed-toggle-show-star)
        ("q" . d/elfeed-quit)
        ("C-x C-k" . d/elfeed-quit)
        ("P" . d/elfeed-add-podcast)
        ("A" . d/elfeed-play)
        ("b" . nil))
  (:map elfeed-search-mode-map
        ("m" . elfeed-toggle-star)
        ("q" . d/elfeed-quit)
        ("C-x C-k" . d/elfeed-quit)
        ("U" . elfeed-update)
        ("u" . elfeed-update-feed))
  :custom
  ;; (setq-default elfeed-search-filter "@1-week-ago--1-day-ago +unread -news +")
  (elfeed-search-filter "+unread +")
  (elfeed-search-date-format (if d/on-droid `("" 0 :left)  `("%d-%m 📰" 7 :left)))
  (elfeed-search-title-max-width 90)
  (elfeed-search-trailing-width 0)

  :config
  (defun elfeed-toggle-show-star ()
    (interactive)
    (if (elfeed-tagged-p 'star elfeed-show-entry)
        (elfeed-show-untag 'star)
      (elfeed-show-tag 'star)))
  ;; (org-capture nil "l"))
  (defun elfeed-toggle-star ()
    (interactive)
    (elfeed-search-toggle-all 'star))
  ;; (org-capture nil "l"))

  (defun d/elfeed-ui ()
    (interactive)
    ;; (setq-local header-line-format " ")
    (variable-pitch-mode)
    (shrface-mode))

  (defun d/elfeed-open ()
    "Wrapper to load the elfeed db from disk before opening"
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force))
  ;; (elfeed-update))

  ;;write to disk when quiting
  (defun d/elfeed-quit ()
    "Wrapper to save the elfeed db to disk before burying buffer"
    (interactive)
    (elfeed-db-save)
    (quit-window))

  ;; play podcasts
  (defun d/elfeed-add-podcast ()
    "Play the enclosure URL in Mpd using 'mingus'."
    (interactive)
    (with-no-warnings
    (let* ((count (length (elfeed-entry-enclosures elfeed-show-entry)))
           (entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)))
           (dlink (shell-command-to-string (format "yt-dlp -f bestaudio -g '%s'" (shr-url-at-point nil)))))
      (require 'mingus)
      ;; (message (concat "Added: " (car (elt (elfeed-entry-enclosures elfeed-show-entry)
      ;;                                      (- enclosure-index 1)))))
      (message dlink)
      (mingus-add dlink))))
       ;; (cond ((shr-url-at-point nil) (shell-command-to-string (format "yt-dlp -f bestaudio -g '%s'" (shr-url-at-point current-prefix-arg))))
       ;;       ((derived-mode-p 'elfeed-show-mode)
       ;;        (if (zerop count)
       ;;            (shell-command-to-string (format "yt-dlp -f bestaudio -g '%s'" (elfeed-entry-link entry)))
       ;;          (car (elt (elfeed-entry-enclosures elfeed-show-entry)
       ;;                    (- enclosure-index 1)))))
       ;;       ((derived-mode-p 'elfeed-search-mode) (elfeed-search-selected :single))))))

  (defun d/elfeed-play ()
    (interactive)
    (let* ((count (length (elfeed-entry-enclosures elfeed-show-entry)))
           (entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single))))
      (message (concat "Added: " (elfeed-entry-link entry)))
      (if (zerop count)
          (async-shell-command (format "mpc add $(yt-dlp -g \"%s\")" (elfeed-entry-link entry)) nil nil)
        (with-no-warnings
          (mingus-add (car (elt (elfeed-entry-enclosures elfeed-show-entry)
                                (- enclosure-index 1))))))
      ))

  ;; face for starred articles
  (defface elfeed-search-star-title-face
    '((t :foreground "#f77"))
    "Marks a starred Elfeed entry.")

  (push '(star elfeed-search-star-title-face) elfeed-search-face-alist)
  (defun d/elfeed-org-mark ()
    "use org file as bookmark for elfeed entries.
Usable as favorites or bookmark."
    (when elfeed-show-entry
      (let* ((link (elfeed-entry-link elfeed-show-entry))
             (title (elfeed-entry-title elfeed-show-entry)))
        (org-store-link-props
         :link link
         :description title))))

  (defun elfeed-open-in-eww ()
    "open elfeed entry in eww."
    (interactive)
    (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single))))
      (eww-browse-url (elfeed-entry-link entry) t)))

  (defun elfeed-open-in-reddit ()
    "open elfeed entry in reddit"
    (interactive)
    (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single))))
      (reddigg-view-comments (elfeed-entry-link entry)))
    (display-buffer-pop-up-window (reddigg--get-cmt-buffer) nil))

  (when d/on-droid
    (define-key elfeed-show-mode-map (kbd "<volume-up>") #'elfeed-show-prev)
    (define-key elfeed-show-mode-map (kbd "<volume-down>") #'elfeed-show-next)))

(use-package elfeed-org
  :after elfeed
  :custom
  (rmh-elfeed-org-files (list (expand-file-name "elfeed.org" user-emacs-directory)))
  :init
  (elfeed-org))

(use-package avy
  :bind
  ("M-j" . avy-goto-char-timer)
  ("M-g w" . avy-goto-char-timer)
  ("M-K" . avy-kill-region)
  ("C-S-k" . avy-kill-whole-line)
  :custom
  (avy-background t))

(provide 'id-rss)
;;; id-rss.el ends here
