;;; id-notes.el --- note 'em all -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; spell check 'em too!
(use-package jinx
  ;; :init (global-jinx-mode)
  :hook org-mode
  :bind ("M-$". jinx-correct))

(use-package denote
  :defer t
  :defines
  (dired-mode-map
   denote-directory)
  :hook ((find-file-hook . denote-link-buttonize-buffer)

	       (dired-mode . denote-dired-mode))
  :bind
  ("C-c n j" . d/my-journal)
  ("C-c n s" . denote)
  ("C-c n t" . denote-type)
  ("C-c n d" . denote-date)
  ("C-c n n" . denote-subdirectory)
  ("M-s n" . denote-open-or-create)
  ("C-c n o" . denote-open-or-create)
  ("C-c n T" . denote-template)
  ("C-c n i" . denote-link)
  ("C-c n I" . denote-link-add-links)
  ("C-c n b" . denote-link-backlinks)
  ("C-c n f f" . denote-link-find-file)
  ("C-c n f b" . denote-link-find-backlink)
  ("C-c n r" . denote-rename-file)
  ("C-c n R" . denote-rename-file-using-front-matter)
  (:map dired-mode-map
	      ("C-c C-d C-i" . denote-link-dired-marked-notes)
	      ("C-c C-d C-r" . denote-dired-rename-marked-files)
	      ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))

  :custom
  (denote-directory (expand-file-name "~/d-sync/notes"))
  (denote-known-keywords '("emacs" "blogs" "article"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-file-type nil)
  (denote-prompts '(title keywords))
  (denote-excluded-directories-regexp nil)
  (denote-excluded-keywords-regexp nil)
  (denote-date-prompt-use-org-read-date t)
  (denote-allow-multi-word-keywords t)
  (denote-date-format nil)
  (denote-backlinks-show-context t)
  (denote-dired-directories
   (list denote-directory
	       (thread-last denote-directory (expand-file-name "attachments"))
	       (expand-file-name "~/d-sync/notes/books/")))

  :config
  (defun d/my-journal ()
    (interactive)
    (let* ((date (org-read-date))
	         (time (org-time-string-to-time date))
	         (title (format-time-string "%A %d %B %Y" time))
	         (initial (denote-sluggify title))
	         (target (read-file-name "Select note: " (denote-directory) nil nil initial
				                           (lambda (f)
				                             (or (denote-file-has-identifier-p f)
					                               (file-directory-p f))))))
	    (if (file-exists-p target)
	        (find-file target)
	      (denote title '("journal") denote-file-type nil date))))


  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%l\n%i\n%?")
    (add-to-list 'org-capture-templates
		             '("n" "New note (denote)" plain
		               (file denote-last-path)
		               #'denote-org-capture
		               :no-save t
		               :immediate-finish nil
		               :kill-buffer t
		               :jump-to-captured t)))

  (defun d/denote-add-to-agenda-files (keyword)
    "Append list of files containing 'keyword' to org-agenda-files"
    (interactive)
    ;; (jab/init-org-agenda-files) ;; start over
    (setq org-agenda-files (append org-agenda-files (directory-files denote-directory t keyword))))

  ;; (d/denote-add-to-agenda-files "_project")
  )

;; custom way to open up productive desk
;; from janusworx blog
(defun d/desk-ready ()
  "Getting ready for work."
  (interactive)
  (toggle-frame-maximized)
  (split-window-below)
  (find-file "~/d-sync/notes/d-desk.org")
  (find-file-other-window "~/d-sync/notes/tasks.org"))
(global-set-key (kbd "C-c r") 'd/desk-ready)

(use-package flycheck-languagetool
  :disabled
  :hook
  (text-mode . flycheck-mode)
  :custom
  (flycheck-languagetool-server-command '("languagetool-http-server"))
  (flycheck-languagetool-language "auto"))

(provide 'id-notes)
;;; id-notes.el ends here
