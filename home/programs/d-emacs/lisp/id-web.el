;;; id-web.el --- never leave emacs -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:


(use-package shr-tag-pre-highlight
  :ensure t
  ;;:defer t
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight)))

(use-package url
  :custom
  (url-user-agent "")
  (url-privacy-level 'paranoid)
  (url-mime-accept-string "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8 ")
  (url-mime-charset-string nil)
  (url-mime-language-string "en-US,en;q=0.5")
  (url-mime-encoding-string "gzip, deflate")
  :config
  (url-setup-privacy-info))

(use-package shr
  :defer t
  :custom
  (shr-use-fonts  t)
  (shr-use-colors nil)
  (shr-indentation 4)
  (shr-bullet "• ")
  (shr-folding-mode t)
  (shr-max-width 120)
  (shr-max-image-proportion 0.4)
  (shr-width nil))

(use-package shr-color
  :defer t
  :custom
  (shr-color-visible-luminance-min 80 "Improve the contrast"))

(use-package eww
  :commands (eww eww-search-words)
  :hook (eww-mode . variable-pitch-mode)
  :bind ("M-s M-w" . eww-search-words)
  (:map eww-mode-map
        ("e" . readable-article)
        ("Q" . d/kill-buffer)
        ("<return>" . eww-follow-link)
        ("m" . elfeed-toggle-star)
        ("b" . d/external-browser))
  :custom
  (eww-search-prefix "https://duckduckgo.com/html/&q="))

(use-package gnutls
  :defer t
  :custom
  (gnutls-verify-error t))

(use-package browse-url
  :config
  ;; browser script
  (unless d/on-droid
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "d-stuff")
    (setq browse-url-secondary-browser-function 'browse-url-generic
          browse-url-generic-program "d-stuff")))

(use-package ox-hugo
  :unless d/on-droid
  :after ox
  :config
  (with-eval-after-load 'org-capture
    (defun org-hugo-new-subtree-post-capture-template ()
      "Returns `org-capture' template string for new Hugo post.
  See `org-capture-templates' for more information."
      (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
	           (fname (org-hugo-slug title)))
	      (mapconcat #'identity
		               `(
		                 ,(concat "* TODO " title)
		                 ":PROPERTIES:"
		                 ,(concat ":EXPORT_FILE_NAME: " fname)
		                 ":END:"
		                 "%?\n")          ;Place the cursor here finally
		               "\n")))

    (add-to-list 'org-capture-templates
		             '("w" "Website Organize"))
    (add-to-list 'org-capture-templates
		             '("wt" "website Todo" entry (file+headline "~/d-git/d-site/README.org" "Ideas - TODO")
		               "* TODO %?\n  SCHEDULED:%T\n " :empty-lines 1))
    (add-to-list 'org-capture-templates
		             '("ww" "website work"
		               entry
		               (file+olp "~/d-git/d-site/org-mode/posts.org" "Posts")
		               (function org-hugo-new-subtree-post-capture-template))))
  )

(defun d/external-browser ()
  (interactive)
  (cond ((image-at-point-p) (kill-new (or (shr-url-at-point nil) (plist-get (cdr (image--get-image)) :file))))
	      ((or (thing-at-point 'url t) (dired-file-name-at-point) (shr-url-at-point nil)) (link-hint-copy-link-at-point))
	      (t (link-hint-copy-link)))
  (let ((url (current-kill 0)))
    (if d/on-droid (browse-url url) (browse-url-generic url))))

(provide 'id-web.el)
;;; id-web.el ends here
