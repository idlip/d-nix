;;; id-web.el --- never leave emacs -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package shr-tag-pre-highlight
  :ensure t
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions '(pre . shrface-shr-tag-pre-highlight))
  (defun shrface-shr-tag-pre-highlight (pre)
    "Highlighting code in PRE."
    (let* ((shr-folding-mode 'none)
           (shr-current-font 'default)
           (code (with-temp-buffer
                   (shr-generic pre)
                   ;; (indent-rigidly (point-min) (point-max) 2)
                   (buffer-string)))
           (lang (or (shr-tag-pre-highlight-guess-language-attr pre)
                     (let ((sym (language-detection-string code)))
                       (and sym (symbol-name sym)))))
           (mode (and lang
                      (shr-tag-pre-highlight--get-lang-mode lang))))
      (shr-ensure-newline)
      (shr-ensure-newline)
      (setq start (point))
      (insert
       (propertize (concat "#+BEGIN_SRC " lang "\n") 'face 'org-block-begin-line)
       (or (and (fboundp mode)
                (with-demoted-errors "Error while fontifying: %S"
                  (shr-tag-pre-highlight-fontify code mode)))
           code)
       (propertize "#+END_SRC" 'face 'org-block-end-line ))
      (shr-ensure-newline)
      (setq end (point))
      (add-face-text-property start end '(:background "#292b2e" :extend t))
      (shr-ensure-newline)
      (insert "\n")))

  (add-to-list 'shr-tag-pre-highlight-lang-modes '(("R" . ess-r)))

)

  ;; (setq shr-tag-pre-highlight-lang-modes '(
  ;;                                          ("elisp" . emacs-lisp)
  ;;                                          ("ditaa" . artist)
  ;;                                          ("asymptote" . asy)
  ;;                                          ("dot" . fundamental)
  ;;                                          ("sqlite" . sql)
  ;;                                          ("calc" . fundamental)
  ;;                                          ("C" . c-ts)
  ;;                                          ("cpp" . c++-ts)
  ;;                                          ("C++" . c++-ts)
  ;;                                          ("screen" . shell-script)
  ;;                                          ("shell" . bash-ts)
  ;;                                          ("bash" . bash-ts)
  ;;                                          ("py" . python-ts)
  ;;                                          ("python" . python-ts)
  ;;                                          ("R" . ess-r)
  ;;                                          ("emacslisp" . emacs-lisp)
  ;;                                          ("el" . emacs-lisp))))

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
  :custom
  (shr-use-fonts  t)
  (shr-use-colors nil)
  (shr-indentation 4)
  (shr-bullet "• ")
  (shr-folding-mode t)
  (shr-max-width 120)
  (shr-max-image-proportion 0.4)
  (shr-width nil))

(use-package shrface
  :hook (eww-after-render devdocs-browser-eww-mode elfeed-show-mode nov-mode)
  :bind (:map shrface-mode-map
	          ("<tab>" . shrface-outline-cycle)
	          ("<backtab>" . shrface-outline-cycle-buffer)
	          ("M-n" . shr-next-link)
	          ("M-p" . shr-previous-link)
              ("C-S-k" . shrface-links-consult)
              ("C-S-j" . shrface-headline-consult)
	          ("C-j" . shrface-next-headline)
	          ("C-k" . shrface-previous-headline))
  :custom
  (shrface-item-bullet 8226)
  (shrface-bullets-bullet-list org-modern-star)
  (shrface-href-versatile t)
  :config
  (shrface-basic)
  (shrface-trial))

(use-package shr-color
  :defer t
  :custom
  (shr-color-visible-luminance-min 40 "Improve the contrast"))

(use-package eww
  :demand t
  :commands (eww eww-search-words)
  :hook (eww-mode . variable-pitch-mode)
  :bind ("M-s M-w" . eww-search-words)
  (:map eww-mode-map
        ("e" . readable-article)
        ("Q" . d/kill-buffer)
        ("RET" . eww-follow-link)
        ("m" . elfeed-toggle-star)
        ("b" . d/external-browser))
  :custom
  (eww-auto-rename-buffer 'title)
  (eww-search-prefix "https://duckduckgo.com/html/&q=")

  :config

  (defun eww-search-words ()
    "Search the web for the text in the region.
If region is active (and not whitespace), search the web for
the text between region beginning and end.  Else, prompt the
user for a search string.  See the variable `eww-search-prefix'
for the search engine used."
    (interactive)
    (if (use-region-p)
        (let ((region-string (buffer-substring (region-beginning) (region-end))))
          (if (not (string-match-p "\\`[ \n\t\r\v\f]*\\'" region-string))
              (eww-browse-url region-string t)
            (eww-browse-url (completing-read "Browse Url" eww-prompt-history))))
      (if (thing-at-point 'url)
          (eww-browse-url (completing-read "Browse Url" eww-prompt-history))
        (call-interactively #'eww)))))

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

(provide 'id-web)
;;; id-web.el ends here
