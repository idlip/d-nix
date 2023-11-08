;;; id-embark.el --- OP than you imagine -*- lexical-binding: t -*-
;;; Commentary:

;; This package alone is so powerful that it is overlooked,
;; Refer: https://karthinks.com/software/fifteen-ways-to-use-embark
;; goto: info:embark
;; It is now one of my fav package

;;; Code:

(use-package embark
  :defer t

  :functions
  (embark-prefix-help-command
   embark-eldoc-first-target)

  :bind
  (("C-." . embark-act)
   ("C-;" . embark-act-all)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings)
   (:map embark-identifier-map
         ("d" . sdcv-search-input)
         ("ch" . color-name-to-hex)
         ("cr" . color-name-to-rgb))
   (:map embark-url-map
         ("b" . browse-url-generic)
         ("r" . reddigg-view-comments))
   (:map embark-file-map
         ("b" . browse-url-of-dired-file))
   (:map embark-region-map
         ("U" . webpaste-paste-buffer-or-region)))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(with-eval-after-load 'embark
  (defun sudo-find-file (file)
    "Open FILE as root."
    (interactive "FOpen file as root: ")
    (when (file-writable-p file)
      (find-file file)
      (user-error "File is user writeable, opening as user"))
    (find-file (if (file-remote-p file)
                   (concat "/" (file-remote-p file 'method) ":"
                           (file-remote-p file 'user) "@" (file-remote-p file 'host)
                           "|doas:root@"
                           (file-remote-p file 'host) ":" (file-remote-p file 'localname))
                 (concat "/doas:root@localhost:" file))))
  (define-key embark-file-map (kbd "S") 'sudo-find-file))

(use-package embark-consult
  :defer t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(defun color-name-to-hex (NAME)
  "Return hexadecimal value of color with NAME.
Return nil if NAME does not designate a valid color."
  (insert
   (when-let* ((rgb (color-name-to-rgb NAME))
               ;; Sets 2 digits per component.
               (hex (apply #'color-rgb-to-hex (append rgb '(2)))))
     hex)))


(provide 'id-embark)
;;; id-embark.el ends here
