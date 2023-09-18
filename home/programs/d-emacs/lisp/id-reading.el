;;; id-reading.el --- now study -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package nov
  :hook (nov-mode . hide-mode-line-mode)
  (nov-mode . variable-pitch-mode)
  :mode ("\\.epub\\'" . nov-mode)
  :custom
  (nov-text-width nil)
  (nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
  (nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions))
  (nov-variable-pitch t))

(use-package doc-view
  :ensure nil
  ;; :mode ("\\.epub\\'" . doc-view-mode)
  :bind (:map doc-view-mode-map
	            ("M-g M-g" . doc-view-goto-page)
	            ("<f8>" . doc-view-presentation))
  :custom
  (doc-view-continuous t)
  (doc-view-mupdf-use-svg t)
  (doc-view-image-width 900)
  (large-file-warning-threshold 700000000)
  (image-cache-eviction-delay 3))

;; Read normal text files as emacs info manuals
;; thanks to:
;;https://emacsnotes.wordpress.com/2023/09/11/view-info-texi-org-and-md-files-as-info-manual/
(defun d/text-info ()
  "View ‘info’, ‘texi’, ‘org’, ‘md’ and 'NEWS' files as ‘Info’ manual."
  (interactive)
  (require 'rx)
  (require 'ox-texinfo)
  (when (buffer-file-name)
    (let* ((org-export-with-broken-links 'mark)
           (ext (file-name-extension (buffer-file-name))))
      (cond
       ;; A NEWS files
       ((string-match "NEWS" (file-name-nondirectory (buffer-file-name)))
        (with-current-buffer
            ;; NEWS files are likely to be in read-only directories.
            ;; So make a copy with an `.org' extension.  Most NEWS
            ;; file are `outline-mode' files with `org' like heading
            ;; structure.  Many of the recent files like ORG-NEWS are
            ;; proper `org' files.
            (find-file-noselect
             (make-temp-file
              (format "%s---" (file-name-nondirectory (buffer-file-name))) nil ".org"
              (buffer-substring-no-properties (point-min) (point-max))))
          (org-with-wide-buffer
           ;; `ox-texinfo' export fails if a headline ends with a
           ;; period (= ".").  So, strip those terminating periods.
           (goto-char (point-min))
           (while (re-search-forward (rx (and bol
                                              (one-or-more "*")
                                              " "
                                              (one-or-more any)
                                              (group ".")
                                              eol))
                                     (point-max) t)
             (replace-match "" t t nil 1))
           (goto-char (point-min))
           (while nil
             ;; TODO: If a NEWS file contains text which resemble a
             ;; LaTeX fragment, the `ox-texinfo' export wouldn't
             ;; succeed.  So, enclose the LaTeX fragment with Org's
             ;; verbatim `=' marker.
             )
           (save-buffer 0)
           (info (org-texinfo-export-to-info)))))
       ;; A `.info' file
       ((or (string= "info" ext))
        (info (buffer-file-name)))
       ;; A `.texi' file
       ((or (string= "texi" ext))
        (info (org-texinfo-compile (buffer-file-name))))
       ;; An `.org' file
       ((or (derived-mode-p 'org-mode)
            (string= "org" ext))
        (info (org-texinfo-export-to-info)))
       ;; A `.md' file
       ((or (derived-mode-p 'markdown-mode)
            (string= "md" ext))
        (let ((org-file-name (concat (file-name-sans-extension (buffer-file-name)) ".org")))
          (apply #'call-process "pandoc" nil standard-output nil
                 `("-f" "markdown"
                   "-t" "org"
                   "-o" ,org-file-name
                   ,(buffer-file-name)))
          (with-current-buffer (find-file-noselect org-file-name)
            (info (org-texinfo-export-to-info)))))
       (t (user-error "Don't know how to convert `%s' to an `info' file"
                      (buffer-file-name)))))))

(global-set-key (kbd "C-x x v") 'd/text-info)


(provide 'id-reading)
;;; id-reading.el ends here
