
(defun d-emacs-re-enable-frame-theme (_frame)
  "Re-enable active theme, if any, upon FRAME creation.
 Add this to `after-make-frame-functions' so that new frames do
 not retain the generic background set by the function
 `d-emacs-avoid-initial-flash-of-light'."
  (when-let ((theme (car custom-enabled-themes)))
    (enable-theme theme)))

(defun d-emacs-avoid-initial-flash-of-light ()
  "Avoid flash of light when starting Emacs, if needed.
 New frames are instructed to call `d-emacs-re-enable-frame-theme'."
  ;; (when (d-emacs-theme-environment-dark-p)
  (setq mode-line-format nil)
  (set-face-attribute 'default nil :background "#000000" :foreground "#ffffff")
  (set-face-attribute 'mode-line nil :background "#000000" :foreground "#ffffff" :box 'unspecified)
  (add-hook 'after-make-frame-functions #'d-emacs-re-enable-frame-theme))

(d-emacs-avoid-initial-flash-of-light)

(add-hook 'after-init-hook (lambda () (set-frame-name "home")))

 ;;; early-init.el ends here
