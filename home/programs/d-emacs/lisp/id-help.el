;;; id-help.el --- help with docs -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package man
  :ensure nil
  :defer t
  :custom
  (Man-notify-method 'pushy "show manpage HERE")
  :custom-face
  (Man-overstrike ((t (:inherit font-lock-type-face :bold t))))
  (Man-underline ((t (:inherit font-lock-keyword-face :underline t))))
  :bind (("C-c m" . consult-man)
   :map Man-mode-map
   ("q" . kill-buffer-and-window)))

;; get me linter first
(use-package flycheck
  :defer t
;; :init (global-flycheck-mode))
  :hook (prog-mode . flycheck-mode))

(use-package woman
  :ensure nil
  :defer t
  :custom-face
  (woman-bold ((t (:inherit font-lock-type-face :bold t))))
  (woman-italic ((t (:inherit font-lock-keyword-face :underline t)))))

(use-package which-key
  :disabled t ;; trying embark-help function
  :defer 2
  :defines (d/on-droid)
  :functions
  (which-key-mode
   which-key-abort
   which-key--create-buffer-and-show
   repeated-prefix-help-command)
  :unless d/on-droid
  :custom
  (which-key-show-transient-maps t)
  (which-key-side-window-location 'bottom)
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 6)
  (which-key-side-window-slot -10)
  (which-key-side-window-max-height 0.25)
  (which-key-idle-delay 0.8)
  (which-key-max-description-length 25)
  (which-key-allow-imprecise-window-fit t)
  (which-key-separator " → " )
  :diminish which-key-mode
  :config
  (which-key-mode)
  ;; From kathink. It repeats the seq without modifier
  (defun repeated-prefix-help-command ()
    "Repeat keychords in sequence without modifier."
    (interactive)
    (when-let* ((keys (this-command-keys-vector))
	            (prefix (seq-take keys (1- (length keys))))
	            (orig-keymap (key-binding prefix 'accept-default))
	            (keymap (copy-keymap orig-keymap))
	            (exit-func (set-transient-map keymap t #'which-key-abort)))
      (define-key keymap [remap keyboard-quit]
		          (lambda () (interactive) (funcall exit-func)))
      (which-key--create-buffer-and-show nil keymap)))

  (setq prefix-help-command #'repeated-prefix-help-command)
  )

(use-package helpful
  :defines (helpful-mode-map)
  :hook (helpful-mode . toggle-mode-line)
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h x" . helpful-command)
  ("C-c C-d" . helpful-at-point)
  ("C-h o" . helpful-symbol)
  ("C-h F" . helpful-function)
  (:map helpful-mode-map
	("q" . kill-buffer-and-window)))


(provide 'id-help)
;;; id-help.el ends here
