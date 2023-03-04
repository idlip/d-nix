(setq package-enable-at-startup nil ; don't auto-initialize!
      ;; don't add that `custom-set-variables' block to my init.el!
      package--init-file-ensured t)
(setq package-archives nil)


;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216 ; 16mb
                  gc-cons-percentage 0.1)))

(defun config:defer-gc ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun config:-do-restore-gc ()
  (setq gc-cons-threshold 16777216))
(defun config:restore-gc ()
  (run-at-time 1 nil #'config:-do-restore-gc))

(add-hook 'minibuffer-setup #'config:defer-gc)
(add-hook 'minibuffer-exit #'config:restore-gc)

(setq safe-local-variable-values
      '((org-src-preserve-indentation . t)
        (eval add-hook 'after-save-hook
              '(lambda nil
                 (org-babel-tangle))
              nil t)))

(setq
 load-prefer-newer nil
 default-input-method nil
 initial-major-mode 'org-mode
 inhibit-default-init t
 inhibit-startup-screen t 			 ; Do not show the startup message.
 inhibit-startup-buffer-menu t       ; stop `list-buffers' from showing when opening multiple files.
 my-computer-has-smaller-memory-p t) ; computers with smaller memory. Not sure if it works or not!


;; Maximize the Emacs frame on startup
(push '(fullscreen . maximized) default-frame-alist)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Suppress warnings and errors during asynchronous native compilation
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-deferred-compilation t)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Prevent unwanted runtime builds; packages are compiled ahead-of-time when
;; they are installed and site files are compiled when gccemacs is installed.
;; (setq comp-deferred-compilation nil)
(setq native-comp-deferred-compilation nil)

;; Prevent unwanted runtime builds in gccemacs (native-comp); packages are
;; compiled ahead-of-time when they are installed and site files are compiled
;; when gccemacs is installed.
(setq comp-deferred-compilation nil)

(provide 'early-init)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; early-init.el ends here
