((org-mode . ((eval . (add-hook 'after-save-hook (lambda () (let ((inhibit-redisplay t) (inhibit-message t) (emacs-lisp-mode-hook '())) (org-babel-tangle))) nil t)))))
