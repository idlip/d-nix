;; (("d-setup.org" . ((org-mode . ((eval . (add-hook 'after-save-hook (lambda () (let ((inhibit-redisplay t) (inhibit-message t) (emacs-lisp-mode-hook '())) (org-babel-tangle))) nil t)))))))

((nil
  . ((eglot-workspace-configuration
      . (:nixd (:nixpkgs (:expr "import <nixpkgs> { }"))
               (:options (:nixos (:exp "(builtins.getFlake \"/home/idlip/d-git/d-nix\").nixosConfigurations.gdk.options"))
                         (:home-manager (:exp "(builtins.getFlake \"/home/idlip/d-git/d-nix\").homeConfigurations.\"idlip@gdk\".options"))
                         ;; (:home-manager (:exp "(builtins.getFlake \"/home/idlip/d-git/d-nix\").nixosConfigurations.gdk.options.home-manager.users.type.getSubOptions []"))
                         ))))))
