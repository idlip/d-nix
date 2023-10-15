;;; id-icons.el --- iconify -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:


(use-package nerd-icons
  :custom
  (nerd-icons-font-family d/fixed-pitch-font))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package kind-icon
  :after corfu
  :defines (corfu-margin-formatters)
  :functions (kind-icon-margin-formatter)
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  (kind-icon-default-style '(:padding -0.5 :stroke 0 :margin 0 :radius 0 :height 0.6 :scale 1.0))
  (kind-icon-use-icons nil)
  (kind-icon-mapping
   `(
     (array ,(nerd-icons-codicon "nf-cod-symbol_array") :face font-lock-type-face)
     (boolean ,(nerd-icons-codicon "nf-cod-symbol_boolean") :face font-lock-builtin-face)
     (class ,(nerd-icons-codicon "nf-cod-symbol_class") :face font-lock-type-face)
     (color ,(nerd-icons-codicon "nf-cod-symbol_color") :face success)
     (command ,(nerd-icons-codicon "nf-cod-terminal") :face default)
     (constant ,(nerd-icons-codicon "nf-cod-symbol_constant") :face font-lock-constant-face)
     (constructor ,(nerd-icons-codicon "nf-cod-triangle_right") :face font-lock-function-name-face)
     (enummember ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
     (enum-member ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
     (enum ,(nerd-icons-codicon "nf-cod-symbol_enum") :face font-lock-builtin-face)
     (event ,(nerd-icons-codicon "nf-cod-symbol_event") :face font-lock-warning-face)
     (field ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-variable-name-face)
     (file ,(nerd-icons-codicon "nf-cod-symbol_file") :face font-lock-string-face)
     (folder ,(nerd-icons-codicon "nf-cod-folder") :face font-lock-doc-face)
     (interface ,(nerd-icons-codicon "nf-cod-symbol_interface") :face font-lock-type-face)
     (keyword ,(nerd-icons-codicon "nf-cod-symbol_keyword") :face font-lock-keyword-face)
     (macro ,(nerd-icons-codicon "nf-cod-symbol_misc") :face font-lock-keyword-face)
     (magic ,(nerd-icons-codicon "nf-cod-wand") :face font-lock-builtin-face)
     (method ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
     (function ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
     (module ,(nerd-icons-codicon "nf-cod-file_submodule") :face font-lock-preprocessor-face)
     (numeric ,(nerd-icons-codicon "nf-cod-symbol_numeric") :face font-lock-builtin-face)
     (operator ,(nerd-icons-codicon "nf-cod-symbol_operator") :face font-lock-comment-delimiter-face)
     (param ,(nerd-icons-codicon "nf-cod-symbol_parameter") :face default)
     (property ,(nerd-icons-codicon "nf-cod-symbol_property") :face font-lock-variable-name-face)
     (reference ,(nerd-icons-codicon "nf-cod-references") :face font-lock-variable-name-face)
     (snippet ,(nerd-icons-codicon "nf-cod-symbol_snippet") :face font-lock-string-face)
     (string ,(nerd-icons-codicon "nf-cod-symbol_string") :face font-lock-string-face)
     (struct ,(nerd-icons-codicon "nf-cod-symbol_structure") :face font-lock-variable-name-face)
     (text ,(nerd-icons-codicon "nf-cod-text_size") :face font-lock-doc-face)
     (typeparameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
     (type-parameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
     (unit ,(nerd-icons-codicon "nf-cod-symbol_ruler") :face font-lock-constant-face)
     (value ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-builtin-face)
     (variable ,(nerd-icons-codicon "nf-cod-symbol_variable") :face font-lock-variable-name-face)
     (t ,(nerd-icons-codicon "nf-cod-code") :face font-lock-warning-face)))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(provide 'id-icons)
;;; id-icons.el ends here
