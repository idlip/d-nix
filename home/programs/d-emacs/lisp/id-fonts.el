;;; id-fonts.el --- fantastic font -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package faces
  :defines
  (d/on-droid
   d/variable-font-size
   d/fixed-pitch-font
   d/variable-pitch-font
   d/font-size)

  :preface
  (defvar d/font-size (if d/on-droid 150 140)
    "Default font size based on the system.")
  (defvar d/variable-font-size (if d/on-droid 160 150)
    "Default variable pitch size")

  ;; Dont worry about the font name, I use fork of Recursive font

  ;; Set reusable font name variables
  (defvar d/fixed-pitch-font "Code D OnePiece"
    "The font to use for monospaced (fixed width) text.")

  (defvar d/variable-pitch-font "Code D Ace"
    "The font to use for variable-pitch (documents) text.")

  :custom
  (face-font-family-alternatives
   '(("Monospace" "Code D OnePiece" "JetBrainsMono Nerd Font")
     ("Consolas" "JetBrainsMono Nerd Font" "Roboto Mono" "PT Mono" "Terminus" "Monospace")
     ("Monospace Serif" "CMU Typewriter Text" "Courier 10 Pitch" "Monospace")
     ("Serif" "Alegreya" "Noto Sans" "Georgia" "Cambria" "Times New Roman" "DejaVu Serif" "serif")))
  :custom-face
  (variable-pitch ((t (:family ,d/variable-pitch-font :height ,d/variable-font-size))))
  (fixed-pitch ((t (:family ,d/fixed-pitch-font :height ,d/font-size))))
  (default ((t (:family ,d/fixed-pitch-font :height ,d/font-size)))))

(use-package font-lock
  :defer t
  :custom ((font-lock-maximum-decoration t)
     (font-lock-global-modes '(not text-mode))
     (font-lock-verbose nil))
  :config
  (set-language-environment "UTF-8")
  (global-font-lock-mode 1))


(provide 'id-fonts)
;;; id-fonts.el ends here
