;;; id-enhance.el --- GUI enhancements -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:


(use-package mwheel
  :custom
  (mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
  (mouse-wheel-progressive-speed nil)
  (scroll-margin 4)
  (scroll-conservatively 101))

(use-package pixel-scroll
  :commands
  (pixel-scroll-precision-scroll-down pixel-scroll-precision-scroll-up)
  :bind
  (("C-v" . d/scroll-down)
   ("M-v" . d/scroll-up))

  :config
  (defun d/scroll-down ()
    "Trust me, make scrolling alot smoother.
+1 Makes you fall in love with Emacs again!"
    (interactive)
    (pixel-scroll-precision-scroll-down 20))

  (defun d/scroll-up ()
    "Trust me, adds a wonderfull smooth scroll.
You can do this by trackpad too (laptop)"
    (interactive)
    (pixel-scroll-precision-scroll-up 20)))

(use-package tooltip
  :defer t
  :config
  (tooltip-mode -1))

(use-package time
  :defer t
  :hook
  (after-init . display-time)
  :custom
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  (display-time-format "%H:%M"))

(use-package battery
  :ensure nil
  :hook
  (after-init . display-battery-mode))

(use-package winner
  :ensure nil
  ;; :hook after-init
  :commands (winner-undo winnner-redo))

(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package rainbow-mode
  :defer t
  :hook '(prog-mode help-mode)
  :bind ("C-c t c" . rainbow-mode))

;; (use-package so-long
;;   :config (global-so-long-mode))

(use-package image-mode
  :ensure nil
  :defines (d/on-droid olivetti-body-width)
  :functions (olivetti-mode)
  :unless d/on-droid
  :bind (:map image-mode-map
              ("q" . d/kill-buffer))
  :hook
  (image-mode . (lambda () (olivetti-mode) (setq olivetti-body-width 0.45))))

(use-package select
  :custom
  (selection-coding-system 'utf-8)
  (x-select-request-type 'text/plain\;charset=utf-8)
  (select-enable-clipboard t "Use the clipboard"))

(use-package simple
  :bind
  ("M-c" . d/flex)
  ("M-l" . downcase-dwim)
  :functions (increment-number-at-point)

  :config

  ;; Stolen from the wiki somewhere
  (defun increment-number-at-point ()
    "Increment the number at point."
    (interactive)
    (skip-chars-backward "0-9")
    (or (looking-at "[0-9]+")
        (error "No number at point"))
    (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

  (defun d/flex ()
    "Perform smart flexing at point.

E.g. capitalize or decapitalize the next word, increment number at point."
    (interactive)
    (let ((case-fold-search nil))
      (call-interactively
       (cond ((looking-at "[0-9]+") #'increment-number-at-point)
             ((looking-at "[[:lower:]]") #'capitalize-word)
             ((looking-at "==") (delete-char 1) (insert "!") (forward-char 2))
             ((looking-at "!=") (delete-char 1) (insert "=") (forward-char 2))
             ((looking-at "+") (delete-char 1) (insert "-") (forward-char 1))
             ((looking-at "-") (delete-char 1) (insert "+") (forward-char 1))
             ((looking-at "<=") (delete-char 2) (insert ">=") (forward-char 2))
             ((looking-at ">=") (delete-char 2) (insert "<=") (forward-char 2))
             ((looking-at "<") (delete-char 1) (insert ">") (forward-char 1))
             ((looking-at ">") (delete-char 1) (insert "<") (forward-char 1))
             (t #'downcase-word))))))


;; Taken from gopar's config (via Yt video)
;; https://github.com/gopar/.emacs.d
(use-package type-break
  :ensure nil
  :disabled t
  :hook (after-init)
  :init
  (defun type-break-demo-agenda ()
    "Display the Org Agenda in read-only mode. Cease the demo as soon as a key is pressed."
    (let ((buffer-name "*Typing Break Org Agenda*")
          lines)
      (condition-case ()
          (progn
            (org-agenda-list)
            (setq buffer-name (buffer-name))
            ;; Set the buffer to read-only
            (with-current-buffer buffer-name
              (read-only-mode 1))
            ;; Message to be displayed at the bottom
            (let ((msg (if type-break-terse-messages
                           ""
                         "Press any key to resume from typing break")))
              ;; Loop until key is pressed
              (while (not (input-pending-p))
                (sit-for 60))
              ;; Clean up after key is pressed
              (read-event)
              (type-break-catch-up-event)
              (kill-buffer buffer-name)))
        (quit
         (and (get-buffer buffer-name)
              (kill-buffer buffer-name))))))

  :custom
  ;; Setting interval of that of a pomodoro session
  (type-break-interval (* 25 60)) ;; 25 mins
  (type-break-good-rest-interval (* 9 60)) ;; 9 mins
  (type-break-good-break-interval (* 5 60)) ;; 5 mins
  (type-break-query-mode t)
  (type-break-keystroke-threshold '(nil . 2625))
  (type-break-demo-boring-stats t)
  (type-break-demo-functions '(type-break-demo-agenda)))


(provide 'id-enhance)
;;; id-enhance.el ends here
