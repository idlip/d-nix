;;; id-org.el --- Organize life like never before -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package org
  :defer t
  :commands (org-capture org-agenda)
  :hook (org-mode . (lambda () (org-indent-mode 1)
                      (org-display-inline-images 0)
                      (variable-pitch-mode 1)))

  :bind (("C-c c c" . org-capture)
         ("C-c c d" . calendar)
         ("C-c t R" . d/bionic-region)
         ("C-c d a" . org-agenda)
         ("C-c t r" . d/bionic-read))

  :custom
  (org-src-window-setup 'current-window)
  (org-startup-indented nil)
  (org-image-actual-width 400)
  (org-startup-folded t)
  (org-ellipsis " ▾")
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-done 'note)
  (org-log-into-drawer t)

  (org-agenda-files
   '("~/d-sync/notes/tasks.org"
     "~/d-git/d-site/README.org"))

  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
     (sequence  "PLAN(p)" "REVIEW(v)" "|" "COMPLETED(c)" "CANC(k@)")))

  (org-refile-targets
   '(("Archive.org" :maxlevel . 1)
     ("tasks.org" :maxlevel . 1)))

  (org-tag-alist
   '((:startgroup)
     (:endgroup)
     ("@work" . ?W)
     ("agenda" . ?a)
     ("linux" . ?l)
     ("planning" . ?p)
     ("note" . ?n)
     ("idea" . ?i)))


  (org-capture-templates
   `(
     ("t" "Task" entry (file+olp "~/d-sync/notes/tasks.org" "One-Timer")
      "* TODO %?\n  SCHEDULED:%U\n  %a\n  %i" :empty-lines 1)

     ("l" "Link" entry
      (file+headline "~/d-sync/notes/bookmarks.org" "elfeed") "* %a\n")

     ("j" "Journal Entries")

     ("jj" "Journal" entry
      (file+olp+datetree "~/d-sync/notes/journal.org")
      "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
      ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
      :clock-in :clock-resume
      :empty-lines 1)))

  :config
  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (defun org-archive-done-tasks ()
    "From the org-heading, it throws all the Done tasks to filename_archive.org"
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
     "/DONE" 'tree))

  ;; This is for managing literate nixos config in org file
  (defun get-named-src-block-contents (name &optional trim)
    "Return the contents of the named Org source block."
    (let* ((block (org-element-map (org-element-parse-buffer) 'src-block
                    (lambda (src-block)
                      (when (string= name (org-element-property :name src-block))
                        src-block))
                    nil t))
           (source (org-element-property :value block)))
      (if trim
          (string-trim source)
        source)))

  (defun d/narrow-or-widen-dwim ()
    "If the buffer is narrowed, it widens. Otherwise, it narrows to region, or Org subtree."
    (interactive)
    (cond ((buffer-narrowed-p) (widen))
          ((region-active-p) (narrow-to-region (region-beginning) (region-end)))
          ((equal major-mode 'org-mode) (org-narrow-to-subtree))
          (t (error "Please select a region to narrow to"))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (calc . t)
     (latex . t) (C . t)
     (R . t) (shell . t) (python . t)))
  (push '("conf-unix" . conf-unix) org-src-lang-modes)
  )

(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  (org-agenda-finalize-hook . org-modern-agenda)

  :custom
  ;; Edit settings
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)

  ;; Org styling, hide markup etc.
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  ;;   org-ellipsis "…"

  ;; Reference:
  ;; Heading: "◉ ○ ✸ ✿"
  ;; Cool-Heading: ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
  ;; Small: ► • ★ ▸
  ;; others: ▼, ↴, ⬎, ⤷,…, and ⋱.
  ;; (org-ellipsis "⤵")
  ;; nerd-icons: "" "󰓏" "󰚀" "󰴈" "" "󰄄"

  ;; (org-modern-star '("◉" "✪" "◈" "✿" "❂"))
  (org-modern-star '("" "󰓏" "󰚀" "󰴈" "" "󰄄"))
  (org-modern-hide-stars 'leading)
  (org-modern-table nil)
  (org-modern-list
   '((?* . "⁍")
     (?- . "❖")
     (?+ . "➤")))

  ;; Agenda styling
  (org-agenda-tags-column 0)
  (org-agenda-block-separator ?─)
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")

  :config
  ;; Add frame borders and window dividers
  (modify-all-frames-parameters
   '((right-divider-width . 1)
     (bottom-divider-width . 0)
     (internal-border-width . 5)))
  (dolist (face '(window-divider
		  window-divider-first-pixel
		  window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
  (global-org-modern-mode))

(provide 'id-org)
;;; id-org.el ends here
