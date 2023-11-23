;;; id-org.el --- Organize life like never before -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package org
  :ensure nil
  :defer t
  :commands
  (org-capture
   org-agenda
   org-indent-mode
   org-display-inline-images
   org-map-entries
   org-archive-subtree
   org-element-property
   org-element-at-point
   org-element-map
   org-element-parse-buffer
   )
  :hook (org-mode . (lambda () (org-indent-mode 1)
                      (org-display-inline-images 0)
                      (variable-pitch-mode 1)))

  :bind
  ("C-c c d" . calendar)
  ("C-c t R" . d/bionic-region)
  ("C-c t i" . d/set-timer)
  ("C-c t r" . d/bionic-read)
  (:map org-mode-map
        ("C-x n n" . d/narrow-or-widen-dwim))

  :custom
  (org-startup-indented nil)
  (org-image-actual-width 400)
  (org-startup-folded t)
  (org-ellipsis " ⮟")
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-done 'note)
  (org-log-into-drawer t)

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

  (fill-column 80)
  ;; Where the org files live
  (org-directory "~/d-sync/notes")
  ;; Make sure we see syntax highlighting
  (org-src-fontify-natively t)
  ;; I dont use it for subs/super scripts
  (org-use-sub-superscripts nil)
  ;; Should everything be hidden?
  (org-startup-folded 'content)
  (org-M-RET-may-split-line '((default . nil)))
  ;; hide stars except for leader star
  (org-hide-leading-stars t)
  (org-hide-emphasis-markers nil)
  ;; Show as utf-8 chars
  (org-pretty-entities t)
  ;; put timestamp when finished a todo
  (org-log-done 'time)
  ;; timestamp when we reschedule
  (org-log-reschedule t)
  ;; Don't indent the stars
  (org-startup-indented nil)
  (org-list-allow-alphabetical t)
  (org-image-actual-width nil)
  ;; Save notes into log drawer
  (org-log-into-drawer t)
  ;;
  (org-fontify-whole-heading-line t)
  (org-fontify-done-headline t)
  ;;
  (org-fontify-quote-and-verse-blocks t)
  ;; See down arrow instead of "..." when we have subtrees
  ;; (org-ellipsis "⤵")
  ;; catch invisible edit
  ( org-catch-invisible-edits 'show-and-error)
  ;; Only useful for property searching only but can slow down search
  (org-use-property-inheritance t)
  ;; Count all children TODO's not just direct ones
  (org-hierarchical-todo-statistics nil)
  ;; Unchecked boxes will block switching the parent to DONE
  (org-enforce-todo-checkbox-dependencies t)
  ;; Don't allow TODO's to close without their dependencies done
  (org-enforce-todo-dependencies t)
  (org-track-ordered-property-with-tag t)
  ;; Where should notes go to? Dont even use them tho
  (org-default-notes-file (concat org-directory "notes.org"))

  ;; Needed to allow helm to compute all refile options in buffer
  (org-outline-path-complete-in-steps nil)
  (org-deadline-warning-days 2)
  (org-log-redeadline t)
  (org-log-reschedule t)
  ;; Repeat to previous todo state
  ;; If there was no todo state, then dont set a state
  (org-todo-repeat-to-state t)
  ;; Refile options
  (org-refile-use-outline-path 'file)
  (org-refile-allow-creating-parent-nodes 'confirm)
  ;; This worked ok, but lets try some more detail refiling
  ;; (org-refile-targets '((org-agenda-files :level .  1)))
  ;; Lets customize which modules we load up
  (org-modules '(ol-w3m
                 ol-bbdb
                 ol-bibtex
                 ol-docview
                 ol-gnus
                 ol-info
                 ol-irc
                 ol-mhe
                 ol-rmail
                 ol-eww
                 ;; Stuff I've enabled below
                 org-habit
                 ;; org-checklist
                 ))
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)

  :config
  (org-clock-persistence-insinuate)

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


  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (calc . t)
     (latex . t) (C . t)
     (R . t)
     (shell . t) (python . t)
     (julia . t)))
  (push '("conf-unix" . conf-unix) org-src-lang-modes)
  (defalias 'd/set-timer (symbol-function 'org-timer-set-timer))
  )

(use-package org-tempo
  :ensure nil
  :after org
  :config
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("p" . "src python"))
  (add-to-list 'org-structure-template-alist '("sh" . "src sh")))

(use-package org-modern
  :defer 1

  :commands
  (shrface-mode
   global-org-modern-mode)
  :hook (org-mode org-agenda-finalize-hook)

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
  ;; (org-modern-star '("" "󰓏" "󰚀" "󰴈" "" "󰄄"))
  (org-modern-star '("󰓏" "󰚀" "󰫤"  "󰴈" "" "󰄄"))
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

;; from gopar's config
(use-package org-clock
  :ensure nil
  :after org
  :commands
  (org-clock-jump-to-current-clock
   org-clock-report)
  :custom
  ;; Save clock history accross emacs sessions (read var for required info)
  (org-clock-persist t)
  ;; If idle for more than 15 mins, resolve by asking what to do with clock
  (org-clock-idle-time 15)
  ;; Set clock in frame title, instead of mode line
  (org-clock-clocked-in-display 'frame-title)
  ;; Show more clocking history
  (org-clock-history-length 10)
  ;; Include running time in clock reports
  (org-clock-report-include-clocking-task t)
  ;; Put all clocking info int the "CLOCKING" drawer
  (org-clock-into-drawer "CLOCKING")
  ;; Setup default clocktable summary
  (org-clock-clocktable-default-properties
   '(:maxlevel 2 :scope file :formula % ;; :properties ("Effort" "Points")
               :sort (5 . ?t) :compact t :block today))
  :bind (:map global-map
              ("C-c j" . (lambda () (interactive) (org-clock-jump-to-current-clock)))
              :map org-mode-map
              ("C-c C-x r" . (lambda () (interactive) (org-clock-report)))))

(use-package org-agenda
  :ensure nil
  :after org
  :bind
  ("C-c d a" . org-agenda)
  (:map org-agenda-mode-map
        ("C-x C-k" . org-agenda-exit))

  ;; :hook (org-agenda-finalize . org-agenda-entry-text-mode)
  :custom
  (org-agenda-tags-column 'auto)
  (org-agenda-window-setup 'only-window)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-log-mode-items '(closed clock state))
  ;; (org-agenda-todo-ignore-scheduled 'future)
  ;; TODO entries that can't be marked as done b/c of children are shown as dimmed in agenda view
  (org-agenda-dim-blocked-tasks nil)
  (org-agenda-inhibit-startup t)
  (org-agenda-show-inherited-tags nil)
  (org-agenda-ignore-drawer-properties '(effort appt category))
  ;; Start the week view on whatever day im on
  (org-agenda-start-on-weekday nil)
  (org-agenda-files
   '("~/d-sync/notes/"
     "~/d-git/d-site/README.org"
     "~/d-sync/notes/bioinfo/")))


(use-package org-capture
  :ensure nil
  :after org
  :defines
  (my-org-agenda-headlines)

  :bind
  ("C-c c c" . org-capture)

  :custom
  ;; dont create a bookmark when calling org-capture
  (org-capture-bookmark nil)
  ;; also don't create bookmark in other things
  (org-bookmark-names-plist nil)
  (org-capture-templates
   `(
     ("t" "Task" entry (file+function "~/d-sync/notes/tasks.org" (lambda () (completing-read "Heading: " my-org-agenda-headlines)))
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
  (setq my-org-agenda-headlines `(projects university tasks one-timer)))

(use-package ol
  :ensure nil
  :after org
  :custom
  (org-link-shell-confirm-function 'y-or-n-p)
  (org-link-elisp-confirm-function 'y-or-n-p))

(use-package org-src
  :ensure nil
  :after org
  :custom
  (org-src-preserve-indentation nil)
  ;; Don't ask if we already have an open Edit buffer
  (org-src-window-setup 'current-window)
  (org-src-ask-before-returning-to-edit-buffer nil)
  (org-edit-src-content-indentation 0)
  :config
  (advice-add 'org-src-get-lang-mode :filter-return
              (lambda (mode)
                (pcase (assoc mode major-mode-remap-alist)
                  (`(,mode . ,ts-mode) ts-mode)
                  (_ mode)))))

(use-package ob-core
  :ensure nil
  :after org
  :custom
  ;; Don't ask every time when I run a code block
  (org-confirm-babel-evaluate nil))

(use-package org-habit
  :ensure nil
  :ensure nil
  :custom
  (org-habit-graph-column 45))

(use-package org-indent
  :ensure nil
  :diminish)


(provide 'id-org)
;;; id-org.el ends here
