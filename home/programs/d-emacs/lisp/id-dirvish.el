;;; id-dirvish.el --- file manager on battery -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:


;; Battery pack
  (use-package dirvish
    :init
    (dirvish-override-dired-mode)
    :custom
    (dirvish-quick-access-entries
     '(("h" "~/"                          "Home")
       ("d" "~/dloads/"                "Downloads")
       ;; ("m" "/mnt/"                       "Drives")
       ("t" "~/.local/share/Trash/files/" "TrashCan")))

    ;; (dirvish-peek-mode) ; Preview files in minibuffer
    ;; (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
    (dirvish-mode-line-format
     '(:left (sort symlink) :right (omit yank index)))
    (dirvish-attributes
     '(file-time file-size collapse subtree-state vc-state git-msg))
    (delete-by-moving-to-trash t)
    (dired-listing-switches
     "-l --almost-all --human-readable --group-directories-first --no-group")
    (dirvish-hide-cursor nil)

    ;; with emacs29
    (dired-mouse-drag-files t)
    (mouse-drag-and-drop-region-cross-program t)
    (mouse-1-click-follows-link nil)

    :bind
    (("C-c f d" . dirvish-fd)
     ("C-x C-d" . dirvish)
     ("C-c f t" . dirvish-side)
     :map dirvish-mode-map
     ("<mouse-1>" . 'dirvish-subtree-toggle-or-open)
     ("<mouse-2>" . 'dired-mouse-find-file-other-window)
     ("<mouse-3>" . 'dired-mouse-find-file)
     ("a"   . dirvish-quick-access)
     ("f"   . dirvish-file-info-menu)
     ("y"   . dirvish-yank-menu)
     ("N"   . dirvish-narrow)
     ("^"   . dirvish-history-last)
     ("h"   . dirvish-history-jump) ; remapped `describe-mode'
     ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
     ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
     ("TAB" . dirvish-subtree-toggle)
     ("M-f" . dirvish-history-go-forward)
     ("M-b" . dirvish-history-go-backward)
     ("M-l" . dirvish-ls-switches-menu)
     ("M-m" . dirvish-mark-menu)
     ("M-t" . dirvish-layout-toggle)
     ("M-e" . dirvish-emerge-menu)
     ("M-j" . dirvish-fd-jump)))

(provide 'id-dirvish)
;;; id-dirvish.el ends here
