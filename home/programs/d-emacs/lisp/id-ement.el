;;; id-ement.el --- matrix -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package ement
  :bind
  (:map ement-room-minibuffer-map
        ("<f6>" . ement-room-compose-from-minibuffer))
  (:map ement-room-mode-map
        ("M-<" . ement-room-scroll-down-command))
  :custom
  (ement-room-send-message-filter 'ement-room-send-org-filter)
  (ement-room-message-format-spec "%S> %L%B%r%R%t")
  (ement-room-list-avatars nil)
  (ement-save-sessions t)
  :config
  ;; copied from viru (ement github)
  (defun d/ement-connect ()
    (interactive)
    (if (ement--read-sessions)
        (call-interactively #'ement-connect)
      (let* ((found (auth-source-search :max 1
                                        :host "matrix.org"
                                        :port "8448"
                                        :require '(:user :secret)))
             (entry (nth 0 found))
             (password (funcall (plist-get entry :secret)))
             (user (plist-get entry :user)))
        (ement-connect :user-id user :password password)))))

(provide 'id-ement)
;;; id-ement.el ends here
