;;; id-android.el --- android also? -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; access phone storage as default
;; Better is to symlink file to ~/ itself

;;(setq default-directory "/storage/emulated/0/")

(custom-set-variables
 '(touch-screen-precision-scroll t)
 '(touch-screen-display-keyboard t)
 '(browse-url-android-share t))

(defun d/key-droid()
  "To enable touch screen keyboard"
  (interactive)
  (frame-toggle-on-screen-keyboard (selected-frame) nil)
  )

;; (setq use-dialog-box nil)

(provide 'id-android)
;;; id-android.el ends here
