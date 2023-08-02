;;; id-android.el --- android also? -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; access phone storage as default
;; Better is to symlink file to ~/ itself

;;(setq default-directory "/storage/emulated/0/")

(customize-set-variable 'touch-screen-precision-scroll t)

(customize-set-variable 'touch-screen-display-keyboard t)

(customize-set-variable 'browse-url-android-share t)

;; (setq use-dialog-box nil)

(provide 'id-android)
;;; id-android.el ends here
