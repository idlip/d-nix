;;; early-init.el --- Emacs 27+ pre-initialisation config -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;; Tangled File, no need to edit !!!

;;; Code:

(custom-set-variables
 '(native-comp-async-report-warnings-errors nil)
 '(native-comp-jit-compilation nil))
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq frame-inhibit-implied-resize t)
(defconst d/on-droid (eq system-type 'android))
;; Emacs (gui app) is also amazing in android
;; https://sourceforge.net/projects/android-ports-for-gnu-emacs/files/termux/
(when d/on-droid
  (setenv "PATH" (format "%s:%s" "/data/data/com.termux/files/usr/bin"
		                 (getenv "PATH")))
  (setenv "LD_LIBRARY_PATH" (format "%s:%s"
				                    "/data/data/com.termux/files/usr/lib"
				                    (getenv "LD_LIBRARY_PATH")))
  (push "/data/data/com.termux/files/usr/bin" exec-path))


(provide 'early-init)
;;; early-init.el ends here
