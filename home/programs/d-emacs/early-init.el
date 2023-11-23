;;; early-init.el --- Emacs 27+ pre-initialisation config

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

(setq package-enable-at-startup nil
      load-prefer-newer t)

(custom-set-variables
 '(native-comp-async-report-warnings-errors nil)
 '(native-comp-jit-compilation nil))

(defconst d/on-droid (eq system-type 'android))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)


;; Emacs (gui app) is also amazing in android
;; https://sourceforge.net/projects/android-ports-for-gnu-emacs/files/termux/
(when d/on-droid
  (setenv "PATH" (format "%s:%s" "/data/data/com.termux/files/usr/bin"
		                 (getenv "PATH")))
  (setenv "LD_LIBRARY_PATH" (format "%s:%s"
				                    "/data/data/com.termux/files/usr/lib"
				                    (getenv "LD_LIBRARY_PATH")))
  (push "/data/data/com.termux/files/usr/bin" exec-path))


;; So we can detect this having been loaded
(provide 'early-init)

;;; early-init.el ends here
