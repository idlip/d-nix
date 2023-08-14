;;; early-init.el --- Emacs 27+ pre-initialisation config

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

(setq package-enable-at-startup nil
      native-comp-async-report-warnings-errors nil
      load-prefer-newer t)

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


;; So we can detect this having been loaded
(provide 'early-init)

;;; early-init.el ends here
