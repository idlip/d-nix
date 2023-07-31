;;; id-python.el --- python -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:



;; taken from Robb Enzmann
(defun d/pyrightconfig-write (virtualenv)
  "Write a `pyrightconfig.json' file at the Git root of a project,
with `venvPath' and `venv' set to the absolute path of
`virtualenv'.  When run interactively, prompts for a directory to select."
  (interactive "DEnv: ")
  ;; Naming convention for venvPath matches the field for pyrightconfig.json
  (let* ((venv-dir (tramp-file-local-name (file-truename virtualenv)))
	 (venv-file-name (directory-file-name venv-dir))
	 (venvPath (file-name-directory venv-file-name))
	 (venv (file-name-base venv-file-name))
	 (base-dir (vc-git-root default-directory))
	 (out-file (expand-file-name "pyrightconfig.json" base-dir))
	 (out-contents (json-encode (list :venvPath venvPath :venv venv))))
    (with-temp-file out-file (insert out-contents))
    (message (concat "Configured `" out-file "` to use environment `" venv-dir))))




(provide 'id-python.el)
;;; id-python.el ends here
