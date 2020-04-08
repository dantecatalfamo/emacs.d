;;; init-file.el --- Init file related functions
;;; Commentary:
;;; Code:

(defun edit-init-file ()
  "Open the user's init file."
  (interactive)
  (find-file user-init-file))

(defun reload-init-file ()
  "Reload the user's init file."
  (interactive)
  (load-file user-init-file))

(provide 'init-file)
;;; init-file.el ends here
