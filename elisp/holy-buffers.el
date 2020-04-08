;;; holy-buffers.el --- Instantly kill buffers unless they're special. -*- lexical-binding: t -*-

;;; Commentary:

;; Instantly kill buffers with `C-x k' unless they're listed as
;; 'holy', in which case warn the user.

;;; Code:

(defvar holy-buffers-list '("*scratch*" "*Messages*")
  "Buffers that won't get killed by 'my-kill-buffer'.")


(defun holy-buffers-p ()
  "Determine if current buffer should be spared."
  (or (string= major-mode 'erc-mode)
      (member (buffer-name) holy-buffers-list)))

(defun holy-buffers-kill-buffer ()
  "Kill buffer instantly, unless it's on 'my-holy-buffers'."
  (interactive)
  (if (holy-buffers-p)
      (message "Woah there, you almost killed %s" (buffer-name))
    (kill-buffer (current-buffer))))

(provide 'holy-buffers)
;;; holy-buffers.el ends here
