(defun org-ssh-connect (&optional arg)
  "Connect to the host at point and open `dired'.
If ARG is non-nil, open `eshell' instead of `dired'."
  (interactive "P")
  (let* ((properties (org-entry-properties))
         (name (alist-get "ITEM" properties nil nil #'string=))
         (user (alist-get "SSH_USER" properties nil nil #'string=))
         (port (alist-get "SSH_PORT" properties nil nil #'string=))
         (host (or (alist-get "IP" properties nil nil #'string=)
                   (alist-get "HOSTNAME" properties nil nil #'string=))))
    (if host
        (let ((default-directory (format "/ssh:%s%s%s:"
                                         (if user (format "%s@" user) "")
                                         name
                                         (if port (format "#%s" port) ""))))
          (message "Connecting to %s..." name)
          (if arg
              (eshell t)
            (dired ".")))
      (user-error "Not an SSH host"))))
