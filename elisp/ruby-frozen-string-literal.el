;;; ruby-frozen-string-literal.el -- Offer to insert the magic comment at the beginnig of ruby files.

;;; Commentary:

;; Convenience function to check the beginning of ruby files for the
;; magic comment # frozen_string_literal: true and offer to insert it
;; if not alrady there

;;; Code:

(defun ruby-frozen-string-literal ()
  "Check the current buffer for the magic comment # frozen_string_literal: true.
If the comment doesn't exist, offer to insert it."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (unless (string= (thing-at-point 'line)
                     "# frozen_string_literal: true\n")
      (insert "# frozen_string_literal: true\n\n")
      (message "Inserted '# frozen_string_literal: true'"))))

(provide 'ruby-frozen-string-literal)
;;; ruby-frozen-string-literal.el ends here
