;;; ruby-frozen-string-literal.el -- Offer to insert the magic comment at the beginnig of ruby files.

;;; Commentary:

;; Convenience function to check the beginning of ruby files for the
;; magic comment # frozen_string_literal: true and offer to insert it
;; if not alrady there

;;; Code:

(defun ruby-frozen-string-literal ()
  "Check the current buffer for the magic comment # frozen_string_literal: true.
If the comment doesn't exist, insert it."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (unless (save-excursion
              (re-search-forward (rx bol "# frozen_string_literal: true" eol) nil 'noerror))
      (insert "# frozen_string_literal: true\n\n"))
    (goto-char (point-min))
    (unless (save-excursion
              (re-search-forward (rx bol "# typed: ") nil 'noerror))
      (insert "# typed: false\n"))))

(provide 'ruby-frozen-string-literal)
;;; ruby-frozen-string-literal.el ends here
