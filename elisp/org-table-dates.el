; -*- lexical-binding: t -*-

;;; Code:

(defvar my-org-table-dates-headline "Reminders")
(defvar my-org-table-dates-tags ":noexport:")
(defvar my-org-table-dates-postfix " bill")

(defun my-finance--parse-tables ()
  "Parse all org-tables with dates in buffer to a plist."
  (let (row-data)
    (org-element-map (org-element-parse-buffer) 'table-row
      (lambda (row)
        (let (name timestamp)
          (org-element-map row 'table-cell
            (lambda (cell)
              (let* ((contents (car (org-element-contents cell)))
                     (type (org-element-type contents)))
                (cond
                 ((eq type 'timestamp)
                  (setq timestamp (org-element-property :raw-value contents)))
                 ((and (eq type 'plain-text) (null name))
                  (setq name (org-no-properties contents)))
                 ((eq type 'link)
                  (setq name (org-no-properties
                              (car (org-element-contents contents)))))))))
          (when (and timestamp name)
            (push `(:timestamp ,timestamp :name ,name) row-data)))))
    (nreverse row-data)))

(defun my-finance-table ()
  "Convert first col name and timestamp to reminders."
  (interactive)
  (let* ((row-data (my-finance--parse-tables))
         (rows (length row-data))
         (inhibit-message t))
    (save-excursion
      (goto-char (point-min))
      (search-forward-regexp
       (rx bol "* " (eval my-org-table-dates-headline))
       nil t)
      (when (string= (org-get-heading t) my-org-table-dates-headline)
        (org-mark-element)
        (delete-active-region))
      (goto-char (point-max))
      (insert "* " my-org-table-dates-headline)
      (org-set-tags my-org-table-dates-tags)
      (end-of-line)
      (insert "\n")
      (dolist (row row-data)
        (insert "** " (plist-get row :name) my-org-table-dates-postfix "\n")
        (insert "   " (plist-get row :timestamp))
        (org-toggle-timestamp-type)
        (insert "\n"))
      (org-up-heading-safe)
      (org-cycle-internal-local))
    (setq inhibit-message nil)
    (message "%d reminders created" rows)))



;
