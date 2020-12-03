;;; org-table-dates.el --- Generate reminders from inative dates in org tables -*- lexical-binding: t -*-
;;; Author: Dante Catalfamo

;;; Commentary:
;; Generate reminders (headings with active dates) from org tables
;; that include inactive dates.

;;; Code:

(defvar org-table-dates-headline "Reminders")
(defvar org-table-dates-tags ":noexport:")
(defvar org-table-dates-postfix " bill")

(defun org-table-dates--parse-tables ()
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

(defun org-table-dates ()
  "Convert first col name and timestamp of an org-table to reminders."
  (interactive)
  (let* ((row-data (org-table-dates--parse-tables))
         (rows (length row-data))
         (inhibit-message t))
    (save-excursion
      (goto-char (point-min))
      (search-forward-regexp
       (rx bol "* " (eval org-table-dates-headline))
       nil t)
      (when (string= (org-get-heading t) org-table-dates-headline)
        (org-mark-element)
        (delete-active-region))
      (goto-char (point-max))
      (insert "* " org-table-dates-headline)
      (org-set-tags org-table-dates-tags)
      (end-of-line)
      (insert "\n")
      (dolist (row row-data)
        (insert "** " (plist-get row :name) org-table-dates-postfix "\n")
        (insert "   " (plist-get row :timestamp))
        (org-toggle-timestamp-type)
        (insert "\n"))
      (org-up-heading-safe)
      (org-cycle-internal-local))
    (setq inhibit-message nil)
    (message "%d reminders created" rows)))


(provide 'org-table-dates)
;;; org-table-dates.el ends here
