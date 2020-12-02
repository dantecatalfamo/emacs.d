;;; org-roll.el --- roll dice in org-mode -*- lexical-binding: t -*-
;;; Author: Dante Catalfamo
;;; Commentary:
;; Add roll: URLs to org-mode. The format for the URL is from
;; decide.el. On opening, the result of the roll will be printed in
;; the minibuffer.

;;; Code:

(require 'decide)
(require 'ol)

(defun my-org-link--open-roll (spec-string)
  "Roll dice or with SPEC-STRING using `decide'."
  (let ((spec (decide-make-dice-spec spec-string)))
    (message "[%s] -> %s"
             (decide-describe-dice-spec spec)
             (apply #'decide-roll-dice-spec spec))))

(defun my-org-link--complete-roll ()
  "Store roll link."
  (concat "roll:" (replace-regexp-in-string "\s" "" (read-from-minibuffer "Roll: "))))

(org-link-set-parameters "roll"   ; Roll dice with `roll:1d6' link
                           :follow #'my-org-link--open-roll
                           :complete #'my-org-link--complete-roll)

(provide 'org-roll)
;;; org-roll.el ends here
