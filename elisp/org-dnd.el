;;; org-dnd.el --- Org-Mode functions for keeping notes in D&D  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Dante Catalfamo

;; Author: Dante Catalfamo

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; A package full of helper functions tha can be helpful when playing
;; D&D and keeping notes inide of org-mode

;; The general structure of the note file should look like this.
;; * Locations
;; * Quests
;; * NPCs
;; * PCs

;;; Code:

(require 'org)

(defun org-dnd-new-npc (name)
  "Add a new NPC with NAME, met at LOCATION.
Insert link to NPC at point."
  (interactive "MName: ")
  (let ((current-location (org-get-heading 'no-tags 'no-todo 'no-prio 'no-comm)))
    (save-excursion
      (goto-char (point-min))
      (search-forward-regexp (rx bol "* NPCs") nil 'noerror)
      (forward-char)
      (insert "** " name "\n")
      (insert "   Met at " current-location "\n\n")))
  (insert "[[*" name "][" name "]]"))

(provide 'org-dnd)
;;; org-dnd.el ends here
