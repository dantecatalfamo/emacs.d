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
(require 'org-element)

(defconst org-dnd-npc-heading "NPCs"
  "Title of the heading where NPCs are stored.")

(defconst org-dnd-quest-heading "Quests"
  "Title of the heading where quests are stored.")

(defun org-dnd-new-npc (name &optional location)
  "Add a new NPC with NAME, met at LOCATION.
Location will be the current header if nil.
Insert link to NPC at point."
  (interactive "MName: ")
  (let ((current-location (org-get-heading 'no-tags 'no-todo 'no-prio 'no-comm)))
    (insert "[[*" name "][" name "]]")
    (save-excursion
      (goto-char (point-min))
      (search-forward-regexp (rx bol "* " (literal org-dnd-npc-heading)) nil 'noerror)
      (forward-char)
      (insert "** " name "\n")
      (insert "   Met at " (or location current-location) " on " (format-time-string "%c") "\n\n"))))

(defun org-dnd-list-npcs ()
  "Return a list of all NPC names."
  (car (org-element-map (org-element-parse-buffer 'heading) '(headline)
         (lambda (headline)
           (if (and (string= (org-element-property :title headline) org-dnd-npc-heading)
                    (eq (org-element-property :level headline) 1))
               (mapcar
                (lambda (heading)
                  (org-element-property :title heading))
                (org-element-contents headline)))))))

(defun org-dnd-reference-npc (name &optional location)
  "Insert a link to an existing NPC with NAME.
Created NPC if referenced NPC does not exist, with LOCATION passed."
  (interactive (list (completing-read "Name: " (org-dnd-list-npcs) nil nil)))
  (if (member name (org-dnd-list-npcs))
      (insert "[[*" name "][" name "]]")
    (org-dnd-new-npc name location)))

(defun org-dnd-jump-to-npc (name)
  "Move cursor to NAME NPC's entry."
  (interactive (list (completing-read "NPC: " (org-dnd-list-npcs))))
  (push-mark)
  (goto-char (point-min))
  (org-link-search (concat "*" name))
  (org-show-entry))

(defun org-dnd-new-quest (quest-name npc-name)
  "Create a new quest called QUEST-NAME given by NPC-NAME."
  (interactive (list (read-from-minibuffer "Quest: ")
                     (completing-read "Quest giver: " (org-dnd-list-npcs))))
  (let ((location (org-get-heading 'no-tags 'no-todo 'no-pro 'no-comm)))
    (insert "Got quest [[*" quest-name "][" quest-name "]]")
    (save-excursion
      (goto-char (point-min))
      (search-forward-regexp (rx bol "* " (literal org-dnd-quest-heading)) nil 'noerror)
      (forward-char)
      (org-insert-todo-subheading nil)
      (insert quest-name "\n")
      (insert "   Given by ")
      (org-dnd-reference-npc npc-name location)
      (insert " at [[*" location "][" location "]]")
      (insert " on " (format-time-string "%c") "\n"))))

(defun org-dnd-list-quests ()
  "List all quests."
  (car (org-element-map (org-element-parse-buffer 'heading) '(headline)
         (lambda (headline)
           (if (and (string= (org-element-property :title headline) org-dnd-quest-heading)
                    (eq (org-element-property :level headline) 1))
               (mapcar
                (lambda (heading)
                  (org-element-property :title heading))
                (org-element-contents headline)))))))

(defun org-dnd-jump-to-quest (name)
  "Move cursor to NAME quest."
  (interactive (list (completing-read "Quest: " (org-dnd-list-quests))))
  (push-mark)
  (goto-char (point-min))
  (org-link-search (concat "*" name))
  (org-show-context))

(defun org-dnd-setup ()
  "Setup the buffer to have the correct headings."
  (interactive)
  (save-excursion
    (mapc
     (lambda (heading)
       (goto-char (point-min))
       (unless (search-forward-regexp (concat "^\\* " heading) nil 'noerror)
         (goto-char (point-max))
         (insert "\n* " heading "\n")))
     '("Locations" "Quests" "NPCs" "PCs"))))

(provide 'org-dnd)
;;; org-dnd.el ends here
