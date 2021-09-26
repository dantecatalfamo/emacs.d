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

(defconst org-dnd-location-heading "Locations"
  "Title of the heading where locations are stored.")

(defconst org-dnd-pc-heading "PCs"
  "Title of the heading where PCs are stored.")

(defun org-dnd--list-subheadings (heading-name)
  "Return a list of the titles of the headings under first level HEADING-NAME."
  (car (org-element-map (org-element-parse-buffer 'heading) '(headline)
         (lambda (headline)
           (when (and (string= (org-element-property :title headline) heading-name)
                      (eq (org-element-property :level headline) 1))
             (mapcar
              (lambda (heading)
                (org-element-property :title heading))
              (org-element-contents headline)))))))

(defun org-dnd--list-recursive-subheadings (heading-name)
  "Return a list of the titles of the headings under first level HEADING-NAME, recursively."
  (car (org-element-map (org-element-parse-buffer 'heading) '(headline)
     (lambda (headline)
       (when (and (string= (org-element-property :title headline) heading-name)
                  (eq (org-element-property :level headline) 1))
         (org-element-map (cdr headline) '(headline)
           (lambda (subhead)
             (org-element-property :title subhead))))))))

(defun org-dnd--jump-to-heading (headline)
  "Move point to HEADLINE."
  (push-mark)
  (goto-char (point-min))
  (org-link-search (concat "*" headline))
  (org-show-entry))

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

;; Non-Player Characters

(defun org-dnd-new-npc (name &optional specified-location)
  "Add a new NPC with NAME, met at SPECIFIED-LOCATION.
Location will be the current header if nil.
Insert link to NPC at point."
  (interactive "MName: ")
  (let ((location (or specified-location (org-get-heading 'no-tags 'no-todo 'no-prio 'no-comm))))
    (insert "[[*" name "][" name "]]")
    (save-excursion
      (goto-char (point-min))
      (search-forward-regexp (rx bol "* " (literal org-dnd-npc-heading)) nil 'noerror)
      (forward-char)
      (insert "** " name "\n")
      (insert "   Met at [[*" location "][" location "]] on " (format-time-string "%c") "\n\n"))))

(defun org-dnd-list-npcs ()
  "Return a list of all NPC names."
  (org-dnd--list-subheadings org-dnd-npc-heading))

(defun org-dnd-reference-npc (name &optional location)
  "Insert a link to an existing NPC with NAME.
Creates NPC if referenced NPC does not exist, with LOCATION passed."
  (interactive (list (completing-read "Name: " (org-dnd-list-npcs) nil nil)))
  (if (member name (org-dnd-list-npcs))
      (insert "[[*" name "][" name "]]")
    (org-dnd-new-npc name location)))

(defun org-dnd-jump-to-npc (name)
  "Move cursor to NAME NPC's entry."
  (interactive (list (completing-read "NPC: " (org-dnd-list-npcs))))
  (org-dnd--jump-to-heading name))

;; Quests

(defun org-dnd-new-quest (quest-name npc-name)
  "Create a new quest called QUEST-NAME given by NPC-NAME."
  (interactive (list (read-from-minibuffer "Quest: ")
                     (completing-read "Quest giver: " (org-dnd-list-npcs))))
  (let ((location (org-get-heading 'no-tags 'no-todo 'no-pro 'no-comm)))
    (insert "[[*" quest-name "][" quest-name "]]")
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
  (org-dnd--list-subheadings org-dnd-quest-heading))

(defun org-dnd-reference-quest (quest-name)
  "Insert a link to an existing quest QUEST-NAME."
  (interactive (list (completing-read "Quest: " (org-dnd-list-quests) nil t)))
  (insert "[[*" quest-name "][" quest-name "]]"))

(defun org-dnd-jump-to-quest (name)
  "Move cursor to NAME quest."
  (interactive (list (completing-read "Quest: " (org-dnd-list-quests))))
  (org-dnd--jump-to-heading name))

;; Locations

(defun org-dnd-list-locations ()
  "Return a list of all locations."
  (org-dnd--list-recursive-subheadings org-dnd-location-heading))

(defun org-dnd-reference-location (name)
  "Insert a link to an existing location NAME."
  (interactive (list (completing-read "Location: " (org-dnd-list-locations) nil t)))
  (insert "[[*" name "][" name "]]"))

(defun org-dnd-jump-to-location (name)
  "Move cursor to NAME location entry."
  (interactive (list (completing-read "Location: " (org-dnd-list-locations))))
  (org-dnd--jump-to-heading name))


;; Player Characters

(defun org-dnd-list-pcs ()
  "Return a list of all PCs."
  (org-dnd--list-subheadings org-dnd-pc-heading))

(defun org-dnd-reference-pc (name)
  "Insert a link to an existing PC NAME."
  (interactive (list (completing-read "Player Character: " (org-dnd-list-pcs) nil t)))
  (insert "[[*" name "][" name "]]"))

(defun org-dnd-jump-to-pc (name)
  "Move cursor tp NAME PC entry."
  (interactive (list (completing-read "Player Character: " (org-dnd-list-pcs))))
  (org-dnd--jump-to-heading name))

;; Transient map

(transient-define-prefix org-dnd-transient-root ()
  "Org-dnd transient map."
  ["Org D&D"
   [("s" "Setup buffer" org-dnd-setup)]
   [("rn" "Reference NPC" org-dnd-reference-npc)
    ("rq" "Reference Quest" org-dnd-reference-quest)
    ("rl" "Reference Location" org-dnd-reference-location)
    ("rp" "Reference Player Character" org-dnd-reference-pc)]
   [("nn" "New NPC" org-dnd-new-npc)
    ("nq" "New Quest" org-dnd-new-quest)]
   [("jn" "Jump to NPC" org-dnd-jump-to-npc)
    ("jq" "Jump to Quest" org-dnd-jump-to-quest)
    ("jl" "Jump to Location" org-dnd-jump-to-location)
    ("jp" "Jump to Player Character" org-dnd-jump-to-pc)]])

(provide 'org-dnd)
;;; org-dnd.el ends here
