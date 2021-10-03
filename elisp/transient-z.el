;;; transient-z.el --- Personal transient keymap for C-z  -*- lexical-binding: t -*-

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

;;

;;; Code:

(require 'transient)
(require 'org-dnd)

(transient-define-prefix transient-z-hugo ()
  "Personal Hugo transient map."
  ["Hugo"
   [("s" "Server" my-hugo-server)
    ("d" "Deploy" my-hugo-deploy)
    ("i" "Insert ISO date" my-insert-iso-date)]])

(transient-define-prefix transient-z-shopify ()
  "Personal Shopify transient map."
  ["Shopify"
   [("u" "Dev up" shopify-dev-up)
    ("d" "Dev down" shopify-dev-down)
    ("s" "Dev server" shopify-dev-server)
    ("p" "Spin status" spin-status)]])

(transient-define-prefix transient-z-org ()
  "Personal org transient map."
  ["Org"
   [("t" "Table Dates" org-table-dates)
    ("r" "Recalculate Buffer Tables" org-table-recalculate-buffer-tables)]])

(transient-define-prefix transient-z-generic ()
  "Personal generic non-mode specific command map."
  ["Generic"
   [("a" "Align non-space" align-non-space)]])

(transient-define-prefix transient-z-platformio ()
  "Personal platformio transient map."
  ["PlatformIO"
   [("b" "Build Project" platformio-build)
    ("l" "Board List" platformio-boards)
    ("u" "Upload Project" platformio-upload)
    ("p" "Upload using External Programmer" platformio-programmer-upload)
    ("s" "Upload SPIFFS" platformio-spiffs-upload)
    ("c" "Clean Project" platformio-clean)
    ("d" "Update Project Libraries" platformio-update)
    ("i" "Update Project Workspace and Index" platformio-init-update-workspace)]])



(transient-define-prefix transient-z ()
  "Personal transient keymap for `C-z'."
  ["Helpers"
   ("d" "Org D&D" org-dnd-transient-root)
   ("g" "Generic" transient-z-generic)
   ("h" "Hugo" transient-z-hugo)
   ("s" "Shopify" transient-z-shopify)
   ("o" "Org" transient-z-org)
   ("p" "PlatformIO" transient-z-platformio)])

(provide 'transient-z)
;;; transient-z.el ends here
