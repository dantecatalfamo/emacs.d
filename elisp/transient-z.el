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
    ("s" "Dev server" shopify-dev-server)]])

(transient-define-prefix transient-z-org ()
  "Personal org transient map."
  ["Org"
   [("t" "Table Dates" org-table-dates)
    ("r" "Recalculate Buffer Tables" org-table-recalculate-buffer-tables)]])

(transient-define-prefix transient-z ()
  "Personal transient keymap for `C-z'."
  ["Helpers"
   ("h" "Hugo" transient-z-hugo)
   ("s" "Shopify" transient-z-shopify)
   ("o" "Org" transient-z-org)])

(provide 'transient-z)
;;; transient-z.el ends here
