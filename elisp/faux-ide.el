;;; faux-ide.el --- Setup windows in an IDE-like layout  -*- lexical-binding: t -*-

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

;; Setup windows in an IDE-style layout.
;; Treemacs on the left, imenu-list on the right, and a terminal on
;; the bottom

;;; Code:

(require 'vterm)
(require 'imenu-list)
(require 'treemacs)

(defun faux-ide-enable ()
  "Open faux ide mode."
  (interactive)
  (let ((buf (current-buffer))
        (window-min-height 30)
        (split-width-threshold nil))
    (vterm-toggle-show t)
    (shrink-window-if-larger-than-buffer)
    (imenu-list-show-noselect)
    (treemacs-select-window)
    (pop-to-buffer buf)))

(defun faux-ide-disable ()
    "Close faux ide mode."
    (let ((buf (current-buffer)))
      (vterm-toggle-hide)
      (imenu-list-minor-mode -1)
      (delete-window (treemacs-get-local-window))
      (pop-to-buffer buf)
      (delete-other-windows)
      (recenter-top-bottom 30)))

(defun faux-ide-toggle ()
  "Toggle faux ide mode."
  (interactive)
  (if (eq (treemacs-current-visibility) 'visible)
      (faux-ide-disable)
    (faux-ide-enable)))

(provide 'faux-ide)
;;; faux-ide.el ends here
