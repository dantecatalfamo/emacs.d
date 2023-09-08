;;; original-txt.el --- Create and write to Original.txt file  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Dante Catalfamo

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

;; This is used for a personal organizational system used for
;; downloaded from the internet

;;; Code:

(require 'helm)

;;;###autoload
(defun original-txt-helm-kill ()
  "Create Original.txt from the kill ring selected by helm."
  (interactive)
  (with-temp-file "Original.txt" (helm-show-kill-ring)))

;;;###autoload
(defun original-txt-from-kill ()
  "Create a file Original.txt in the current directory from the last kill."
  (interactive)
  (with-temp-file "Original.txt" (yank)))

(defun original-txt-from-string (str)
  "Create a file Original.txt in the current directory with the content STR."
  (with-temp-file "Original.txt" (insert str)))

;;;###autoload
(defun original-txt-from-helm-kill ()
  "Create Original.txt from the kill ring selected by helm."
  (interactive)
  (find-file "Original.txt")
  (helm-show-kill-ring))

(provide 'original-txt)
;;; original-txt.el ends here
