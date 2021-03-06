;;; platformio-helpers.el --- Helper methods for platformio-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Dante Catalfamo

;; Author: Dante Catalfamo
;; Package-Requires: ((emacs "25.1") (async "1.9.0"))

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

;; Helper methods for `platformio-mode'. Currently includes a board
;; selection list

;;; Code:

(require 'json)
(require 'async)
(require 'seq)
(require 'platformio-mode)

(defconst platformio-board-list-buffer "*PlatformIO Boards*")

(defun platformio--add-board (board)
  "Add a BOARD to a PlatformIO porject."
  (platformio--exec (string-join (list "init --ide emacs --board " board))))

(defun platformio--build-table (_autoignore _noconfirm)
  "Return a list of all boards supported by PlatforIO."
  (setq revert-buffer-in-progress-p t)
  (setq mode-line-process ":refreshing")
  (async-start
   (lambda ()
     (require 'seq)
     (require 'json)
     (setq out nil)
     (seq-map
      (lambda (board)
        (push (list (alist-get 'id board)
                    (vector
                     `("Add" action
                       (lambda (_button) (platformio--add-board ,(alist-get 'id board))))
                     (alist-get 'name board)
                     (alist-get 'id board)
                     (alist-get 'mcu board)
                     (alist-get 'platform board)
                     (number-to-string (alist-get 'fcpu board))
                     (number-to-string (alist-get 'ram board))
                     (number-to-string (alist-get 'rom board))
                     (string-join (alist-get 'frameworks board) ", ")
                     (alist-get 'vendor board)
                     `("URL" action
                       (lambda (_button)
                         (browse-url-default-browser ,(alist-get 'url board))))
                     `("Docs" action
                       (lambda (_button)
                         (browse-url-default-browser ,(string-join (list "https://docs.platformio.org/en/latest/boards/"
                                                                         (alist-get 'platform board)
                                                                         "/"
                                                                         (alist-get 'id board)
                                                                         ".html")))))))
              out))
      (json-read-from-string
       (shell-command-to-string "platformio boards --json-output")))
     (nreverse out))

   (lambda (result)
     (with-current-buffer platformio-board-list-buffer
       (setq tabulated-list-entries result)
       (tabulated-list-revert)
       (setq revert-buffer-in-progress-p nil)
       (setq mode-line-process "")))))

(define-derived-mode platformio-boards-mode tabulated-list-mode "PlatformIO-Boards"
  "PlatformIO boards mode."
  (setq tabulated-list-format [("" 3 nil)
                               ("Name" 24 t)
                               ("ID" 30 t)
                               ("MCU" 17 t)
                               ("Platform" 16 t)
                               ("CPU Freq" 12 nil)
                               ("RAM" 12 nil)
                               ("ROM" 12 nil)
                               ("Frameworks" 35 nil)
                               ("Vendor" 20 t)
                               ("" 4 nil)
                               ("" 4 nil)])
  (setq tabulated-list-sort-key nil)
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (setq revert-buffer-function #'platformio--build-table))

(defun platformio-boards ()
  "List boards supported by PlatformIO."
  (interactive)
  (switch-to-buffer platformio-board-list-buffer)
  (platformio-boards-mode)
  (revert-buffer))

(provide 'platformio-helpers)
;;; platformio-helpers.el ends here
