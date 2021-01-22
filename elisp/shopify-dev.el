;;; shopify-dev.el --- Helper functions for Shopify's `dev' tool  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Dante Catalfamo

;; Author: Dante Catalfamo
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))

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

;; Very basic integration with `dev'.

;;; Code:

(require 'f)
(require 'projectile)
(require 'vterm)

(defvar shopify-dev-dir "/opt/dev/" "Location of the `dev' directory.")

(defun shopify-dev--run (dev-command)
  "Run dev with DEV-COMMAND argument in the current directory.
Immediately exits upon success."
  (let* ((project-name (car (last (f-split (projectile-project-root)))))
         (buffer-name (format "*dev %s (%s)*" dev-command project-name))
         (source-path (expand-file-name "dev.sh" shopify-dev-dir))
         (bash-command (format "cd \"%s\";source %s; dev %s && exit"
                               default-directory
                               source-path
                               dev-command))
         (buffer (generate-new-buffer buffer-name)))
    (display-buffer buffer)
    (with-current-buffer buffer
      (vterm-mode)
      (vterm-send-string bash-command)
      (vterm-send-return))))

(defun shopify-dev-up ()
  "Run `dev up' in the current directory."
  (interactive)
  (shopify-dev--run "up"))


(defun shopify-dev-server ()
  "Run `dev server' in the current directory."
  (interactive)
  (shopify-dev--run "server"))

(defun shopify-dev-down ()
  "Run `dev down' in the current directory."
  (interactive)
  (shopify-dev--run "down"))

(provide 'shopify-dev)
;;; shopify-dev.el ends here
