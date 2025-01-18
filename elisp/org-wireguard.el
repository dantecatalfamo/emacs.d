;;; org-wireguard.el --- Personal wireguard table functions  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Dante Catalfamo

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

;; Functions for manipulating my wireguard org table

;;; Code:

(require 'org)
(require 'org-element)

(defun org-wireguard-table-new-entry (name ip &optional public-addr public-port)
  "Insert a row into the wireguard table.
NAME and IP are required, PUBLIC-ADDR and PUBLIC-PORT are optional."
  (interactive
   (list
    (read-from-minibuffer "Name: ")
    (read-from-minibuffer "IP: " "192.168.69.")
    (read-from-minibuffer "Public Addr: ")
    (read-from-minibuffer "Public Port: ")))
  (let* ((privkey (string-trim (shell-command-to-string "wg genkey")))
         (pubkey (string-trim (shell-command-to-string (format "wg pubkey <<< %s" privkey))))
         (table-row (format "| %s | %s | %s | %s | %s | %s |" name ip public-addr public-port pubkey privkey)))
    (save-excursion
      (goto-char (org-table-end))
      (insert table-row)
      (org-table-align)
      (newline))))

(defun org-wireguard-new-table ()
  "Create a new org wireguard table."
  (interactive)
  (org-table-create "6x2")
  (org-table-next-field)
  (insert "Name")
  (org-table-next-field)
  (insert "Internal IP")
  (org-table-next-field)
  (insert "Public Address")
  (org-table-next-field)
  (insert "Public Port")
  (org-table-next-field)
  (insert "PublicKey")
  (org-table-next-field)
  (insert "PrivateKey")
  (org-table-next-field))

(defun org-wireguard-export-openbsd ()
  "Export the wireguard table at point to OpenBSD's hostname.if format."
  (interactive)
  (let ((table (org-table-to-lisp)))
    (apply #'concat
           (mapcar
            (lambda (row)
              (let ((name (substring-no-properties (nth 0 row)))
                    (ip (substring-no-properties (nth 1 row)))
                    (pub-addr (substring-no-properties (nth 2 row)))
                    (pub-port (substring-no-properties (nth 3 row)))
                    (pubkey (substring-no-properties (nth 4 row)))
                    (_privkey (substring-no-properties (nth 5 row))))
                (concat
                 (format "wgpeer %s wgaip %s" pubkey ip)
                 (when (not (string-empty-p pub-addr))
                   (format " wgendpoint %s %s" pub-addr pub-port))
                 (format " # %s\n" name))))
            (cddr table)))))

(provide 'org-wireguard)
;;; org-wireguard.el ends here
