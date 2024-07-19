;;; now-playing.el --- Inser current song at point  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Dante Catalfamo

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

;; Nothing

;;; Code:


(defvar insert-now-playing-script
   "use AppleScript version \"2.4\" -- Yosemite (10.10) or later
use scripting additions

tell application \"Spotify\"
	set c to the current track
	tell me to \"Song: \" & name of c & \"
Album: \" & album of c & \"
Artist: \" & artist of c as text
end tell"
   )

(defun now-playing-get-info ()
  "Return the information on the current song."
  (shell-command-to-string (format "osascript -e '%s'" insert-now-playing-script)))

(defun insert-now-playing ()
  "Insert the current playing Spotify song at point."
  (interactive)
  (insert (now-playing-get-info)))


(provide 'now-playing)
;;; now-playing.el ends here
