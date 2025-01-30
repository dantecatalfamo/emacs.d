;;; repo-helper -- heplers for managing project directories -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022 Dante Catalfamo

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

;; A few helpful functions for managing and navigating work and personal projects

;;; Code:

(require 'f)
(require 'dash)

(defvar repo-projects-root "~/src"
  "Root directory under which all projects are located.")

(defvar repo-projects-depth 3
  "Depth under the root that projects' root folders are located.")

(defconst my-git-trim-regex
  (rx (or (seq bol (or "ssh"
                       (seq "http" (? "s")))
               "://")
          (seq ".git" eol)))
  "Regex to remove unnecessary path elements from git url.")


(defun repo--list-subdirs (directory &optional depth)
  "Return a list of directories inside of DIRECTORY.
DEPTH is how many folers deep to go."
  (unless depth (setq depth 1))
  (if (< depth 1)
      directory
    (let (subdir-list)
      (dolist (dir (directory-files directory) subdir-list)
        (unless (member dir '("." ".."))
          (when (f-directory? (f-join directory dir))
            (push (repo--list-subdirs (f-join directory dir) (1- depth))
                  subdir-list))))
      (-flatten subdir-list))))


(defun repo--last-n-dirs (dir-path depth)
  "Return a relative path of the last n elements of a path.
Number of elements specified by DEPTH, path is a string DIR-PATH."
  (let* ((path (f-split dir-path))
         (length (safe-length path))
         path-parts)
    (dotimes (counter depth path-parts)
      (push (nth (- (1- length) counter)
                 path)
            path-parts))
    (apply 'f-join path-parts)))


(defun repo--list-last-dirs (dir-list depth)
  "Given list of paths DIR-LIST, apply repo--last-n-dirs to depth DEPTH."
  (let (dir-paths)
    (dolist (dir dir-list dir-paths)
      (push (repo--last-n-dirs dir depth) dir-paths))))


(defun repo-select-project (root depth)
  "Interactively sekect a project directory starting at directory ROOT going to depth DEPTH."
  (f-join root (completing-read "Select Project: " (repo--list-last-dirs (repo--list-subdirs root depth) depth))))


;;;###autoload
(defun repo-change-project (project-root)
  "Change dev projects, add the selected project.el.
PROJECT-ROOT is the root directory of the project."
  (interactive (list
                (repo-select-project repo-projects-root repo-projects-depth)
                current-prefix-arg))

  (dired project-root)
  (project-remember-project (project-current)))


;;;###autoload
(defun repo-clone (url)
  "Clone a git repository at URL into the appropriate directory."
  (interactive "sRepository URL: ")
  (let* ((repo-path (replace-regexp-in-string my-git-trim-regex "" url))
         (full-path (expand-file-name repo-path (expand-file-name "src" "~"))))
    (message "Cloning...")
    (async-start-process "git clone" "git" `(lambda (_proc) (message "Cloning complete: %s" ,full-path))
                         "clone" "--recursive" url full-path)))

;;;###autoload
(defun repo-open-pr ()
  "Open a PR from the current branch (github only)."
  (interactive)
  (let ((origin-url (string-trim "git remote get-url origin"))
        (current-branch (string-trim "git rev-parse --abbrev-ref HEAD")))
    (browse-url (format "https://github.com/%s/pull/new/%s"
                        (replace-regexp-in-string ".+github.com:\\(.+\\)\\.git" "\\1" origin-url)
                        current-branch))))

(provide 'repo-helper)
;;; repo-helper.el ends here
