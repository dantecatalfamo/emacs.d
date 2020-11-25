;;; dev-helper -- heplers for managing dev project directories -*- lexical-binding: t; -*-
;;; Commentary:
;;; A few helpful functions for managing and navigating work projects

;;; Code:

(require 'f)
(require 'dash)
(require 'projectile)
(require 'treemacs)

(defvar dev-projects-root "~/src"
  "Root directory under which all projects are located.")

(defvar dev-projects-depth 3
  "Depth under the root that projects' root folders are located.")


(defun dev--list-subdirs (directory &optional depth)
  "Return a list of directories inside of DIRECTORY.
DEPTH is how many folers deep to go."
  (unless depth (setq depth 1))
  (if (< depth 1)
      directory
    (let (subdir-list)
      (dolist (dir (directory-files directory) subdir-list)
        (unless (member dir '("." ".."))
          (when (f-directory? (f-join directory dir))
            (push (dev--list-subdirs (expand-file-name (concat (file-name-as-directory directory)
                                                          dir))
                                (1- depth))
                  subdir-list))))
      (-flatten subdir-list))))


(defun dev--last-n-dirs (dir-path depth)
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


(defun dev--list-last-dirs (dir-list depth)
  "Given list of paths DIR-LIST, apply dev--last-n-dirs to depth DEPTH."
  (let (dir-paths)
    (dolist (dir dir-list dir-paths)
      (push (dev--last-n-dirs dir depth) dir-paths))))


(defun dev-add-project-projectile (project-root project-name)
  "Add project to projectile.
PROJECT-ROOT is the project's root directory, PROJECT-NAME is the name."
  (unless (member project-root
                  projectile-known-projects)
    (projectile-add-known-project project-root)))


(defun dev-add-project-treemacs (project-root project-name)
  "Add project to treemacs.
PROJECT-ROOT is the project's root directory, PROJECT-NAME is the name."
  (treemacs-do-add-project-to-workspace project-root
                                        project-name))


(defun dev-select-project (root depth)
  "Interactively sekect a project directory starting at directory ROOT going to depth DEPTH."
  (f-join root (completing-read "Select Project: " (dev--list-last-dirs (dev--list-subdirs root depth) depth))))


(defun dev-change-project (project-root &optional ARG)
  "Change dev projects, add the selected project to projectile and treemacs.
PROJECT-ROOT is the root directory of the project.
When run with ARG, open project with Dired instead of projectile"
  (interactive (list
                (dev-select-project dev-projects-root dev-projects-depth)
                current-prefix-arg))

    (let ((project-name (file-name-nondirectory
                         (directory-file-name project-root))))

      (dev-add-project-projectile project-root project-name)
      (dev-add-project-treemacs project-root project-name))

    (if ARG
        (dired project-root)
      (projectile-switch-project-by-name project-root)))


;;;###autoload
(defun dev-open-pr ()
  "Open a PR from the current branch (github only)."
  (interactive)
  (let ((origin-url (string-trim "git remote get-url origin"))
        (current-branch (string-trim "git rev-parse --abbrev-ref HEAD")))
    (browse-url (format "https://github.com/%s/pull/new/%s"
                        (replace-regexp-in-string ".+github.com:\\(.+\\)\\.git" "\\1" origin-url)
                        current-branch))))

(provide 'dev-helper)
;;; dev-helper.el ends here
