;;; PROJECT-X.EL - Extra convenience features for project.el
;;;
;;; This file is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; For a full copy of the GNU General Public License
;;; see <http://www.gnu.org/licenses/>.
;;;
;;; COMMENTARY:
;;;
;;; Some convenience features for project.el:
;;; - Recognize any directory with a `.project' file as a project.
;;; - Save and restore project files and window configurations across sessions
;;; 
;;; COMMANDS:
;;;
;;; project-window-state-save : Save the window configuration of currently open project buffers
;;; project-window-state-load : Load a previously saved project window configuration
;;;
;;; CUSTOMIZATION:
;;;
;;; `project-window-list-file': File to store project window configurations
;;; `project-local-identifier': String matched against file names to decide if a
;;;                             directory is a project
;;;
;;; by Karthik Chikmagalur
;;; <karthik.chikmagalur@gmail.com>

(require 'project)

(defgroup project-x nil
  "Convenience features for the Project library."
  :group 'project)

;; Persistent project sessions
;; -------------------------------------
(defcustom project-window-list-file
  (locate-user-emacs-file "project-window-list")
  "File in which to save project window configurations by default."
  :type 'file
  :group 'project-x)

(setq project-window-list-file "~/.cache/emacs/project-window-list")

(defvar project-window-alist nil
  "Alist of window configurations associated with known projects.")

(defun project--window-state-write (&optional file)
  "Write project window states to `project-window-list-file'.
If FILE is specified, write to it instead."
  (when project-window-alist
    (with-temp-file  project-window-list-file
      (insert ";;; -*- lisp-data -*-\n")
      (prin1 project-window-alist (current-buffer)))
    (message (format "Wrote project window state to %s" project-window-list-file))))

(defun project--window-state-read (&optional file)
  "Read project window states from
`project-window-list-file'. If FILE is specified, read from it
instead."
  (and (or file
           (file-exists-p project-window-list-file))
       (with-temp-buffer
         (insert-file-contents (or file project-window-list-file))
         (if-let ((win-state-alist (read (current-buffer))))
             (setq project-window-alist win-state-alist)
           (message (format "Could not read %s" project-window-list-file))))))

(defun project-window-state-save (&optional arg)
  "Save current window state of project. With optional
argument ARG, query for project."
  (interactive "P")
  (when-let* ((dir (if arg (project-prompt-project-dir)
                     (project-root (project-current t))))
              (default-directory dir))
    (unless project-window-alist (project--window-state-read))
    (let ((file-list))
      ;; Collect file-list of all the open project buffers
      (dolist (buf (project--buffer-list (project-current)) file-list)
        (if-let ((file-name (or (buffer-file-name buf)
                                (with-current-buffer buf
                                  (and (eq major-mode 'dired-mode)
                                       dired-directory)))))
            (add-to-list 'file-list file-name)))
      (setf (alist-get dir project-window-alist nil nil 'equal)
            (list (cons 'files file-list)
                  (cons 'windows (window-state-get nil t)))))
    (message (format "Saved windows for %s" dir))))

(defun project-window-state-load (dir)
  "Load the saved window state for project with directory
DIR. If DIR is unspecified query the user for a project instead."
  (interactive (list (project-prompt-project-dir)))
  (unless project-window-alist (project--window-state-read))
  (if-let* ((project-window-alist)
            (project-state (alist-get dir project-window-alist
                                      nil nil 'equal)))
      (let ((file-list (alist-get 'files project-state))
            (window-config (alist-get 'windows project-state)))
        (dolist (file-name file-list nil)
          (find-file file-name))
        (window-state-put window-config nil 'safe)
        (message (format "Loaded windows for %s" dir)))
    (message (format "No saved window state for project %s" dir))))

(defun project-windows ()
  "Restore the last saved window state of the chosen project."
  (interactive)
  (project-window-state-load (project-root (project-current))))

;; Recognize directories as projects by defining a new project backend `local'
;; -------------------------------------
(defcustom project-local-identifier ".project"
  "Filename that identifies a directory as a project."
  :type 'string
  :group 'project-x)

(cl-defmethod project-root ((project (head local)))
  (cdr project))

(defun project-try-local (dir)
  "Determine if DIR is a non-VC project.
DIR must include a .project file to be considered a project."
  (if-let ((root (locate-dominating-file dir project-local-identifier)))
      (cons 'local root)))

(define-minor-mode project-x-mode
  "Minor mode to enable extra convenience features for
  project.el. When enabled, save and load project window states.
  Recognize any directory that contains (or whose parent
  contains) a special file as a project."
  :global t
  :version "0.10"
  :lighter ""
  :group 'project-x
  (if project-x-mode
      ;;Turning the mode ON
      (progn
        (add-hook 'project-find-functions 'project-try-local 90)
        (add-hook 'kill-emacs-hook 'project--window-state-write)
        (project--window-state-read))
    (remove-hook 'project-find-functions 'project-try-local 90)
    (remove-hook 'kill-emacs-hook 'project--window-state-write)))

(provide 'project-x)
