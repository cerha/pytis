;;; specinfo.el --- specinfo Emacs utilities

;; Copyright (C) 2013 Brailcom, o.p.s.

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(require 'diff-mode)
(require 'python)
(require 'vc-git)
(require 'gensqlalchemy)


(defgroup pytis nil
  "Emacs pytis support."
  :group 'python)

(defcustom pytis-specinfo "specinfo"
  "specinfo binary."
  :group 'pytis
  :type 'string)

(defcustom pytis-config-directory nil
  "Directory storing application configuration files.
If non-nil then ask the user for a configuration file from this directory when
`pytis-config-file' is nil."
  :group 'pytis
  :type 'directory)

(defvar pytis-config-file nil
  "Location of the application configuration file.
If nil then try to find the configuration file automatically or ask the user.")
(make-variable-buffer-local 'pytis-config-file)

(defun pytis-set-config-file ()
  (interactive)
  (setq pytis-config-file
        (expand-file-name
         (read-file-name "Configuration file: " pytis-config-directory))))  
  
(defun pytis-config-file ()
  (cond
   (pytis-config-file
    pytis-config-file)
   (pytis-config-directory
    (pytis-set-config-file))
   (t
    (let ((config-file (concat default-directory "config.py")))
      (while (and (not (file-exists-p config-file))
                  (not (string= (file-name-directory config-file) "/")))
        (setq config-file (concat (file-name-directory
                                   (directory-file-name
                                    (file-name-directory config-file)))
                                  (file-name-nondirectory config-file))))
      (if (string= (file-name-directory config-file) "/")
          (pytis-set-config-file)
        config-file)))))

(defun pytis-specinfo (arg)
  "Show information about current presentation specification.
With a prefix argument show information diff of all
specifications against selected git branch."
  (interactive "P")
  (with-gensqlachemy-specification
    (with-gensqlalchemy-pythonpath
      (let* ((diff (when arg (completing-read "Compare against branch: " (vc-git-branches))))
             (module (gensqlalchemy-module))
             (submodule (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
             (config-file (pytis-config-file))
             (output-buffer (get-buffer-create (if arg "*specinfo-diff*" "*specinfo*")))
             (args (append (when config-file (list "--config" config-file))
                           (when diff (list "--diff" diff))
                           (list module)
                           (unless diff (list (concat submodule "." specification-name "$"))))))
        (pop-to-buffer output-buffer)
        (erase-buffer)
        (insert pytis-specinfo " " (mapconcat #'identity args " ") "\n")
        (when (and (= (apply 'call-process pytis-specinfo nil t nil args) 0)
                   arg)
          (diff-mode))))))


;;; Announce

(provide 'specinfo)

;;; specinfo.el ends here
