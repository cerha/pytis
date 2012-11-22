;;; gensqlalchemy.el --- support for working with pytis database specifications

;; Copyright (C) 2012 Brailcom, o.p.s.

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

(require 'cl)
(require 'sql)

(defcustom gensqlalchemy-gsql "gsql"
  "gsql binary.")

(defvar gensqlalchemy-specification-directory "dbdefs")
(defvar gensqlalchemy-common-directories '("lib"))

(define-minor-mode gensqlalchemy-mode
  "Toggle gensqlalchemy mode.
Currently the mode just defines some key bindings."
  nil " GQ" '(("\C-c\C-qe" . gensqlalchemy-eval)
              ("\C-c\C-q\C-q" . gensqlalchemy-eval)
              ("\C-c\C-qa" . gensqlalchemy-add)
              ("\C-c\C-qd" . gensqlalchemy-definition)
              ("\C-c\C-qs" . gensqlalchemy-show-sql-buffer)
              ("\C-c\C-qt" . gensqlalchemy-test)
              ))

(defun gensqlalchemy-sql-mode ()
  (unless (eq major-mode 'sql-mode)
    (sql-mode)
    (sql-set-product 'postgres)))

(defun gensqlalchemy-specification-directory (buffer &optional require)
  (let ((directory (with-current-buffer (or buffer (current-buffer))
                     (directory-file-name default-directory))))
    (while (and (not (string= directory "/"))
                (not (string= (file-name-nondirectory directory) gensqlalchemy-specification-directory)))
      (setq directory (directory-file-name (file-name-directory directory))))
    (when (and require (string= directory "/"))
      (error "Specification directory not found"))
    (file-name-directory directory)))
  
(defun gensqlalchemy-buffer-name (ext &optional buffer)
  (let ((directory (directory-file-name (gensqlalchemy-specification-directory buffer)))
        name)
    (while (member (file-name-nondirectory directory) gensqlalchemy-common-directories)
      (setq directory (directory-file-name (file-name-directory default-directory))))
    (setq name (file-name-nondirectory directory))
    (when (string= name "")
      (setq name gensqlalchemy-specification-directory))
    (format "*%s:%s*" name ext)))

(defun gensqlalchemy-specification ()
  (save-excursion
    (let ((spec-regexp "^class +\\([a-zA-Z_0-9]+\\) *("))
      (goto-char (line-beginning-position))
      (unless (or (looking-at spec-regexp)
                  (re-search-backward spec-regexp nil t)
                  (re-search-forward spec-regexp nil t))
        (error "No specification found around point"))
      (match-string-no-properties 1))))
  
(defun gensqlalchemy-prepare-output-buffer (base-buffer erase)
  (let ((directory (gensqlalchemy-specification-directory base-buffer t)))
    (let ((output-buffer (pop-to-buffer (gensqlalchemy-buffer-name "sql" base-buffer))))
      (with-current-buffer output-buffer
        (when erase
          (erase-buffer))
        (gensqlalchemy-sql-mode)
        (setq default-directory directory))
      output-buffer)))

(defun gensqlalchemy-run-gsql (output-buffer &rest args)
  (apply 'call-process gensqlalchemy-gsql nil output-buffer t args))

(defun gensqlalchemy-eval (&optional dependencies)
  "Convert current specification to SQL and display the result.
If called With a prefix argument then show also dependent objects."
  (interactive "P")
  (gensqlalchemy-display t dependencies))

(defun gensqlalchemy-add (&optional dependencies)
  "Convert current specification to SQL and add it to the displayed SQL.
If called With a prefix argument then show also dependent objects."
  (interactive "P")
  (gensqlalchemy-display nil dependencies))

(defun gensqlalchemy-display (replace dependencies)
  (let* ((spec-name (gensqlalchemy-specification))
         (output-buffer (gensqlalchemy-prepare-output-buffer (current-buffer) replace))
         (args (append (unless dependencies
                         '("--no-deps"))
                       (list (format "--limit=^%s$" spec-name)
                             gensqlalchemy-specification-directory))))
    (unless replace
      (with-current-buffer output-buffer
        (unless (string= "" (buffer-substring-no-properties (point-min) (point-max)))
          (goto-char (point-max))
          (insert "\n"))))
    (apply 'gensqlalchemy-run-gsql output-buffer args)))

(defun gensqlalchemy-show-sql-buffer ()
  "Show the buffer with converted SQL output."
  (interactive)
  (let ((buffer (get-buffer (gensqlalchemy-buffer-name "sql"))))
    (when buffer
      (pop-to-buffer buffer))))

(defun gensqlalchemy-test ()
  "Try to run SQL commands from SQL output buffer.
The commands are wrapped in a transaction which is aborted at the end."
  (interactive)
  (let ((buffer (if (eq major-mode 'sql)
                    (current-buffer)
                  (get-buffer (gensqlalchemy-buffer-name "sql")))))
    (when buffer
      (with-current-buffer buffer
        (sql-send-string "begin;")
        (sql-send-buffer)
        (sql-send-string "rollback;")))))

(defvar gensqlalchemy-psql-def-commands
  '(("FUNCTION" . "\\df+")
    ("SCHEMA" . "\\dn+")
    ("SEQUENCE" . "\\ds+")
    ("TABLE" . "\\d+")
    ("TYPE" . "\\dT+")
    ("VIEW" . "\\d+")))
(defun gensqlalchemy-definition ()
  "Show database definition of the current specification."
  (interactive)
  (let ((objects '())
        (spec-name (gensqlalchemy-specification))
        (directory (gensqlalchemy-specification-directory nil t))
        (output-buffer nil))
    (with-temp-buffer
      (setq default-directory directory)
      (gensqlalchemy-run-gsql (current-buffer)
                              "--names" "--no-deps" (format "--limit=^%s$" spec-name)
                              gensqlalchemy-specification-directory)
      (goto-char (point-min))
      (while (looking-at "^\\([-a-zA-Z]+\\) \\(.*\\)$")
        (push (cons (match-string 1) (match-string 2)) objects)
        (goto-char (line-beginning-position 2))))
    (with-current-buffer (gensqlalchemy-buffer-name "sql")
      (mapc #'(lambda (spec)
                (destructuring-bind (kind . name) spec
                  (let ((command (cdr (assoc kind gensqlalchemy-psql-def-commands))))
                    (when command
                      (setq output-buffer sql-buffer)
                      (sql-send-string (format "%s %s" command name))))))
            objects))
    (if output-buffer
        (pop-to-buffer output-buffer)
      (message "Definition not found"))))

;;; Announce

(provide 'gensqlalchemy)

;;; gensqlalchemy.el ends here
