;;; gensqlalchemy.el --- support for working with pytis database specifications

;; Copyright (C) 2012, 2013 Brailcom, o.p.s.

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


;; To enable the mode automatically in database specifications, you may want to
;; add the following to your Python mode hook function:
;;
;;   (when (and (buffer-file-name)
;;              (string-match "/db/dbdefs/" (buffer-file-name)))
;;     (gensqlalchemy-mode 1))


(require 'cl)
(require 'compile)
(require 'python)
(require 'sql)

(defgroup gensqlalchemy nil
  "Emacs support for gensqlalchemy editing."
  :group 'python)

(defcustom gensqlalchemy-gsql "gsql"
  "gsql binary."
  :group 'gensqlalchemy
  :type 'string)

(defcustom gensqlalchemy-pretty-output-level 1
  "Value for `--pretty' gsql command line option."
  :group 'gensqlalchemy
  :type 'integer)

(defcustom gensqlalchemy-temp-schema (format "%s_temp" user-login-name)
  "Schema to use for SQL code testing."
  :group 'gensqlalchemy
  :type 'string)

(defcustom gensqlalchemy-temp-directory (concat temporary-file-directory "gensqlalchemy-el/")
  "Directory to use for storing miscellaneous temporary files."
  :group 'gensqlalchemy
  :type 'directory)

(defvar gensqlalchemy-specification-directory "dbdefs")
(defvar gensqlalchemy-common-directories '("lib" "db"))

(define-minor-mode gensqlalchemy-mode
  "Toggle gensqlalchemy mode.
Currently the mode just defines some key bindings."
  nil " gsql" '(("\C-c\C-qe" . gensqlalchemy-eval)
                ("\C-c\C-q\C-q" . gensqlalchemy-eval)
                ("\C-c\C-qa" . gensqlalchemy-add)
                ("\C-c\C-qd" . gensqlalchemy-show-definition)
                ("\C-c\C-qf" . gensqlalchemy-sql-function-file)
                ("\C-c\C-qi" . gensqlalchemy-info)
                ("\C-c\C-qs" . gensqlalchemy-show-sql-buffer)
                ("\C-c\C-qt" . gensqlalchemy-test)
                ("\C-c\C-q=" . gensqlalchemy-compare)
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
      (setq directory (directory-file-name (file-name-directory directory))))
    (setq name (file-name-nondirectory directory))
    (when (string= name "")
      (setq name gensqlalchemy-specification-directory))
    (format "*%s:%s*" name ext)))

(defmacro with-gensqlachemy-specification (&rest body)
  (let (($spec-regexp (gensym))
        ($point (gensym)))
    `(save-excursion
       (let ((,$spec-regexp "^class +\\([a-zA-Z_0-9]+\\) *("))
         (goto-char (line-beginning-position))
         (unless (or (looking-at ,$spec-regexp)
                     (re-search-backward ,$spec-regexp nil t)
                     (re-search-forward ,$spec-regexp nil t))
           (error "No specification found around point"))
         (goto-char (line-beginning-position))
         (let ((specification-name (match-string-no-properties 1))
               (,$point (point)))
           (forward-char)
           (unless (re-search-forward ,$spec-regexp nil t)
             (goto-char (point-max)))
           (save-restriction
             (narrow-to-region ,$point (point))
             (goto-char ,$point)
             ,@body))))))

(defun gensqlalchemy-specification ()
  (with-gensqlachemy-specification
    specification-name))

(defun gensqlalchemy-temp-file (file-name)
  (concat gensqlalchemy-temp-directory file-name))

(defun gensqlalchemy-empty-file (file-name)
  (= (or (nth 7 (file-attributes file-name)) 0) 0))
  
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

(defun gensqlalchemy-send-buffer ()
  "Send the buffer contents to the SQL process via file.
This is useful for longer inputs where the input may break in comint."
  (let ((file (gensqlalchemy-temp-file "input")))
    (write-region (point-min) (point-max) file)
    (sql-send-string (concat "\\i " file))))

(defun gensqlalchemy-eval (&optional dependencies)
  "Convert current specification to SQL and display the result.
If called with a prefix argument then show dependent objects as well."
  (interactive "P")
  (gensqlalchemy-display t dependencies nil))

(defun gensqlalchemy-add (&optional dependencies)
  "Convert current specification to SQL and add it to the displayed SQL.
If called with a prefix argument then show dependent objects as well."
  (interactive "P")
  (gensqlalchemy-display nil dependencies nil))

(defun gensqlalchemy-display (replace dependencies schema)
  (let* ((spec-name (gensqlalchemy-specification))
         (output-buffer (gensqlalchemy-prepare-output-buffer (current-buffer) replace))
         (args (append (list (format "--pretty=%d" gensqlalchemy-pretty-output-level))
                       (unless dependencies
                         '("--no-deps"))
                       (when schema
                         (list (concat "--schema=" schema)))
                       (list (format "--limit=^%s$" spec-name)
                             gensqlalchemy-specification-directory))))
    (save-some-buffers)
    (unless replace
      (with-current-buffer output-buffer
        (unless (string= "" (buffer-substring-no-properties (point-min) (point-max)))
          (goto-char (point-max))
          (insert "\n"))))
    (when schema
      (insert (format "create schema %s;\n" gensqlalchemy-temp-schema)))
    (apply 'gensqlalchemy-run-gsql output-buffer args)
    output-buffer))
  
(defun gensqlalchemy-show-sql-buffer ()
  "Show the buffer with converted SQL output."
  (interactive)
  (let ((buffer (get-buffer (gensqlalchemy-buffer-name "sql"))))
    (when buffer
      (pop-to-buffer buffer))))

(defmacro with-gensqlalchemy-sql-buffer (buffer &rest body)
  (let (($buffer buffer))
    `(with-current-buffer ,$buffer
       (unless sql-buffer
         (sql-set-sqli-buffer))
       ,@body)))

(defmacro with-gensqlalchemy-rollback (&rest body)
  (let (($buffer (gensym)))
    `(let ((,$buffer (if (eq major-mode 'sql-mode)
                         (current-buffer)
                       (get-buffer (gensqlalchemy-buffer-name "sql")))))
       (when ,$buffer
         (with-gensqlalchemy-sql-buffer ,$buffer
           (sql-send-string "begin;")
           (unwind-protect (progn ,@body)
             (with-current-buffer ,$buffer
               (sql-send-string "rollback;"))))))))

(defmacro with-gensqlalchemy-log-file (file-name &rest body)
  `(progn
     (unless (file-exists-p gensqlalchemy-temp-directory)
       (make-directory gensqlalchemy-temp-directory))
     (sql-send-string (concat "\\o " ,file-name))
     ,@body
     (sql-send-string "\\o")))

(defun gensqlalchemy-wait-for-outputs (sql-buffer)
  (let ((terminator "gensqlalchemy-el-terminator")
        (n 100))
    (with-current-buffer sql-buffer
      (let ((point (point-max)))
        (goto-char point)
        (sql-send-string (format "select '%s';" terminator))
        (while (and (> n 0) (not (re-search-forward terminator nil t)))
          (decf n)
          (sit-for 0.1)
          (goto-char point))))))

(defvar gensqlalchemy-psql-def-commands
  '(("FUNCTION" . "\\df+")
    ("SCHEMA" . "\\dn+")
    ("SEQUENCE" . "\\ds+")
    ("TABLE" . "\\d+")
    ("TYPE" . "\\dT+")
    ("VIEW" . "\\d+")))
(defun gensqlalchemy-definition (file-name &optional send-buffer schema)
  (let ((objects '())
        (spec-name (gensqlalchemy-specification))
        (directory (gensqlalchemy-specification-directory nil t))
        (output-buffer nil))
    (with-temp-buffer
      (setq default-directory directory)
      (apply 'gensqlalchemy-run-gsql
             (current-buffer)
             (append (list "--names" "--no-deps" (format "--limit=^%s$" spec-name))
                     (when schema
                       (list (concat "--schema=" schema)))
                     (list gensqlalchemy-specification-directory)))
      (goto-char (point-min))
      (while (looking-at "^\\([-a-zA-Z]+\\) \\(.*\\)$")
        (push (list (match-string 1) (match-string 2)) objects)
        (goto-char (line-beginning-position 2))))
    (with-gensqlalchemy-rollback
      (when send-buffer
        (with-gensqlalchemy-sql-buffer send-buffer
          (gensqlalchemy-send-buffer)))
      (sql-send-string "set search_path to public;")
      (with-gensqlalchemy-log-file file-name
        (mapc #'(lambda (spec)
                  (destructuring-bind (kind name) spec
                    (let ((command (cdr (assoc kind gensqlalchemy-psql-def-commands))))
                      (when command
                        (setq output-buffer sql-buffer)
                        (sql-send-string (format "%s %s" command name))))))
              objects)))
    output-buffer))
        
(defun gensqlalchemy-show-definition (&optional arg)
  "Show database definition of the current specification.
With an optional prefix argument show new definition of the specification."
  (interactive "P")
  (let ((file (gensqlalchemy-temp-file "def"))
        (buffer (and arg (save-window-excursion
                           (gensqlalchemy-display t nil gensqlalchemy-temp-schema))))
        (schema (and arg gensqlalchemy-temp-schema)))
    (gensqlalchemy-wait-for-outputs (gensqlalchemy-definition file buffer schema))
    (if (gensqlalchemy-empty-file file)
        (message "Definition not found")
      (let ((buffer (get-buffer-create (gensqlalchemy-buffer-name "def"))))
        (pop-to-buffer buffer)
        (erase-buffer)
        (insert-file-contents file)))))

(defun gensqlalchemy-test ()
  "Try to run SQL commands from SQL output buffer.
The commands are wrapped in a transaction which is aborted at the end."
  (interactive)
  (with-gensqlalchemy-rollback
    (gensqlalchemy-send-buffer)))

(defun gensqlalchemy-compare (&optional arg)
  "Compare current specification with the definition in the database.
With an optional prefix argument show the differences in Ediff."
  (interactive "P")
  (let ((buffer (save-excursion (gensqlalchemy-display t nil gensqlalchemy-temp-schema)))
        (old-def-file (gensqlalchemy-temp-file "olddef"))
        (new-def-file (gensqlalchemy-temp-file "newdef")))
    (gensqlalchemy-definition old-def-file)
    (gensqlalchemy-definition new-def-file buffer gensqlalchemy-temp-schema)
    (gensqlalchemy-wait-for-outputs (with-current-buffer buffer sql-buffer))
    (if arg
        (ediff-files old-def-file new-def-file)
      (diff old-def-file new-def-file "-u"))))

(defun gensqlalchemy-info ()
  "Show info about current specification.
Currently it prints basic information about this object and all dependent
objects."
  (interactive)
  (let ((spec-name (gensqlalchemy-specification))
        (default-directory (gensqlalchemy-specification-directory nil t)))
    (compilation-start (format "%s --names --source --limit='^%s$' %s"
                               gensqlalchemy-gsql spec-name gensqlalchemy-specification-directory))))

        (directory (gensqlalchemy-specification-directory nil t)))
    (pop-to-buffer (get-buffer-create (gensqlalchemy-buffer-name "info"))) 
    (setq default-directory directory)
    (erase-buffer)
    (gensqlalchemy-run-gsql (current-buffer)
                            "--names" "--source" (format "--limit=^%s$" spec-name)
                            gensqlalchemy-specification-directory)))

(defun gensqlalchemy-sql-function-file ()
  "Visit SQL file associated with current function."
  (interactive)
  (let ((name (with-gensqlachemy-specification
               (unless (re-search-forward "^    name = ['\"]\\(.*\\)['\"]" nil t)
                 (error "Function name not found"))
               (match-string 1))))
    (find-file-other-window (concat "sql/" (match-string 1) ".sql"))))

(defun gensqlalchemy-show-error ()
  "Try to show specification error from the last traceback in current buffer."
  (interactive)
  (let ((regexp "^  File \"\\(.*/dbdefs/.*\\.py\\)\", line \\([0-9]+\\), in "))
    (when (or (re-search-backward regexp nil t)
              (re-search-forward regexp nil t))
      (let ((file (match-string-no-properties 1))
            (line (match-string-no-properties 2)))
        (find-file-other-window file)
        (goto-line (car (read-from-string line)))))))

;;; Announce

(provide 'gensqlalchemy)

;;; gensqlalchemy.el ends here
