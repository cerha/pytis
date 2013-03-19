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

(defvar gensqlalchemy-specification-directory "dbdefs")
(defvar gensqlalchemy-common-directories '("lib" "db"))

(define-minor-mode gensqlalchemy-mode
  "Toggle gensqlalchemy mode.
Currently the mode just defines some key bindings."
  nil " GQ" '(("\C-c\C-qe" . gensqlalchemy-eval)
              ("\C-c\C-q\C-q" . gensqlalchemy-eval)
              ("\C-c\C-qa" . gensqlalchemy-add)
              ("\C-c\C-qd" . gensqlalchemy-definition)
              ("\C-c\C-qf" . gensqlalchemy-sql-function-file)
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
If called with a prefix argument then show dependent objects as well."
  (interactive "P")
  (gensqlalchemy-display t dependencies))

(defun gensqlalchemy-add (&optional dependencies)
  "Convert current specification to SQL and add it to the displayed SQL.
If called with a prefix argument then show dependent objects as well."
  (interactive "P")
  (gensqlalchemy-display nil dependencies))

(defun gensqlalchemy-display (replace dependencies)
  (let* ((spec-name (gensqlalchemy-specification))
         (output-buffer (gensqlalchemy-prepare-output-buffer (current-buffer) replace))
         (args (append (list (format "--pretty=%d" gensqlalchemy-pretty-output-level))
                       (unless dependencies
                         '("--no-deps"))
                       (list (format "--limit=^%s$" spec-name)
                             gensqlalchemy-specification-directory))))
    (save-some-buffers)
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
    (with-current-buffer (get-buffer-create (gensqlalchemy-buffer-name "sql"))
      (mapc #'(lambda (spec)
                (destructuring-bind (kind . name) spec
                  (let ((command (cdr (assoc kind gensqlalchemy-psql-def-commands))))
                    (when command
                      (unless sql-buffer
                        (sql-set-sqli-buffer))
                      (setq output-buffer sql-buffer)
                      (sql-send-string (format "%s %s" command name))))))
            objects))
    (if output-buffer
        (pop-to-buffer output-buffer)
      (message "Definition not found"))))

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
