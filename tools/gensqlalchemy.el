;;; gensqlalchemy.el --- support for working with pytis database specifications

;; Copyright (C) 2012, 2013, 2014 Brailcom, o.p.s.

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


;; This file defines gensqlalchemy-mode which is a minor mode for working with
;; gensqlalchemy database specifications.  Although it is a common minor mode,
;; it makes sense only in Python major modes.

;; To enable the mode automatically in database specifications, you may want to
;; add the following lines to your Python mode hook function:
;;
;;   (when (and (buffer-file-name)
;;              (string-match "/db/dbdefs/" (buffer-file-name)))
;;     (gensqlalchemy-mode 1))
;;
;; Or you can enable it manually with `M-x gensqlalchemy-mode'.

;; The mode binds its commands to key sequences starting with prefix `C-c C-q'.
;; You can type `C-c C-q C-h' to get overview of the available key bindings.

;; The most basic command is `C-c C-q C-q' (`gensqlalchemy-eval') which creates
;; and displays the SQL code of the current specification object.  Its variant
;; `C-c C-q a' (`gensqlalchemy-add') adds the definition to already created SQL
;; code, this way you can compose an SQL update file for several objects.  With
;; a prefix argument the commands generate SQL code also for objects depending
;; on the current definition.  You can display the current SQL code anytime
;; using `C-c C-q z' (`gensqlalchemy-show-sql-buffer').

;; gensqlalchemy.el uses standard Emacs sql-mode means for communication with
;; the database.  To be able to work with a database, you must first create a
;; corresponding SQL buffer using standard Emacs commands such as
;; `M-x sql-postgres'.  gensqlalchemy then asks for that buffer name when it
;; needs to communicate with the database.

;; Once a database connection is available, you can test the generated SQL code
;; with `C-c C-q t' (`gensqlalchemy-test').  The code is executed in a separate
;; transaction and rollbacked afterwards.  gensqlalchemy.el actually never
;; changes the database itself, everything is executed in discarded
;; transactions.  Nevertheless you should always be careful and not to use
;; gensqlalchemy.el interactions on production databases.

;; An exception is the command `C-c C-q !' (`gensqlalchemy-execute') which
;; executes the current gensqlalchemy output buffer in the associated database
;; connection, as a single transaction.

;; You can view current object definition in the database using `C-c C-q s'
;; (`gensqlalchemy-show-definition').  With a prefix argument new definition of
;; the object is inserted into the database into a separate schema and then
;; displayed; it's some form of pretty formatting the SQL code and accompanying
;; it with additional information.  You can compare the original and new
;; definitions anytime using `C-c C-q =' (`gensqlalchemy-compare').  When you
;; want to view the changes in Ediff, use a prefix argument.  Note that the new
;; definition is put into a separate schema again.  You can also compare SELECT
;; outputs of new views and functions using `C-c C-q o'
;; (`gensqlalchemy-compare-outputs').

;; `C-c C-q u' (`gensqlalchemy-upgrade') generates an upgrade SQL script for
;; the current object, including dependent objects.

;; You can list all objects dependent on the current specification using
;; `C-c C-q i' (`gensqlalchemy-info').  The list includes source locations you
;; can visit by pressing RET or clicking on them.

;; Whenever you meet an error during gsql SQL conversion, you can use
;; `M-x gensqlalchemy-show-buffer' command to display the corresponding source
;; in specifications.  Some gensqlalchemy.el functions can do that for you
;; automatically when such an error occurs on their invocation.

;; When editing specifications of SQL or PL/pgSQL database functions stored in
;; external .sql files, you can directly jump to the given file with the
;; command `C-c C-q f' (`gensqlalchemy-sql-function-file').  After editing and
;; saving it you can kill the buffer and window with `C-x 4 0'.

;; The mode comes with reasonable default settings, but you may want to
;; customize them using `M-x customize-group RET gensqlalchemy RET'.  The most
;; important option is `gensqlalchemy-gsql' which needs to be set if gsql
;; utility is not present in standard PATH.

;; In order to make gsql work PYTHONPATH must be set properly.  You can either
;; set PYTHONPATH directly before starting Emacs or you can set
;; `gensqlalchemy-pythonpath' variable.  You can do so per each application by
;; setting it in .dir-locals.el in APPLICATION directory, e.g.
;;
;;   ((python-mode
;;     (gensqlalchemy-pythonpath . "/PATH/TO/APPLICATION/lib:/PATH/TO/pytis/lib:/PATH/TO/lcg/lib"))
;;    ("lib/APPLICATION/dbdefs" . ((python-mode . ((gensqlalchemy-mode 1))))))

(require 'cl)
(require 'compile)
(require 'etags)
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

(defcustom gensqlalchemy-default-line-limit 1000
  "Default LIMIT value in SELECT commands performed by SQLAlchemy."
  :group 'gensqlalchemy
  :type 'integer)

(defcustom gensqlalchemy-ignored-definition-schemas '("public")
  "List of schema names to discard in definition comparison."
  :group 'gensqlalchemy
  :type '(list string))

(defcustom gensqlalchemy-pythonpath nil
  "PYTHONPATH value to use when invoking gsql."
  :group 'gensqlalchemy
  :type 'string)

(defcustom gensqlalchemy-shrink-to-function nil
  "If non-nil, shrink specification window when displaying SQL function."
  :group 'gensqlalchemy
  :type 'boolean)

(defvar gensqlalchemy-specification-directory "dbdefs")
(defvar gensqlalchemy-common-directories '("lib" "dbdefs"))

(define-minor-mode gensqlalchemy-mode
  "Toggle gensqlalchemy mode.
Currently the mode just defines some key bindings."
  nil " gsql" '(("\C-c\C-qe" . gensqlalchemy-eval)
                ("\C-c\C-q\C-q" . gensqlalchemy-eval)
                ("\C-c\C-qa" . gensqlalchemy-add)
                ("\C-c\C-qd" . gensqlalchemy-dependencies)
                ("\C-c\C-qf" . gensqlalchemy-sql-function-file)
                ("\C-c\C-qi" . gensqlalchemy-info)
                ("\C-c\C-qo" . gensqlalchemy-compare-outputs)
                ("\C-c\C-qt" . gensqlalchemy-test)
                ("\C-c\C-qs" . gensqlalchemy-show-definition)
                ("\C-c\C-qu" . gensqlalchemy-upgrade)
                ("\C-c\C-qz" . gensqlalchemy-show-sql-buffer)
                ("\C-c\C-q=" . gensqlalchemy-compare)
                ("\C-c\C-q!" . gensqlalchemy-execute)
                ))

(defun gensqlalchemy-sql-mode ()
  (unless (eq major-mode 'sql-mode)
    (sql-mode)
    (sql-set-product 'postgres)))

(defun gensqlalchemy-specification-directory (&optional buffer require)
  (let ((directory (with-current-buffer (or buffer (current-buffer))
                     default-directory)))
    (while (and (not (string= directory "/"))
                (not (string= (file-name-nondirectory (directory-file-name directory))
                              gensqlalchemy-specification-directory)))
      (setq directory (file-name-directory (directory-file-name directory))))
    (when (and require (string= directory "/"))
      (error "Specification directory not found"))
    directory))

(defun gensqlalchemy-process-directory (&optional buffer require)
  (gensqlalchemy-specification-directory buffer require))
  
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
  (unless (file-exists-p gensqlalchemy-temp-directory)
    (make-directory gensqlalchemy-temp-directory))
  (concat gensqlalchemy-temp-directory file-name))

(defun gensqlalchemy-empty-file (file-name)
  (= (or (nth 7 (file-attributes file-name)) 0) 0))
  
(defun gensqlalchemy-prepare-output-buffer (base-buffer erase)
  (let ((directory (gensqlalchemy-process-directory base-buffer t))
        (buffer (gensqlalchemy-buffer-name "sql" base-buffer)))
    (with-current-buffer (get-buffer-create buffer)
      (when erase
        (erase-buffer))
      (gensqlalchemy-sql-mode)
      (setq default-directory directory))
    buffer))

(defun gensqlalchemy-module ()
  (let ((paths (split-string (or (getenv "PYTHONPATH") "") ":" t))
        (module nil)
        (directory (expand-file-name default-directory)))
    (while (and paths (not module))
      (let ((p (pop paths)))
        (unless (char-equal (aref p (1- (length p))) ?/)
          (setq p (concat p "/")))
        (when (string-prefix-p p directory)
          (setq module (replace-regexp-in-string
                        "/" "."
                        (substring directory (length p) -1))))))
    (unless module
      (error "Specification module for directory %s not found in PYTHONPATH \"%s\""
              directory (or (getenv "PYTHONPATH") "")))
    module))

(defmacro with-gensqlalchemy-pythonpath (&rest body)
  `(let ((pythonpath (getenv "PYTHONPATH")))
     (when gensqlalchemy-pythonpath
       (setenv "PYTHONPATH" gensqlalchemy-pythonpath))
     (unwind-protect (progn ,@body)
       (setenv "PYTHONPATH" pythonpath))))

(defun gensqlalchemy-run-gsql (&rest args)
  (apply 'call-process gensqlalchemy-gsql nil t nil args))

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
  (let ((buffer (gensqlalchemy-display t dependencies nil nil)))
    (when buffer
      (pop-to-buffer buffer))))

(defun gensqlalchemy-add (&optional dependencies)
  "Convert current specification to SQL and add it to the displayed SQL.
If called with a prefix argument then show dependent objects as well."
  (interactive "P")
  (let ((buffer (gensqlalchemy-display nil dependencies nil nil)))
    (when buffer
      (pop-to-buffer buffer))))

(defun gensqlalchemy-display (erase dependencies schema upgrade)
  (with-gensqlalchemy-pythonpath
    (let* ((spec-name (gensqlalchemy-specification))
           (output-buffer (gensqlalchemy-prepare-output-buffer (current-buffer) erase))
           (args (append (list (format "--pretty=%d" gensqlalchemy-pretty-output-level))
                         (when upgrade
                           '("--upgrade"))
                         (unless dependencies
                           '("--no-deps"))
                         (when schema
                           (list (concat "--schema=" schema)))
                         (list (format "--limit=^%s$" spec-name)
                               (gensqlalchemy-module)))))
      (save-some-buffers)
      (with-current-buffer output-buffer
        (when upgrade
          (with-gensqlalchemy-sql-buffer output-buffer
            (with-current-buffer sql-buffer
              (dolist (item `(("--database" ,sql-database)
                              ("--host" ,sql-server)
                              ("--port" ,sql-port)
                              ("--user" ,sql-user)))
                (destructuring-bind (option value) item
                  (unless (or (equal value "") (equal value 0))
                    (push (format "%s=%s" option value) args))))
              (when (and (string= (or sql-password) "")
                         (save-excursion
                           (goto-char (point-min))
                           (looking-at "^Password\\>")))
                (set (make-local-variable 'sql-password) (read-passwd "Database password: ")))
              (setenv "PGPASSWORD" sql-password))))
        (unless (or erase
                    (string= "" (buffer-substring-no-properties (point-min) (point-max))))
          (goto-char (point-max))
          (insert "\n"))
        (when schema
          (insert (format "create schema %s;\n" gensqlalchemy-temp-schema)))
        (apply 'gensqlalchemy-run-gsql args)
        (if (search-backward "Traceback (most recent call last):" nil t)
            (progn
              (pop-to-buffer output-buffer)
              (goto-char (point-max))
              (gensqlalchemy-show-error)
              nil)
          output-buffer)))))
  
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

(defmacro with-gensqlalchemy-transaction (commit-command &rest body)
  (let (($buffer (gensym)))
    `(let ((,$buffer (if (eq major-mode 'sql-mode)
                         (current-buffer)
                       (get-buffer (gensqlalchemy-buffer-name "sql")))))
       (when ,$buffer
         (with-gensqlalchemy-sql-buffer ,$buffer
           (sql-send-string "begin;")
           (unwind-protect (progn ,@body)
             (with-current-buffer ,$buffer
               (sql-send-string (concat ,commit-command ";")))))))))

(defmacro with-gensqlalchemy-rollback (&rest body)
  `(with-gensqlalchemy-transaction "rollback"
     ,@body))

(defmacro with-gensqlalchemy-commit (&rest body)
  `(with-gensqlalchemy-transaction "commit"
     ,@body))

(defmacro with-gensqlalchemy-log-file (file-name &rest body)
  `(progn
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

(defun gensqlalchemy-current-objects (&optional schema)
  (let ((spec-name (gensqlalchemy-specification))
        (objects '()))
    (with-gensqlalchemy-pythonpath
      (with-temp-buffer
        (setq default-directory (gensqlalchemy-process-directory nil t))
        (apply 'gensqlalchemy-run-gsql
               (append (list "--names" "--no-deps" (format "--limit=^%s$" spec-name))
                       (when schema
                         (list (concat "--schema=" schema)))
                       (list (gensqlalchemy-module))))
        (goto-char (point-min))
        (while (looking-at "^\\([-a-zA-Z]+\\) \\([^(\n]*\\)\\((.*)\\)?$")
          (push (list (match-string 1) (match-string 2) (match-string 3)) objects)
          (goto-char (line-beginning-position 2)))))
    objects))

(defvar gensqlalchemy-psql-def-commands
  '(("FUNCTION" . "\\sf+")
    ("SCHEMA" . "\\dn+")
    ("SEQUENCE" . "\\ds+")
    ("TABLE" . "\\d+")
    ("TYPE" . "\\dT+")
    ("VIEW" . "\\d+")))
(defun gensqlalchemy-definition (file-name &optional send-buffer schema simplify)
  (let ((objects (gensqlalchemy-current-objects schema))
        (output-buffer nil)
        (paths gensqlalchemy-ignored-definition-schemas)
        path-schema)
    (with-gensqlalchemy-rollback
      (when send-buffer
        (with-gensqlalchemy-sql-buffer send-buffer
          (gensqlalchemy-send-buffer)))
      (setq foo (cons 1 objects))
      (with-gensqlalchemy-log-file file-name
        (mapc #'(lambda (spec)
                  (destructuring-bind (kind name args) spec
                    (let ((path-schema nil)
                          (command (cdr (assoc kind gensqlalchemy-psql-def-commands))))
                      (when command
                        (if (string-match "^\\([^.]+\\)\\." name)
                            (setq path-schema (match-string 1 name)
                                  name (substring name (1+ (match-end 1))))
                          (setq path-schema (or schema "public")))
                        (add-to-list 'paths path-schema)
                        (sql-send-string (format "set search_path to \"%s\";" path-schema))
                        (setq output-buffer sql-buffer)
                        (when args
                          (setq name (concat name (replace-regexp-in-string "[^(),]+::" "" args))))
                        (sql-send-string (format "%s %s" command name))))))
              objects)))
    (gensqlalchemy-wait-for-outputs output-buffer)
    (when simplify
      (save-excursion
        (find-file file-name)
        (goto-char (point-min))
        (while (re-search-forward (format "\\<\\(%s\\)\\." (mapconcat #'identity paths "\\|"))
                                  nil t)
          (replace-match ""))
        (save-buffer)
        (kill-buffer)))
    output-buffer))

(defun gensqlalchemy-show-definition (&optional arg)
  "Show database definition of the current specification.
With an optional prefix argument show new definition of the specification."
  (interactive "P")
  (let ((file (gensqlalchemy-temp-file "def"))
        (buffer (and arg (gensqlalchemy-display t nil gensqlalchemy-temp-schema nil)))
        (schema (and arg gensqlalchemy-temp-schema)))
    (unless (and arg (not buffer))
      (gensqlalchemy-definition file buffer schema)
      (if (gensqlalchemy-empty-file file)
          (message "Definition not found")
        (let ((buffer (get-buffer-create (gensqlalchemy-buffer-name "def"))))
          (pop-to-buffer buffer)
          (erase-buffer)
          (insert-file-contents file))))))

(defun gensqlalchemy-test ()
  "Try to run SQL commands from SQL output buffer.
The commands are wrapped in a transaction which is aborted at the end."
  (interactive)
  (with-gensqlalchemy-rollback
    (gensqlalchemy-send-buffer)))

(defun gensqlalchemy-execute ()
  "Run SQL commands from SQL output buffer.
The commands are wrapped in a transaction so they are either all executed
successfully or all the changes are rolled back."
  (interactive)
  (with-gensqlalchemy-commit
    (gensqlalchemy-send-buffer)))

(defun gensqlalchemy-compare (&optional arg)
  "Compare current specification with the definition in the database.
With an optional prefix argument show the differences in Ediff."
  (interactive "P")
  (let ((buffer (gensqlalchemy-display t nil gensqlalchemy-temp-schema nil))
        (old-def-file (gensqlalchemy-temp-file "olddef"))
        (new-def-file (gensqlalchemy-temp-file "newdef")))
    (when buffer
      (gensqlalchemy-definition old-def-file nil nil t)
      (gensqlalchemy-definition new-def-file buffer gensqlalchemy-temp-schema t)
      (if arg
          (ediff-files old-def-file new-def-file)
        (diff old-def-file new-def-file "-u")))))

(defvar gensqlalchemy-last-function-arguments nil)
(defun gensqlalchemy-select (object file n use-last-arguments order-by)
  (destructuring-bind (kind name args) object
    (unless (member kind '("TABLE" "VIEW" "FUNCTION"))
      (error "Unable to run SELECT on %s" kind))
    (let ((command (concat "SELECT * FROM " name)))
      (when args
        (unless use-last-arguments
          (setq gensqlalchemy-last-function-arguments
                (read-string (format "Function arguments %s: " args)
                             gensqlalchemy-last-function-arguments)))
        (setq command (concat command "(" gensqlalchemy-last-function-arguments ")")))
      (unless (string= order-by "")
        (setq command (concat command " ORDER BY " order-by)))
      (when n
        (setq command (concat command " LIMIT " (number-to-string n))))
      (setq command (concat command ";"))
      (with-gensqlalchemy-log-file file
        (sql-send-string command)))))

(defvar gensqlalchemy-last-order-by "")
(defun gensqlalchemy-compare-outputs (&optional n)
  "Compare SELECT outputs of the original and new definition.
By default compare at most `gensqlalchemy-default-line-limit' lines of output.
With a numeric prefix argument compare that many lines of output.
With a universal prefix argument compare complete outputs."
  (interactive "P")
  (cond
   ((null n)
    (setq n gensqlalchemy-default-line-limit))
   ((consp n)
    (setq n nil)))
  (when (and (numberp n) (< n 0))
    (setq n 0))
  (let ((old-object (car (gensqlalchemy-current-objects)))
        (new-object (car (gensqlalchemy-current-objects gensqlalchemy-temp-schema)))
        (old-data-file (gensqlalchemy-temp-file "olddata"))
        (new-data-file (gensqlalchemy-temp-file "newdata"))
        (new-buffer (gensqlalchemy-display t nil gensqlalchemy-temp-schema nil)))
    (when new-buffer
      (setq gensqlalchemy-last-order-by
            (read-string "Order by: " gensqlalchemy-last-order-by))
      (with-gensqlalchemy-rollback
        (gensqlalchemy-select old-object old-data-file n nil gensqlalchemy-last-order-by))
      (with-gensqlalchemy-rollback
        (with-gensqlalchemy-sql-buffer new-buffer
          (gensqlalchemy-send-buffer))
        (gensqlalchemy-select new-object new-data-file n t gensqlalchemy-last-order-by))
      (gensqlalchemy-wait-for-outputs (with-current-buffer new-buffer sql-buffer))
      (diff old-data-file new-data-file))))

(defun gensqlalchemy-upgrade ()
  "Generate an upgrade SQL script for the current object."
  (interactive)
  (save-some-buffers)
  (let ((buffer (gensqlalchemy-display t t nil t)))
    (when buffer
      (pop-to-buffer buffer))))

(defun gensqlalchemy-info ()
  "Show info about current specification.
Currently it prints basic information about this object and all dependent
objects."
  (interactive)
  (let ((spec-name (gensqlalchemy-specification))
        (default-directory (gensqlalchemy-process-directory nil t)))
    (with-gensqlalchemy-pythonpath
      (compilation-start (format "PYTHONPATH='%s' %s --names --source --limit='^%s$' %s"
                                 (or (getenv "PYTHONPATH") "")
                                 gensqlalchemy-gsql spec-name (gensqlalchemy-module))))))

(defun gensqlalchemy-sql-function-file ()
  "Visit SQL file associated with current function."
  (interactive)
  (let ((name (with-gensqlachemy-specification
               (unless (re-search-forward "^    name = ['\"]\\(.*\\)['\"]" nil t)
                 (error "Function name not found"))
               (match-string 1))))
    (when gensqlalchemy-shrink-to-function
      (split-window-below)
      (save-match-data
        (save-excursion
          (save-restriction
            (unless (looking-at "^class ")
              (python-nav-backward-up-list 9))
            (goto-char (line-end-position))
            (narrow-to-defun)
            (sit-for 0)
            (shrink-window-if-larger-than-buffer)
            (widen)))))
    (find-file-other-window (concat "sql/" (match-string 1) ".sql"))))

(defun gensqlalchemy-find-object (name)
  (let ((dir (gensqlalchemy-specification-directory))
        (class-regexp (concat "^class +" (regexp-quote name) "\\>")))
    (flet ((spec-buffer-p (buffer)
             (let ((file (or (buffer-file-name buffer) "")))
               (or (string-prefix-p dir file)
                   (string-match "/pytis/db/dbdefs/" file))))
           (look-for-tag (name next-p)
             (let ((buffer (ignore-errors (find-tag-noselect name next-p))))
               (when buffer
                 (with-current-buffer buffer
                   (when (looking-at (concat "class +" (regexp-quote name) "\\>"))
                     buffer))))))
      (save-excursion
        (if (progn
              (goto-char (point-min))
              (re-search-forward class-regexp nil t))
            (list name (file-name-nondirectory (buffer-file-name)) (line-beginning-position))
          (let ((buffer (look-for-tag name nil))
                (pytis-buffer-p nil))
            (while (and buffer (not (spec-buffer-p buffer)))
              (setq buffer (look-for-tag name t)))
            (if buffer
                (with-current-buffer buffer
                  (list name (file-name-nondirectory (buffer-file-name buffer)) (point)))
              (with-current-buffer (find-file-noselect (concat dir "__init__.py"))
                (goto-char (point-min))
                (let ((result nil))
                  (while (and (not result)
                              (re-search-forward "^from +\\([a-zA-Z0-9_]+\\) +import " nil t))
                    (let ((file (concat dir (match-string-no-properties 1) ".py")))
                      (when (file-readable-p file)
                        (with-temp-buffer
                          (insert-file-contents file)
                          (goto-char (point-min))
                          (when (re-search-forward class-regexp nil t)
                            (setq result (list name file (line-beginning-position))))))))
                  (unless result
                    (error "Object %s not found" name))
                  result)))))))))
  
(defun gensqlalchemy-dependencies ()
  "Show information about dependencies of the current specification.
Currently __init__.py file is shown with the point at the first imported file
where the specification may be put without breaking dependencies."
  (interactive)
  (let ((beg nil)
        (end nil)
        (object-regexp nil)
        (hard-objects '())
        (soft-objects '())
        (hard-dependencies '())
        (current-file (file-name-nondirectory (or (buffer-file-name) "")))
        (dir (gensqlalchemy-specification-directory)))
    (save-some-buffers)
    (save-excursion
      (save-restriction
        (while (not beg)
          (if (re-search-backward "^class " nil t)
              (setq beg (and (not (eq (get-text-property (point) 'face) 'font-lock-string-face))
                             (point)))
            (setq beg (point-min))))
        (goto-char (1+ beg))
        (while (not end)
          (if (re-search-forward "^class " nil t)
              (setq end (and (not (eq (get-text-property (point) 'face) 'font-lock-string-face))
                             (line-beginning-position)))
            (setq end (point-max))))
        (goto-char (point-min))
        (let ((import-string (format "from %s import " (with-gensqlalchemy-pythonpath
                                                         (gensqlalchemy-module))))
              (imported-objects ""))
          (while (search-forward import-string nil t)
            (setq imported-objects
                  (concat imported-objects "\\|"
                          (mapconcat #'identity
                                     (split-string
                                      (buffer-substring-no-properties (point) (line-end-position))
                                      " *[ ,] *" t)
                                     "\\|"))))
          (setq object-regexp (format "\\<\\(\\(sql\\.[ct]\\.\\([a-zA-Z0-9_]+\\)\\)%s\\)\\>"
                                      imported-objects)))
        (narrow-to-region beg end)
        (goto-char (point-min))
        (while (re-search-forward object-regexp nil t)
          (let ((object (or (match-string-no-properties 3) (match-string-no-properties 1)))
                (point (point)))
            (re-search-backward "^\\(class\\|    [a-zA-Z_]\\)")
            (push object (if (looking-at "    def ") soft-objects hard-objects))
            (goto-char point))))
      (setq soft-objects (set-difference soft-objects hard-objects :test #'string=)) ; unused now
      (setq hard-dependencies (mapcar #'gensqlalchemy-find-object (sort hard-objects #'string<))))
    (let ((files (mapcar #'file-name-nondirectory
                         (remove-if #'(lambda (file) (not (string-prefix-p dir file)))
                                    (mapcar #'second hard-dependencies)))))
      (find-file-other-window (concat dir "__init__.py"))
      (goto-char (point-min))
      (while (and files (re-search-forward "^from +\\([a-zA-Z0-9_]+\\) +import " nil t))
        (setq files (remove (concat (match-string-no-properties 1) ".py") files)))
      (goto-char (line-beginning-position 2)))))

(defun gensqlalchemy-show-error ()
  "Try to show specification error from the last traceback in current buffer."
  (interactive)
  (let ((regexp "^  File \"\\(.*/dbdefs/.*\\.py\\)\", line \\([0-9]+\\)\\(, in .*$\\)?"))
    (when (save-excursion
            (or (re-search-backward regexp nil t)
                (re-search-forward regexp nil t)))
      (let ((file (match-string-no-properties 1))
            (line (match-string-no-properties 2))
            (overlay (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put overlay 'face 'highlight)
        (find-file-other-window file)
        (goto-char 1)
        (forward-line (1- (car (read-from-string line))))))))

(defun gensqlalchemy-class->name (class-name)
  "Return default database name corresponding to CLASS-NAME.
This is useful in snippets."
  (let ((case-fold-search nil)
        (start 0)
        (end 1)
        (length (length class-name))
        (components '()))
    (while (< end length)
      (when (get-char-code-property (aref class-name end) 'lowercase) ; upper case letter
        (push (substring class-name start end) components)
        (setq start end))
      (incf end))
    (unless (= start length)
      (push (substring class-name start length) components))
    (mapconcat #'downcase (nreverse components) "_")))

;;; Announce

(provide 'gensqlalchemy)

;;; gensqlalchemy.el ends here
