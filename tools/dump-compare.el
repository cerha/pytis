;;; dump-compare.el --- compare PostgreSQL dumps

;; Copyright (C) 2012, 2013 OUI Technology Ltd.

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defun dump-value (string no-plpy)
  (when (and no-plpy (string-match "LANGUAGE plpythonu" string))
    (setq string ""))
  (while (string-match "\\(^[ 	]+\\|[ 	]+$\\|\n--$\\)" string)
    (setq string (replace-match "" nil nil string)))
  (while (string-match "^\\(SET search_path = \\|GRANT \\|REVOKE \\|ALTER .* OWNER TO \\|#def \\).*$" string)
    (setq string (replace-match "" nil nil string)))
  (while (string-match "; \\(Owner\\|Tablespace\\):.*$" string)
    (setq string (replace-match "" nil nil string)))
  (while (string-match "CREATE \\(UNIQUE \\)?INDEX \\([a-zA-Z0-9_]+\\) " string)
    (setq string (replace-match "..." nil nil string 2)))
  (while (string-match "_trigger__\\(after\\|before\\)\\>" string)
    (setq string (replace-match "" nil nil string)))
  (while (string-match "__\\(ins\\|upd\\|del\\)\\(ert\\|ete\\|ate\\)_instead\\>" string)
    (setq string (replace-match "_\\1" nil nil string)))
  (while (string-match "\\(CREATE TRIGGER [a-z0-9_]+_upd_log AFTER \\)INSERT OR DELETE OR UPDATE ON " string)
    (setq string (replace-match "\\1UPDATE ON " nil nil string)))
  (while (string-match "Name: \\([a-zA-Z0-9_]+\\); Type: INDEX" string)
    (setq string (replace-match "..." nil nil string 1)))
  (while (string-match "\\$_?\\$\\( *\\)[a-zA-Z]" string)
    (setq string (replace-match "\n" nil nil string 1)))
  (while (string-match "[^\n ]\\( *\\$\\)_?\\$" string)
    (setq string (replace-match "\n$" nil nil string 1)))
  (while (string-match "\\\\n" string)
    (setq string (replace-match "\n" nil nil string)))
  (while (string-match "\n\n+" string)
    (setq string (replace-match "\n" nil nil string)))
  (when (string-match "^CREATE TABLE " string)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (re-search-forward "^CREATE TABLE ")
      (search-forward "(")
      (forward-char -1)
      (let ((beg (line-beginning-position 2)))
        (forward-sexp)
        (forward-char -3)
        (unless (= (following-char) ?,)
          (forward-char)
          (insert ","))
        (sort-lines nil beg (line-beginning-position 2)))
      (setq string (buffer-substring (point-min) (point-max)))))
  (when (string-match "^COPY " string)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward "\t\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)\t" nil t)
        (replace-match "0000-00-00 00:00:00" nil nil nil 1))
      (setq string (buffer-substring (point-min) (point-max)))))
  string)

(defun dump-compare-data (data-1 data-2 file-1 file-2)
  (save-excursion
    (let ((buffer (find-file-noselect (concat file-2 "-missing.sql"))))
      (set-buffer buffer)
      (erase-buffer)
      (maphash #'(lambda (key value)
                   (unless (gethash key data-2)
                     (insert key "\n")))
               data-1)
      (save-buffer))
    (let ((buffer (find-file-noselect (concat file-2 "-new.sql"))))
      (set-buffer buffer)
      (erase-buffer)
      (maphash #'(lambda (key value)
                   (unless (gethash key data-1)
                     (insert key "\n")))
               data-2)
      (save-buffer))
    (let ((buffer-1 (find-file-noselect (concat file-1 "-changes.sql")))
          (buffer-2 (find-file-noselect (concat file-2 "-changes.sql"))))
      (set-buffer buffer-1)
      (erase-buffer)
      (set-buffer buffer-2)
      (erase-buffer)
      (maphash #'(lambda (key old-value)
                   (let ((new-value (gethash key data-2)))
                     (when (and new-value (not (equal old-value new-value)))
                       (set-buffer buffer-1)
                       (insert "-- ** " key "\n")
                       (insert old-value "\n")
                       (set-buffer buffer-2)
                       (insert "-- ** " key "\n")
                       (insert new-value "\n"))))
               data-1)
      (set-buffer buffer-1)
      (save-buffer)
      (set-buffer buffer-2)
      (save-buffer))))

(defun dump-read-data (file no-plpy)
  (save-excursion
    (let ((buffer (find-file-noselect file))
          (data (make-hash-table :test 'equal))
          (next-regexp "^\\(-- Name: \\|SET default\\)")
          (defaults '()))
      (set-buffer buffer)
      (goto-char (point-min))
      (while (re-search-forward next-regexp nil t)
        (let ((beg (goto-char (line-beginning-position)))
              (key nil)
              (name "")
              (type "")
              (schema ""))
          (if (looking-at "SET")
              (progn
                (when (looking-at "SET default_with_oids")
                  (setq defaults (plist-put defaults 'oids (buffer-substring (point) (line-end-position)))))
                (forward-line))
            (when (re-search-forward "^-- Name: \\([^;]+\\); Type: \\([^;]+\\); Schema: \\([-a-zA-Z_]+\\)" nil t)
              (setq name (match-string 1)
                    type (match-string 2)
                    schema (match-string 3))
              (when (and (string= type "INDEX")
                         (save-excursion
                           (re-search-forward
                            "CREATE \\(UNIQUE \\)?INDEX [a-zA-Z0-9_]+ ON \\([a-zA-Z0-9_]+\\) .*(\\(.+\\))"
                            nil t)))
                (setq name (concat (match-string 2) "+" (match-string 3))))
              (when (and (string= type "TRIGGER")
                         (string-match "_trigger__\\(after\\|before\\)" name))
                (setq name (replace-match "" nil nil name)))
              (when (and (string= type "RULE")
                         (string-match "_\\(ins\\|upd\\|del\\)\\(ert\\|ete\\|ate\\)_instead$" name))
                (setq name (replace-match "\\1" nil nil name)))
              (setq key (concat type "/" schema "/" name))))
          (goto-char (if (re-search-forward next-regexp nil t) (line-beginning-position) (point-max)))
          (when (and key
                     (or (string= name "_changes") (not (string-match "^_" name)))
                     (not (member schema '("davical_app" "nobackup" "sogo")))
                     (not (and (string= type "TRIGGER") (string-match "_\\(ins\\|del\\)_log$" name))))
            (puthash key (dump-value (buffer-substring-no-properties beg (point)) no-plpy) data))))
      data)))

(defun dump-compare (file-1 file-2 &optional no-plpy)
  (interactive "fFile 1: \nfFile 2: \nP")
  (let ((data-1 (dump-read-data file-1 no-plpy))
        (data-2 (dump-read-data file-2 no-plpy)))
    (dump-compare-data data-1 data-2 file-1 file-2)))

(provide 'dump-compare)
