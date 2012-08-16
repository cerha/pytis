;;; dump-compare.el --- compare PostgreSQL dumps

;; Copyright (C) 2012 Brailcom, o.p.s.

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
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(defun dump-value (string)
  (when (string-match "LANGUAGE plpythonu" string)
    (setq string ""))
  (while (string-match "\\(^[ 	]+\\|[ 	]+$\\|\n--$\\)" string)
    (setq string (replace-match "" nil nil string)))
  (while (string-match "^\\(SET search_path = \\|GRANT \\|REVOKE \\|ALTER .* OWNER TO \\|#def \\).*$" string)
    (setq string (replace-match "" nil nil string)))
  (while (string-match "; \\(Owner\\|Tablespace\\):.*$" string)
    (setq string (replace-match "" nil nil string)))
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

(defun dump-read-data (file)
  (save-excursion
    (let ((buffer (find-file-noselect file))
          (data (make-hash-table :test 'equal))
          (next-regexp "^\\(-- Name: \\|SET default\\)")
          (defaults '()))
      (set-buffer buffer)
      (goto-char (point-min))
      (while (re-search-forward next-regexp nil t)
        (let ((beg (goto-char (line-beginning-position)))
              (key nil))
          (if (looking-at "SET")
              (progn
                (when (looking-at "SET default_with_oids")
                  (setq defaults (plist-put defaults 'oids (buffer-substring (point) (line-end-position)))))
                (forward-line))
            (let (name type schema)
              (when (re-search-forward "^-- Name: \\([^;]+\\); Type: \\([^;]+\\); Schema: \\([-a-zA-Z_]+\\)" nil t)
                (setq name (match-string 1)
                      type (match-string 2)
                      schema (match-string 3))
                (setq key (concat type "/" schema "/" name)))))
          (goto-char (if (re-search-forward next-regexp nil t) (line-beginning-position) (point-max)))
          (when (and key (or (string= name "_changes") (not (string-match "^\\(_\\|sogo[a-z]\\)" name))))
            (puthash key (dump-value (buffer-substring-no-properties beg (point))) data))))
      data)))

(defun dump-compare (file-1 file-2)
  (interactive "fFile 1: \nfFile 2: ")
  (let ((data-1 (dump-read-data file-1))
        (data-2 (dump-read-data file-2)))
    (dump-compare-data data-1 data-2 file-1 file-2 )))

(provide 'dump-compare)
