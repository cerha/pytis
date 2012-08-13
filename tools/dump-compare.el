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
  (while (string-match "^[ 	]+" string)
    (setq string (replace-match "" nil nil string)))
  (while (string-match "^\\(SET search_path = \\|GRANT \\|REVOKE \\|ALTER .* OWNER TO \\).*$" string)
    (setq string (replace-match "" nil nil string)))
  (while (string-match "\\(; Owner: .*\\)$" string)
    (setq string (replace-match "" nil nil string)))
  (while (string-match "\n\n+" string)
    (setq string (replace-match "\n" nil nil string)))
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
                       (insert "-- **")
                       (insert old-value "\n")
                       (set-buffer buffer-2)
                       (insert "-- **")
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
          key
          point)
      (set-buffer buffer)
      (goto-char (point-min))
      (while (re-search-forward "-- Name: \\([^;]+\\); Type: \\([^;]+\\); Schema: \\([a-zA-Z_]+\\)" nil t)
        (let ((name (match-string 1))
              (type (match-string 2))
              (schema (match-string 3)))
          (save-excursion
            (goto-char (line-beginning-position 0))
            (when (and key (not (string-match "^sogo[a-z]" name)))
              (puthash key (dump-value (buffer-substring-no-properties point (point))) data))
            (setq key (concat type "/" schema "/" name))
            (setq point (point)))))
      (when key
        (puthash key (dump-value (buffer-substring-no-properties point (point-max))) data))
      data)))

(defun dump-compare (file-1 file-2)
  (interactive "fFile 1: \nfFile 2: ")
  (let ((data-1 (dump-read-data file-1))
        (data-2 (dump-read-data file-2)))
    (dump-compare-data data-1 data-2 file-1 file-2 )))

(provide 'dump-compare)
