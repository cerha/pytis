;;; pytis-transform.el --- Pytis source transformations

;; Copyright (C) 2009 Brailcom, o.p.s.

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

;;; Commentary:

;;

;;; Code:

(require 'cl)
(require 'dired)

(defvar pytis-spec-class-regexp "^class ")

(defvar pytis-transformations
  '(("Action" "name" "a_")
    ("Filter" "id" "f_")
    ("Condition" "id" "f_")
    ("Binding" "id" "b_")))

(defun pytis-transform-string-to-id (string)
  (save-match-data
    (when (string-match "\\W\\{3\\}" string)
      (setq string (substring string 0 (match-beginning 0)))))
  (apply 'string
         (mapcar #'(lambda (char)
                     (if (or (and (<= ?a char) (<= char ?z))
                             (and (<= ?0 char) (<= char ?9))
                             (member char '(?_ ?-)))
                         char
                       (or (cdr (assoc char '((?á . ?a) (?č . ?c) (?ď . ?d) (?é . ?e) (?ě . ?e)
                                              (?í . ?i) (?ň . ?n) (?ó . ?o) (?ř . ?r) (?š . ?s)
                                              (?ť . ?t) (?ú . ?u) (?ů . ?u) (?ý . ?y) (?ž . ?z))))
                           ?_)))
                 (string-to-list (downcase string)))))

(defun pytis-transform-buffer ()
  (interactive)
  (goto-char (point-min))
  (let ((case-fold-search nil)
        (regexp (concat pytis-spec-class-regexp
                        "\\|\\<\\("
                        (mapconcat 'first pytis-transformations "\\|")
                        "\\)("))
        (ids '()))
    (while (re-search-forward regexp nil t)
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (cond
         ((eq (plist-get (text-properties-at (point)) 'face)
              'font-lock-comment-face)
          ;; We can't transform in a commented out code because forward-sexp
          ;; would not work there.
          nil)
         ((save-excursion
            (goto-char beg)
            (looking-at pytis-spec-class-regexp))
          (setq ids '()))
         (t
          (destructuring-bind (class id-arg prefix) (assoc (buffer-substring beg (- end 1)) pytis-transformations)
            (flet ((adjusted-id (id &optional prefix)
                     (when prefix
                       (setq id (concat prefix id)))
                     (while (member id ids)
                       (setq id (concat id "x")))
                     (cons (concat "'" id "'") id)))
              (let ((id (when (looking-at "\\(_(\\)?['\"]\\([^'\"\n]+\\)")
                          (adjusted-id (pytis-transform-string-to-id (match-string 2)) prefix)))
                    (id-arg-regexp (format ",[ \n]*%s=['\"]\\([^'\"\n]*\\)['\"]" id-arg))
                    (general-id-arg-regexp (format ",[ \n]*%s=\\([a-zA-Z0-9_]+\\)" id-arg)))
                (save-excursion
                  (loop
                   (when (looking-at id-arg-regexp)
                     (setq id (adjusted-id (match-string 1)))
                     (delete-region (match-beginning 0) (match-end 0))
                     (return))
                   (when (looking-at general-id-arg-regexp)
                     (setq id (cons (match-string 1) nil))
                     (delete-region (match-beginning 0) (match-end 0))
                     (return))
                   (condition-case nil
                       (forward-sexp)
                     (scan-error (return)))))
                (unless id
                  (error "Can't find id here"))
                (when (cdr id)
                  (push (cdr id) ids))
                (insert (format "%s, " (car id))))))))))))
               
(defun pytis-transform-dired (&optional transform-function)
  (interactive)
  (dolist (file (dired-get-marked-files nil nil 'dired-nondirectory-p))
    (let ((buffer (get-file-buffer file)))
      (if (and buffer (with-current-buffer buffer buffer-read-only))
	  (error "File `%s' is visited read-only" file))))
  (dolist (file (dired-get-marked-files nil nil 'dired-nondirectory-p))  
    (find-file file)
    (funcall (or transform-function #'pytis-transform-buffer))))

(defun pytis-transform-specifications ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward pytis-spec-class-regexp nil t)
    (beginning-of-line 2)
    (when (looking-at "^    \"")
      (forward-sexp)
      (beginning-of-line 2))
    (let* ((bound (save-excursion (or (re-search-forward pytis-spec-class-regexp nil t)
                                      (point-max))))
           (marked (save-excursion (re-search-forward "^    public =" bound t)))
           (answer (unless marked (read-char "Public? (y, n, s) "))))
      (cond
       ((equal answer ?y)
        (insert "    public = True\n"))
       ((equal answer ?n)
        (insert "    public = False\n"))))))

(defun pytis-transform-specifications-dired ()
  (interactive)
  (pytis-transform-dired #'pytis-transform-specifications))


;;; Announce

(provide 'pytis-transform)

;;; pytis-transform.el ends here
