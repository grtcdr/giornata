;;; giornata.el --- Foolishly simple journaling -*- lexical-binding: t -*-

;; Copyright (C) 2023 Taha Aziz Ben Ali

;; Author: Taha Aziz Ben Ali <ba.tahaaziz@gmail.com>
;; Maintainer: Taha Aziz Ben Ali <ba.tahaaziz@gmail.com>
;; Version: 2024.04.03

(declare-function consult--grep "consult" (prompt make-builder dir initial))
(declare-function consult--grep-make-builder "consult" (paths))
(declare-function calendar-day-name "calendar" (date &optional abbrev absolute))

(defgroup giornata nil
  "Foolishly simple journaling."
  :prefix "giornata-"
  :group 'convenience)

(defcustom giornata-directory "~/diary"
  "Directory containing your diary."
  :type 'directory
  :group 'giornata)

(defcustom giornata-front-matter
  "* %A, %d-%m-%y\n\n"
  "String to be inserted at the top of a newly opened journal entry.

This variable is processed by `giornata-front-matter-spec' to
produce the final front matter.

The following format strings, if encountered, are expanded to
their associated value:

    %A, the day of the week,
    %a, like %A but abbreviated,
    %y, the year,
    %m, the month,
    %d, the day.

What you set this variable to depends on the file format you
choose to write in.

If you're using `markdown-mode', you could use:

    (setq giornata-front-matter
      (concat \"---\\n\"
	      \"date: %A, %y-%m-%d\\n\"
	      \"---\\n\\n\"))

If you're using `org-mode', consider using this:

    (setq giornata-front-matter \"#+DATE: <%d-%m-%y %a>\\n\\n\")
"
  :type 'string)

(defcustom giornata-dir-locals
  '((fundamental-mode
     . ((mode . text)
	(mode . auto-fill)
	(mode . olivetti)
	(mode . buffer-face)
	(cursor-type . bar)
	(olivetti-body-width . 0.4))))
  "Generic directory local variables.

This variable determines the configuration that
`giornata-scaffold' saves to the `dir-locals-file' relative to
the `giornata-directory'."
  :type 'list)

(defun giornata-front-matter-spec (time)
  "Return a specification alist for `giornata-front-matter'.
TIME is a list such as the one returned by `decode-time'."
  (let ((day   (decoded-time-day time))
	(month (decoded-time-month time))
	(year  (decoded-time-year time)))
    (list (cons ?A (calendar-day-name (list month day year)))
	  (cons ?a (calendar-day-name (list month day year) t))
	  (cons ?y (format "%04d" year))
	  (cons ?m (format "%02d" month))
	  (cons ?d (format "%02d" day)))))

(defun giornata--format-front-matter (time)
  "Return the formatted front matter.
TIME is a list such as that returned by `decode-time'.
Internally, this formats `giornata-front-matter' using
`giornata-front-matter-spec'."
  (format-spec giornata-front-matter
	       (giornata-front-matter-spec time)))

(defun giornata--buffer-empty-p ()
  "Return non-nil if buffer is empty."
  (= (point-min) (point-max)))

(defun giornata-dir-locals ()
  "Return the effective directory local variables of `giornata-directory'."
  (let* ((directory (if (string-suffix-p "/" giornata-directory)
			giornata-directory
		      (concat giornata-directory "/")))
	 (class (intern directory))
	 (variables (dir-locals-get-class-variables class)))
    (cond ((not variables)
	   (and (dir-locals-read-from-dir directory)
		(dir-locals-get-class-variables class)))
	  (t variables))))

(defun giornata-default-format ()
  "Return the default file format of journal entries."
  (let* ((variables (giornata-dir-locals))
	 (fundamental (cdr (assoc 'fundamental-mode variables))))
    (catch 'mode
      (dolist (major-mode '(markdown org text))
	(when (rassoc major-mode fundamental)
	  (throw 'mode major-mode)))
      (quote text))))

(defun giornata--create-entry (timestamp)
  "Create or visit the entry corresponding to TIMESTAMP.
TIMESTAMP is a time value.  MODE is the default major mode, it is
used to determine what front matter to insert."
  (let* ((time      (decode-time timestamp))
	 (year      (decoded-time-year time))
	 (month     (decoded-time-month time))
	 (day       (decoded-time-day time))
	 (directory (expand-file-name (format "%04d/%02d" year month) giornata-directory))
	 (filename  (expand-file-name (format "%02d" day) directory))
	 (mode      (intern (concat (symbol-name (giornata-default-format)) "-mode"))))
    (make-directory directory :parents)
    (find-file filename)
    (unless (eq major-mode mode)
      (funcall major-mode))
    (when (giornata--buffer-empty-p)
      (insert (giornata--format-front-matter time)))
    (unless (eobp)
      (goto-char (point-max)))))

;;;###autoload
(defun giornata-today ()
  "Create or visit today's entry in the diary."
  (interactive)
  (giornata--create-entry (current-time)))

;;;###autoload
(defun giornata-consult ()
  "Search `giornata-directory' with `grep' via `consult' package."
  (interactive)
  (if (require 'consult nil :noerror)
      (consult--grep "Search" #'consult--grep-make-builder giornata-directory nil)
    (user-error "`consult' is not available.")))

;;;###autoload
(defun giornata-scaffold ()
  "Scaffold `giornata-directory' with variable `giornata-dir-locals'."
  (interactive)
  (save-window-excursion
    (find-file (file-name-concat giornata-directory dir-locals-file))
    (if (giornata--buffer-empty-p)
	(progn
	  (insert (format "%S" giornata-dir-locals))
	  (save-buffer))
      (user-error "There already exists a \"%s\" file in \"%s\"." dir-locals-file giornata-directory))))

(provide 'giornata)
