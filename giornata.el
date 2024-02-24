;;; giornata.el --- Foolishly simple journaling -*- lexical-binding: t -*-

;; Copyright (C) 2023 Taha Aziz Ben Ali

(require 'markdown-mode)

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

(defvar giornata-front-matter
  (concat "---\n"
	  "date: %a, %y-%m-%d\n"
	  "---\n\n"))

(defcustom giornata-dir-locals
  '((fundamental-mode . ((mode . markdown)
			 (mode . auto-fill)
			 (mode . olivetti)
			 (mode . buffer-face)
			 (cursor-type . bar)
			 (olivetti-body-width . 0.4))))
  "Generic directory local variables."
  :type 'list)

(defun giornata-front-matter-spec (time)
  "Return a specification alist for `giornata-front-matter'.
TIME is a list such as the one returned by `decode-time'."
  (let ((day   (decoded-time-day time))
	(month (decoded-time-month time))
	(year  (decoded-time-year time)))
    (list (cons ?a (calendar-day-name (list month day year)))
	  (cons ?y (format "%04d" year))
	  (cons ?m (format "%02d" month))
	  (cons ?d (format "%02d" day)))))

(defun giornata--format-front-matter (time)
  "Return the formatted front matter.
TIME is a list such as the one returned by `decode-time'.
Internally, this formats `giornata-front-matter' using
`giornata-front-matter-spec'."
  (format-spec giornata-front-matter
	       (giornata-front-matter-spec time)))


(defun giornata-buffer-empty? ()
  "Return non-nil if buffer is empty."
  (= (point-min) (point-max)))

(defun giornata--create-entry (timestamp)
  "Create or visit the entry corresponding to TIMESTAMP.
TIMESTAMP is a time value."
  (let* ((time      (decode-time timestamp))
	 (year      (decoded-time-year time))
	 (month     (decoded-time-month time))
	 (day       (decoded-time-day time))
	 (directory (expand-file-name (format "%04d/%02d" year month) giornata-directory))
	 (filename  (expand-file-name (format "%02d" day) directory)))
    (make-directory directory :parents)
    (find-file filename)
    (when (giornata-buffer-empty?)
      (insert (giornata--format-front-matter time)))
    (unless (eobp)
      (goto-char (point-max)))))

;;;###autoload
(defun giornata-today ()
  "Create or visit today's entry in the diary."
  (interactive)
  (giornata--create-entry (current-time))
  (unless (eq major-mode 'markdown-mode)
    (markdown-mode)))

;;;###autoload
(defun giornata-consult ()
  (interactive)
  (if (require 'consult nil :noerror)
      (consult--grep "Search" #'consult--grep-make-builder giornata-directory nil)
    (user-error "`consult' is not available.")))

;;;###autoload
(defun giornata-scaffold ()
  "Scaffold `giornata-directory' with a predefined configuration."
  (interactive)
  (save-window-excursion
    (find-file (file-name-concat giornata-directory dir-locals-file))
    (if (giornata-buffer-empty?)
	(progn
	  (insert (format "%S" giornata-dir-locals))
	  (save-buffer))
      (user-error "There already exists a \"%s\" file in \"%s\"." dir-locals-file giornata-directory))))

(provide 'giornata)
