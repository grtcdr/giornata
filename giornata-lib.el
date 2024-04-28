;;; giornata.el --- Foolishly simple journaling -*- lexical-binding: t -*-

;; Copyright (C) 2023 Taha Aziz Ben Ali <ba.tahaaziz@gmail.com>
;; License: ISC - See LICENSE for legalese
;; Homepage: https://giornata.grtcdr.tn
;; Version: 2024.04.20

(require 'calendar)

(defvar giornata-directory)
(defvar giornata-front-matter)

(defconst giornata--entry-regexp
  (rx string-start (= 2 digit) string-end)
  "Return a regular expression matching a valid entry in the diary.")

(defconst giornata--directory-regexp
  (rx string-start (or (= 4 digit) (= 2 digit)) string-end)
  "Return a regular expression matching a valid directory in the diary.")

(defvar giornata-major-modes '(markdown org text rst)
  "List of supported major modes.")

(defun giornata--buffer-empty-p ()
  "Return non-nil if buffer is empty."
  (= (point-min) (point-max)))

(defun giornata--normalize-directory ()
  "Return a normalized version of `giornata-directory'.
The function ensures that the former always ends in a slash."
  (string-pad
   giornata-directory
   (1+ (length giornata-directory))
   ?/))

(defun giornata-dir-locals ()
  "Return the effective directory local variables of `giornata-directory'."
  (let* ((directory (giornata--normalize-directory))
	 (class (intern directory))
	 (variables (dir-locals-get-class-variables class)))
    (cond ((not variables)
	   (and (dir-locals-read-from-dir directory)
		(dir-locals-get-class-variables class)))
	  (t variables))))

(defun giornata--default-file-format ()
  "Return the default file format of journal entries."
  (let* ((variables (giornata-dir-locals))
	 (fundamental (cdr (assoc 'fundamental-mode variables))))
    (catch 'mode
      (dolist (major-mode giornata-major-modes)
	(when (rassoc major-mode fundamental)
	  (throw 'mode major-mode)))
      (quote text))))

(defun giornata--default-major-mode ()
  "Return the default major mode of journal entries."
  (thread-first
    (giornata--default-file-format)
    (symbol-name)
    (concat "-mode")
    (intern)))

(defun giornata--directory-p (filename)
  "Return non-nil if FILENAME is considered a valid directory."
  (string-match-p
   giornata--directory-regexp
   (car (last (file-name-split filename)))))

(defun giornata-front-matter-spec (time)
  "Return a specification alist for `giornata-front-matter'.
TIME is a list like that which `decode-time' returns."
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

(defun giornata--entries (&optional year month)
  "Return a list of diary entries.
YEAR and MONTH can act as filters, returning only those entries
beneath them."
  (directory-files-recursively
     (cond ((and month year)
	    (file-name-concat
	     giornata-directory
	     (format "%04d" year)
	     (format "%02d" month)))
	   (year
	    (file-name-concat
	     giornata-directory
	     (format "%04d" year)))
	   (t giornata-directory))
     giornata--entry-regexp nil #'giornata--directory-p))

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
    (unless (eq major-mode (giornata--default-major-mode))
      (funcall major-mode))
    (when (giornata--buffer-empty-p)
      (insert (giornata--format-front-matter time)))
    (unless (eobp)
      (goto-char (point-max)))))

(provide 'giornata-lib)
