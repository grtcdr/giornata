;;; giornata-calendar.el --- Integration with the built-in calendar -*- lexical-binding: t -*-

;; Copyright (C) 2023 Taha Aziz Ben Ali

;; Author: Taha Aziz Ben Ali <ba.tahaaziz@gmail.com>
;; Maintainer: Taha Aziz Ben Ali <ba.tahaaziz@gmail.com>
;; License: ISC - See LICENSE for legalese
;; Version: 2024.04.03

(require 'giornata)
(require 'calendar)
(require 'iso8601)

(defun giornata--entries-as-dates (&optional year month)
  "Return diary entries as dates.
YEAR and MONTH can act as filters, returning only those entries
underneath them."
  (mapcar
   #'giornata--date-as-list
   (mapcar
    (lambda (filename)
      (string-remove-prefix (concat giornata-directory "/") filename))
    (funcall #'giornata--entries year month))))

(defun giornata--date-as-list (date)
  "Transform DATE string into a list.
DATE must be a valid ISO 8601 date."
  (when-let* ((date (string-replace "/" "-" date))
	      (decoded-date (iso8601-parse-date date))
	      (year (decoded-time-year decoded-date))
	      (month (decoded-time-month decoded-date))
	      (day (decoded-time-day decoded-date)))
    (list year month day)))

(defun giornata--mdy-to-ymd (date)
  "Convert a DATE from (MONTH DAY YEAR) to (YEAR MONTH DAY)."
  (let ((year  (nth 0 date))
	(month (nth 1 date))
	(day   (nth 2 date)))
    (list month day year)))

(defun giornata--highlight-entries (month year &optional _)
  "Highlight entries of the specified MONTH and YEAR."
  (let ((entries
	 ;; Needlessly convert the format of every entry from
	 ;; YEAR-MONTH-DAY to MONTH-DAY-YEAR (as the calendar's internal
	 ;; library makes some terrible assumptions).
	 (mapcar #'giornata--mdy-to-ymd
		 ;; A month may be displayed for which no entries have yet
		 ;; been made.
		 (ignore-error 'file-missing
		   (giornata--entries-as-dates year month)))))
    (dolist (date entries)
      (calendar-mark-visible-date date))))

;;;###autoload
(defun giornata-from-calendar ()
  "Create an entry in the diary for the date at point."
  (interactive)
  (condition-case nil
      (let* ((date  (calendar-cursor-to-date))
	     (year  (nth 2 date))
	     (month (nth 0 date))
	     (day   (nth 1 date))
	     (time  (encode-time (list 0 0 0 day month year))))
	(calendar-exit t)
	(giornata--create-entry time))
    (void-variable
     (and (yes-or-no-p "See the calendar first?")
	  (calendar)))))

;;;###autoload
(define-minor-mode giornata-calendar-mode
  "Integration with the built-in calendar."
  :init-value nil
  :require 'calendar
  :version "0.1.0"
  (if giornata-calendar-mode
      (advice-add 'calendar-generate-month :after #'giornata--highlight-entries)
    (advice-remove 'calendar-generate-month #'giornata--highlight-entries)))

(provide 'giornata-calendar)
