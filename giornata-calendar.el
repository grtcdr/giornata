;;; giornata.el --- Integration with the built-in calendar -*- lexical-binding: t -*-

;; Copyright (C) 2023 Taha Aziz Ben Ali

(require 'giornata)
(require 'calendar)
(require 'iso8601)

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
	(giornata--create-entry time)
	(unless (eq major-mode 'markdown-mode)
	  (markdown-mode)))
    (void-variable
     (and (yes-or-no-p "See the calendar first?")
	  (calendar)))))

(defun giornata--directory-p (directory)
  "Return non-nil if DIRECTORY is considered valid."
  (let ((year-or-month (rx (or (= 4 digit) (= 2 digit)))))
    (string-match-p year-or-month directory)))

(defun giornata--entries (&optional year month)
  "Return a list of diary entries.
YEAR and MONTH are optional integers which act as filters."
  (let ((fyear (and year (format "%04d" year)))
	(fmonth (and month (format "%02d" month)))
	(base-directory giornata-directory))
    (if fyear
	(setq base-directory
	      (file-name-concat base-directory fyear)))
    (if fmonth
	(setq base-directory
	      (file-name-concat base-directory fmonth)))
    (directory-files-recursively
     base-directory giornata--entry-regexp nil
     #'giornata--directory-p)))

(defun giornata--entries-as-dates (&optional year month)
  "Return diary entries as dates."
  (mapcar
   #'giornata--date-as-list
   (mapcar
    (lambda (filename)
      (string-remove-prefix (concat giornata-directory "/") filename))
    (funcall #'giornata--entries year month))))

(defun giornata--date-as-list (date)
  "Transform a string-formatted DATE into a list.
DATE must be a valid ISO 8601 date."
  (when-let* ((date (string-replace "/" "-" date))
	      (decoded-date (iso8601-parse-date date))
	      (year (decoded-time-year decoded-date))
	      (month (decoded-time-month decoded-date))
	      (day (decoded-time-day decoded-date)))
    (list year month day)))

;;;###autoload
(define-minor-mode giornata-calendar-mode
  "Integration with the built-in calendar."
  :init-value nil
  :require 'calendar
  :version "0.1.0"
  (if giornata-calendar-mode
      (defadvice calendar-generate-month
	  (after giornata-highlight-entries activate)
	"Highlight the days with an associated journal entry."
	(let ((monthly-entries
	       ;; Needlessly convert the format of every entry from YEAR-MONTH-DAY
	       ;; to MONTH-DAY-YEAR (as the calendar's internal library makes some
	       ;; terrible assumptions)
	       (mapcar (lambda (date)
			 (let ((year  (nth 0 date))
			       (month (nth 1 date))
			       (day   (nth 2 date)))
			   (list month day year)))
		       ;; A month may be display for which no entries have yet
		       ;; been made.
		       (ignore-error 'file-missing
			 (giornata--entries-as-dates year month)))))
	  (dolist (entry monthly-entries)
	    (calendar-mark-visible-date entry))))
    (advice-remove 'calendar-generate-month 'giornata-highlight-entries)))

(provide 'giornata-calendar)
