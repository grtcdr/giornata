;; -*- lexical-binding: t -*-

;; Copyright (C) 2023 Taha Aziz Ben Ali

(require 'calendar)

(defgroup giornata nil
  "Foolishly simple diary system."
  :prefix "giornata-")

(defcustom giornata-directory "~/diary-giornata"
  "Directory containing your diary."
  :type 'file
  :group 'giornata)

(defun giornata--template (year month day)
  "Return the template of diary entries."
  (when-let* ((day-name (calendar-day-name (list month day year))))
    (format-spec
     (concat
      "---\n"
      "title: %t, %y-%m-%d\n"
      "---\n\n")
     (list (cons ?t day-name)
	   (cons ?y year)
	   (cons ?m month)
	   (cons ?d day)))))

(defun giornata--create-entry (year month day)
  "Create or open the entry corresponding to YEAR, MONTH and DAY."
  (let* ((base  (expand-file-name (format "%04d/%02d" year month) giornata-directory))
	 (entry (expand-file-name (format "%02d" day) base)))
    (make-directory base :parents)
    (find-file entry)
    (when (= (point-min) (point-max))
      (insert (giornata--template year month day)))
    (unless (eobp)
      (end-of-buffer))))

;;;###autoload
(defun giornata-today ()
  "Create or open today's entry in the diary."
  (interactive)
  (let* ((date  (decode-time))
	 (year  (decoded-time-year date))
	 (month (decoded-time-month date))
	 (day   (decoded-time-day date)))
    (giornata--create-entry year month day)))

;;;###autoload
(defun giornata-create-entry-from-calendar ()
  "Create an entry in the diary for the date at point."
  (interactive)
  ;; [2023-12-11] TODO: Can error handling be improved?
  (if-let* ((date  (calendar-cursor-to-date))
	    (year  (nth 2 date))
	    (month (nth 0 date))
	    (day   (nth 1 date)))
      (giornata--create-entry year month day)
    (user-error "Could not extract date from point")))

(provide 'giornata)

