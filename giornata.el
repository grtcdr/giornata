;;; giornata.el --- Foolishly simple journaling -*- lexical-binding: t -*-

;; Copyright (C) 2023 Taha Aziz Ben Ali

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
;; REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
;; AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
;; INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
;; LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
;; OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;; PERFORMANCE OF THIS SOFTWARE.

(require 'calendar)

(defgroup giornata nil
  "Foolishly simple journaling."
  :prefix "giornata-")

(defcustom giornata-directory "~/diary"
  "Directory containing your diary."
  :type 'directory
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
	   (cons ?y (format "%04d" year))
	   (cons ?m (format "%02d" month))
	   (cons ?d (format "%02d" day))))))

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
    (giornata--create-entry year month day)
    (unless (eq major-mode 'markdown-mode)
      (markdown-mode))))

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

