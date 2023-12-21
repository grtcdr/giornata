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
  :prefix "giornata-"
  :group 'convenience)

(defcustom giornata-directory "~/diary"
  "Directory containing your diary."
  :type 'directory
  :group 'giornata)

(defvar giornata-front-matter
  (concat "---\n"
	  "title: %a, %y-%m-%d\n"
	  "---\n\n"))

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
    (when (= (point-min) (point-max))
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
(defun giornata-from-calendar ()
  "Create an entry in the diary for the date at point."
  (interactive)
  (condition-case nil
      (let* ((date  (calendar-cursor-to-date))
	     (year  (nth 2 date))
	     (month (nth 0 date))
	     (day   (nth 1 date))
	     (time  (encode-time (list 0 0 0 day month year))))
	(giornata--create-entry time)
	(unless (eq major-mode 'markdown-mode)
	  (markdown-mode)))
    (void-variable
     (and (yes-or-no-p "See the calendar first?")
	  (calendar)))))

(provide 'giornata)
