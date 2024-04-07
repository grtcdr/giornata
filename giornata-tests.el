;;; giornata-tests.el --- Test suite -*- lexical-binding: t -*-

;; Copyright (C) 2023 Taha Aziz Ben Ali

;; Author: Taha Aziz Ben Ali <ba.tahaaziz@gmail.com>
;; Maintainer: Taha Aziz Ben Ali <ba.tahaaziz@gmail.com>
;; License: ISC - See LICENSE for legalese
;; Version: 2024.04.03

;; This test suite includes both unit and integration tests, the latter must be
;; frozen in time so faketime(1) is a hard requirement for most tests to
;; succeed.

(require 'ert)
(require 'giornata)
(require 'giornata-calendar)


;;; Helpers

(defmacro with-temporary-giornata-directory (&rest body)
  "Evaluate BODY deterministically.

BODY will be evaluated with `giornata-directory' bound to a
temporary directory."
  (declare (indent nil))
  `(let ((giornata-directory (make-temp-file "giornata-" :directory)))
     (prog1 (progn ,@body)
       (delete-directory giornata-directory t))))

(defun giornata-create-entries (number direction)
  "Create the given NUMBER of entries in either DIRECTION."
  (let ((index 0))
    (while (> number index)
      (setq index (1+ index))
      (save-window-excursion
	(calendar)
	(cond ((eq direction 'forward)
	       (calendar-forward-day index))
	      ((eq direction 'backward)
	       (calendar-backward-day index)))
	(giornata-from-calendar)
	(save-buffer)))))

(defun giornata-relative-entry (filename)
  "Return FILENAME relative to `giornata-directory'."
  (string-trim-left
   (string-remove-prefix
    (expand-file-name giornata-directory) filename)
   "/"))


;;; Tests

(ert-deftest giornata--buffer-empty-p ()
  "Check that `giornata--buffer-empty-p' works as expected."
  (with-temp-buffer
    (should (equal (giornata--buffer-empty-p) t)))
  (with-temp-buffer
    (insert "foo")
    (should (not (giornata--buffer-empty-p)))))

(ert-deftest giornata-from-calendar ()
  "Check that `giornata-from-calendar' works as expected."
  (with-temporary-giornata-directory
   (giornata-create-entries 7 'forward)
   (giornata-create-entries 7 'backward)
   (should (= (length (giornata--entries)) 14))))

(ert-deftest giornata--directory-p ()
  "Check that `giornata--directory-p' works as expected."
  (should (equal (giornata--directory-p "foo") nil))
  ;; Month-matching
  (should (= (giornata--directory-p "20") 0))
  ;; Year-matching
  (should (= (giornata--directory-p "2024") 0)))

(ert-deftest giornata--format-front-matter ()
  "Check that `giornata--format-front-matter' works as expected."
  (let ((time (decode-time (current-time)))
	(giornata-front-matter "%A (%a), %y-%m-%d"))
    (should
     (string-equal
      (giornata--format-front-matter time)
      "Monday (Mon), 2024-01-01"))))

(ert-deftest giornata-today ()
  "Check that `giornata-today' works as expected."
  (with-temporary-giornata-directory
   (call-interactively #'giornata-today)
   (call-interactively #'save-buffer)
   (let* ((entries (giornata--entries))
	  (first (giornata-relative-entry (car entries))))
     (should (equal (length entries) 1))
     (should (string-equal first "2024/01/01")))))

(ert-deftest giornata-scaffold ()
  "Check that `giornata-scaffold' works as expected."
  (with-temporary-giornata-directory
   (let ((giornata-dir-locals
	  '((fundamental-mode . ((mode . text))))))
     (call-interactively #'giornata-scaffold)
     (should
      (string-equal
       (format "%S\n" giornata-dir-locals)
       (with-temp-buffer
	 (insert-file-contents
	  (file-name-concat giornata-directory dir-locals-file))
	 (buffer-string)))))))

(ert-deftest giornata--default-file-format ()
  "Check that `giornata--default-file-format' works as expected."
  (with-temporary-giornata-directory
   ;; Without a dir-locals.el, `text' is the default format.
   (should (equal (giornata--default-file-format) 'text))
   ;; Check again with a modified `giornata-dir-locals'.
   (let ((giornata-dir-locals
	  '((fundamental-mode . ((mode . markdown))))))
     (call-interactively #'giornata-scaffold))
   (should (equal (giornata--default-file-format) 'markdown))))

(ert-deftest giornata--default-major-mode ()
  "Check that `giornata--default-major-mode' works as expected."
  (with-temporary-giornata-directory
   ;; Without a dir-locals.el, `text' is the default format.
   (should (equal (giornata--default-major-mode) 'text-mode))
   ;; Check again with a modified `giornata-dir-locals'.
   (let ((giornata-dir-locals
	  '((fundamental-mode . ((mode . markdown))))))
     (call-interactively #'giornata-scaffold))
   (should (equal (giornata--default-major-mode) 'markdown-mode))))

(ert-deftest giornata--date-as-list ()
  "Check that `giornata--date-as-list' works as expected."
  (should (equal (giornata--date-as-list "1985/03/20")
		 (list 1985 03 20)))
  ;; `giornata--date-as-list' only works with ISO 8601 dates.
  (should-error (giornata--date-as-list "01/01/2000")
		:type 'wrong-type-argument))
