;;; giornata-tests.el --- Integration test suite -*- lexical-binding: t -*-

(require 'ert)
(require 'giornata)
(require 'giornata-calendar)

(defmacro with-temporary-giornata-directory (&rest body)
  "Evaluate BODY deterministically.

BODY will be evaluated with `giornata-directory' bound to a
temporary directory.  `current-time' will be set to a fake time."
  (declare (indent nil))
  `(let ((giornata-directory (make-temp-file "giornata-" :directory)))
     (prog1 (progn ,@body)
       (delete-directory giornata-directory :recurse))))

(defun giornata-relative-entry (filename)
  "Return FILENAME relative to `giornata-directory'."
  (string-trim-left
   (string-remove-prefix
    (expand-file-name giornata-directory) filename)
   "/"))

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

(ert-deftest giornata-default-format ()
  "Check that `giornata-default-format' works as expected."
  (with-temporary-giornata-directory
   ;; Without a dir-locals.el, `text' is the default format.
   (should (equal (giornata-default-format) 'text))
   ;; Check again with a modified `giornata-dir-locals'
   (let ((giornata-dir-locals
	  '((fundamental-mode . ((mode . markdown))))))
     (call-interactively #'giornata-scaffold))
   (should (equal (giornata-default-format) 'markdown))))

(defmacro with-giornata-front-matter (&rest body)
  "Evaluate BODY with a preset `giornata-front-matter'."
  (declare (indent nil))
  `(let ((time (decode-time (current-time)))
	 (giornata-front-matter
	  '((markdown-mode . "---\ndate: %A, %y-%m-%d\n---\n")
	    (org-mode . "#+DATE: <%y-%m-%d %a>\n")
	    (text-mode . "%A, %y-%m-%d\n"))))
     ,@body))

(ert-deftest giornata--format-front-matter:markdown-mode ()
  (with-giornata-front-matter
   (should
    (string-equal
     (giornata--format-front-matter 'markdown-mode time)
     "---\ndate: Monday, 2024-01-01\n---\n"))))

(ert-deftest giornata--format-front-matter:org-mode ()
  (with-giornata-front-matter
   (should
    (string-equal
     (giornata--format-front-matter 'org-mode time)
     "#+DATE: <2024-01-01 Mon>\n"))))

(ert-deftest giornata--format-front-matter:text-mode ()
  (with-giornata-front-matter
   (should
    (string-equal
     (giornata--format-front-matter 'text-mode time)
     "Monday, 2024-01-01\n"))))

(ert-deftest giornata-buffer-empty? ()
  (with-temp-buffer
    (should (equal (giornata-buffer-empty?) t))))
