(require 'ert)
(require 'giornata)
(require 'giornata-calendar)

(defmacro with-deterministic-giornata (&rest body)
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
  (with-deterministic-giornata
   (call-interactively #'giornata-today)
   (call-interactively #'save-buffer)
   (let* ((entries (giornata--entries))
	  (first (giornata-relative-entry (car entries))))
     (should (= (length entries) 1))
     (should (string= first "2024/01/01")))))

