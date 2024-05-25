;;; giornata.el --- Foolishly simple journaling -*- lexical-binding: t -*-

;; Copyright (C) 2023 Taha Aziz Ben Ali <ba.tahaaziz@gmail.com>
;; License: ISC - See LICENSE for legalese
;; Homepage: https://giornata.grtcdr.tn
;; Version: 2024.05.25

(require 'giornata-lib)

(defgroup giornata ()
  "Foolishly simple journaling."
  :prefix "giornata-"
  :group 'convenience)

(defcustom giornata-directory "~/journal"
  "Directory containing your journal."
  :type 'directory
  :group 'giornata)

(defcustom giornata-front-matter
  "* %A, %d-%m-%y\n\n"
  "String to be inserted at the top of a newly opened journal entry.

This variable is processed by `giornata-front-matter-spec' to
produce the final front matter.

The following format strings, if encountered, are expanded to
their associated value:

    %A, the day of the week,
    %a, like %A but abbreviated,
    %y, the year,
    %m, the month,
    %d, the day.

Introducing a new format specifier should be accompanied by the
addition of a new entry in `giornata-front-matter-spec'.

What you set this variable to depends on the file format you
choose to write in.

For example, if you're using `markdown-mode', you could use:

    (setq giornata-front-matter
      (concat \"---\\n\"
	      \"date: %A, %y-%m-%d\\n\"
	      \"---\\n\\n\"))

And for those using `org-mode', consider this:

    (setq giornata-front-matter \"#+DATE: <%d-%m-%y %a>\\n\\n\")
"
  :type 'string
  :group 'giornata)

(defcustom giornata-dir-locals
  '((fundamental-mode
     ((mode . text)
      (mode . auto-fill)
      (mode . buffer-face)
      (cursor-type . bar))))
  "Generic directory local variables.

This variable determines the configuration that
`giornata-scaffold' saves to the `dir-locals-file' relative to
the `giornata-directory'.

Given that journal entries do not possess a file extension, they
should be configured as if their mode were `fundamental-mode'."
  :type '(alist)
  :link '(info-link "(emacs)Directory Variables")
  :group 'giornata)

(defcustom giornata-menu-item nil
  "Add an item for `giornata-today' to the menu bar.
If non-nil, an item named \"Giornata\" will appear under the
\"Tools\" menu."
  :type 'boolean
  :group 'giornata
  :set
  #'(lambda (symbol value)
      (set-default symbol value)
      (if value
	  (define-key-after global-map [menu-bar tools giornata]
	    (cons "Giornata" 'giornata-today)
	    'calendar)
	(define-key global-map [menu-bar tools giornata] nil t))))

;;;###autoload
(defun giornata-today ()
  "Create or visit today's entry in the journal."
  (interactive)
  (giornata--create-entry (current-time)))

;;;###autoload
(defun giornata-scaffold ()
  "Scaffold `giornata-directory' with variable `giornata-dir-locals'."
  (interactive)
  (save-window-excursion
    (find-file (file-name-concat giornata-directory dir-locals-file))
    (if (giornata--buffer-empty-p)
	(progn
	  (insert (format "%S" giornata-dir-locals))
	  (save-buffer))
      (user-error "There already exists a \"%s\" file in \"%s\"." dir-locals-file giornata-directory))))

(provide 'giornata)
