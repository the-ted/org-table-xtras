;;; org-table-xtras.el --- Some Add-ins for org-table
;;
;; Author: Theodore Wiles
;; Created: 2014-01-04
;; Version: 0.5
;; Last-Updated: 2014-01-04
;; URL: http://github.com/theodorewiles/org-table-xtras
;; Keywords:
;; Compatability:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This minor mode adds some extra functionality to org-mode tables.
;; It defines functions to copy field formulas, and also defines functions
;; to insert footnotes describing the formulas used in each cell.
;; Finally, it also creates a function to iterate spreadsheets using separate inputs.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 0.5 Initial version
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;
;; In order to get footnotes automatically added to your exported documents, put
;; the following in your .emacs:
;; (add-hook 'org-export-before-parsing-hook 'org-table-xtras-export-hook)
;;
;;; Code:

(eval-when-compile
  (require 'cl))


(defun org-table-xtras-copy-formula (n type &optional negative?)
  "Copy a formula N times, either across a :row, :column,
or :diag (values for TYPE). If you want to copy the formula from
right-to-left or down-to-up instead, set NEGATIVE? to t."
  (let* ((offset (case type
		   (:row '(1 0))
		   (:col '(0 1))
		   (:diag '(1 1))))
	 (offset (if negative? (map 'list (lambda (x) (* x -1)) offset)
		   offset))

	 (formula (org-table-current-field-formula))
	 (value (org-table-get-field))
	 (replacement (if
			  (not (eq formula nil))
			  formula
			value))
	 (newline (+ (org-table-current-line) (first offset)))
	 (newcol (+ (org-table-current-column) (second offset)))
	 (stored-list (org-table-get-stored-formulas))
	 (scol (format "@%d$%d" newline newcol)))
    (progn
      (if (eq formula nil)
	  (progn
	    (setq stored-list (delq (assoc scol stored-list) stored-list))
	    (org-table-store-formulas stored-list)
	    (org-table-put newline newcol replacement t))
	(org-table-put newline newcol formula t))
      (org-table-goto-line newline)
      (org-table-goto-column newcol)
      (org-table-maybe-eval-formula)
      (if (> n 1)
	  (org-table-xtras-copy-formula (- n 1) type negative?)))))

(defun org-table-xtras-copy-field-next-row (&optional a)
  "Copy a field's formula or value to the next row. A prefix
argument determines how many successive times the formula or
value is added. Negative prefix values copy in the reverse
order."
  (interactive "p")
  (let ((b (if (not a) 0 a)))
    (org-table-xtras-copy-formula (abs b) :row (if (< b 0) t))))

(defun org-table-xtras-copy-field-next-column (&optional a)
  "Copy a field's formula or value to the next column. A prefix
argument determines how many successive times the formula or
value is added. Negative prefix values copy in the reverse
order."
  (interactive "P")
  (let ((b (if (eq a nil) 0 a)))
    (org-table-xtras-copy-formula (abs b) :col (if (< b 0) t))))

(defun org-table-xtras-copy-field-diagonally (&optional a)
  "Copy a field's formula or value to the next diagonal. A prefix
argument determines how many successive times the formula or
value is added. Negative prefix values copy in the reverse
order."
  (interactive "P")
  (let ((b (if (eq a nil) 0 a)))
    (org-table-xtras-copy-formula (abs b) :diag (if (< b 0) t))))



(defun org-table-xtras-get-cell-index (entry type)
  "Return the :row or :col (based on TYPE) index of ENTRY"
  (let* ((row-expr "[\\.@]\\([0-9]+\\)")
	 (col-expr "\\$\\([0-9]+\\)")
	 (expr "[\\.@]\\([0-9]+\\)\\$\\([0-9]+\\)")
	 (cell-type (if (eq type :row) 1 2))
	 (format-formula (org-table-convert-refs-to-rc (first entry))))
    (progn
      (if (string-match (if (eq type :row) row-expr col-expr) format-formula)
	  (string-to-int (match-string 1 format-formula))
	(if (eq type :col) (string-to-int format-formula)
	  0)))))

(defun org-table-xtras-clean-entry-formula (text) 
  "Format the TEXT formula into a nice-looking formula representation"
  (concat "=" (replace-regexp-in-string "'" "`" text) "="))
;;  (replace-regexp-in-string "\\$" "!" text)

(defun org-table-xtras-update-table (entry index update-table)
  "Insert the correct footnote in the table"
  (let* (
	 (strindex (int-to-string index))
	 (col (org-table-xtras-get-cell-index entry :col))
	 (row (max (org-table-xtras-get-cell-index entry :row)
		   1))
	 ;; (dummy (message (int-to-string col)))
	 ;; (dummy (message (int-to-string row)))
	 (label (concat (org-table-xtras-tbl-name) "--" strindex ))
	 )
    (save-excursion
      (if (and (org-table-goto-line row) (<= col org-table-current-ncol))
	  (progn
	    (org-table-goto-line row)
	    (org-table-goto-column col)	   
	    (org-table-end-of-field 0)
	    (insert (concat " [fn:" label "]"))
	    (if update-table
		(progn
		  (ignore-errors (org-footnote-delete-definitions (concat "fn:"label)))
		  (org-footnote-create-definition label)
		  (insert (org-table-xtras-clean-entry-formula (cdr entry))))))))))

(defun org-table-xtras-sort-formulas (x y)
  "Sort formulas by how they first occur in the table"
  (string< (cdr x) (cdr y)))

(defun org-table-xtras-insert-formulas (e entries index)
  "Iterate through the available formulas, inserting them at point."
      (let* ((form1 (cdr e))
	     (form2 (cdr (first entries)))
	     (usable-index (if (eq nil index) 1 index)))
	(if (> (length entries) 0)
	    (if (equalp form1 form2) ;; If this is a duplicative formula
		(progn
		  (org-table-xtras-update-table e usable-index nil)
		  (org-table-xtras-insert-formulas (car entries) (cdr entries) usable-index))
	      (progn 
		(org-table-xtras-update-table e usable-index t)
		(org-table-xtras-insert-formulas (car entries) (cdr entries) (+ 1 usable-index))))
	  (org-table-xtras-update-table e usable-index t))))


(defun org-xtras-footnote-delete-definitions (label)
  "Delete every definition of the footnote LABEL. LABEL can be a regexp.
Return the number of footnotes removed."
  (save-excursion
    (goto-char (point-min))
    (let ((def-re (concat "^\\[" label "\\]"))
	  (ndef 0))
      (while (re-search-forward def-re nil t)
	(let ((full-def (org-footnote-at-definition-p)))
	  (when full-def
	    ;; Remove the footnote, and all blank lines before it.
	    (goto-char (nth 1 full-def))
	    (skip-chars-backward " \r\t\n")
	    (unless (bolp) (forward-line))
	    (delete-region (point) (nth 2 full-def))
	    (incf ndef))))
      ndef)))

(defun org-table-xtras-remove-fns ()
  (interactive)
  (goto-char (org-table-begin))
  (org-xtras-footnote-delete-definitions (concat "fn:" (org-table-xtras-tbl-name) "--[0-9]*"))
  (while (>
	  (org-footnote-delete-references (concat "fn:" (org-table-xtras-tbl-name) "--[0-9]+")) 0)
    (org-footnote-delete-references (concat "fn:" (org-table-xtras-tbl-name) "--[0-9]+"))))

(defun org-table-xtras-print-formulas ()
  "Print the formulas at the end of the table"
  (interactive)
  (let* ((fns (sort* (org-table-get-stored-formulas) 'org-table-xtras-sort-formulas)))
    (org-table-xtras-remove-fns)
    (org-table-xtras-insert-formulas (car fns) (cdr fns) 1)))

(defun org-table-xtras-eval-table (TBLNAME ARGS OUTPUTVAR)
  "Evaluate a table (TBLNAME) with different argument inputs.

ARGS is a list of parameter changes, in the form '((ARGNAME . VALUE)), 

OUTPUTVAR is the name of the parameter that you want to return."

  (save-excursion
    (org-table-xtras-go-to-table TBLNAME)
    (let* ((beg (org-table-begin))
	   (end (save-excursion 
		  (progn 
		    (goto-char (org-table-end))
		    (org-table-TBLFM-begin)
		    (line-end-position))))
	   (buf (current-buffer)))
      (with-temp-buffer
	(switch-to-buffer (current-buffer) nil t)
	(insert-buffer-substring buf beg end)
	(org-mode)
	(goto-char 1)
	(forward-line)
	(dolist (item ARGS)
	  (org-table-xtras-replace-param item))
	(org-table-iterate)
	(message (substring-no-properties (org-table-get-constant OUTPUTVAR)))))))

(defun org-table-xtras-replace-param (item)
  "Replace a parameter in a table with a new value. ITEM takes
the form '((ARGNAME . VALUE))"
  (let* ((param (car item))
	 (value (cdr item)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (concat param " *=[\.A-z0-9]* *|") nil t)
	(replace-match (concat param "=" value "|"))))))
	       
(defun org-table-xtras-go-to-table (name)
  "Go to the table given by NAME"
  (goto-char (point-min))
  (re-search-forward (concat "#\\+TBLNAME: +" name))
  (forward-line))

(defun org-table-xtras-tbl-name ()
  "Return the name of the current table, or prompt for one if none exists."
  (interactive)
  (save-excursion
    (goto-char (org-table-begin))
    (let ((pos (re-search-backward "#\\+TBLNAME: +\\([A-z0-9]+ *$\\)"
				   (line-end-position -1) t)))
      (if pos
	  (match-string 1)
	(progn
	  (previous-line)
	  (insert (concat
		   "\n#+TBLNAME: "
		   (read-from-minibuffer "No TBLNAME found. Insert new TBLNAME:")
		   )))))))

(defun org-table-xtras-run-print-formulas ()
  "Do org-table-xtras-print-formulas on all of the named tables in the exported file."
  (interactive)
  (let ((search (re-search-forward "#\\+TBLNAME: +\\([A-z0-9]+ *$\\)" nil t)))
    (if search
	(progn
	  (goto-char (org-table-begin))
	  (org-table-xtras-print-formulas)
	  t)
      nil)))

(defun org-table-xtras-export-hook (backend)
  "A hook that will populate all of the tables with footnotes for export only."
  (goto-char (point-min))
  (while (org-table-xtras-run-print-formulas)
    t))

(define-minor-mode org-table-xtras-mode
  "Some Add-ins for org-table"
  :lighter " xtras"
  :keymap (let ((map (make-sparse-keymap)))
		(define-key map (kbd "C-c C-= C-n") 'org-table-xtras-copy-field-next-row)
		(define-key map (kbd "C-c C-= C-f") 'org-table-xtras-copy-field-next-column)
		(define-key map (kbd "C-c C-= C-d") 'org-table-xtras-copy-field-diagonally)
		(define-key map (kbd "C-c C-= C-p") 'org-table-xtras-print-formulas)
		(define-key map (kbd "C-c C-= C-r") 'org-table-xtras-remove-fns)
		map))

(provide 'org-table-xtras-mode)
