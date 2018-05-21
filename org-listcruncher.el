;;; org-listcruncher.el --- Parse emacs org list contents into table

;; Author: Derek Feichtinger <dfeich@gmail.com>
;; Keywords: convenience
;; Package-Requires: ((cl-lib "0.5") (helm "1.9.2") (emacs "24.4"))
;; Homepage: https://github.com/dfeich/org-listcruncher
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;; TODO
;; - allow user to define a preferred order for the output columns

;;; Code:
(require 'org)
(require 'cl-lib)

(defgroup org-listcruncher nil
  "Parses Org mode lists according to a parsing function and yields an org table structure."
  :group 'org :version 25.3)

(defcustom org-listcruncher-parse-fn #'org-listcruncher-parseitem-default
  "Function used for parsing list items." :group 'org-listcruncher)

(defun org-listcruncher-parseitem-default (line)
  "Default list item parsing function for org-listcruncher.

LINE is the list item to be parsed.  Outputting of a line is
triggered by having 'item:' at the start of the line.  The
description is a string.  The key value pairs are given after the
description in a format of (key1: val1, key2: val2, ...)."
  (let (outp descr varstr varlst)
    (if (string-match "^ *\\\(item:\\\)? *\\\([^(]*\\\) *\\\((\\\(.*\\\))\\\)?" line)
	(progn
	  (setq outp (if (match-string 1 line) t nil)
		descr (match-string 2 line)
		varstr (match-string 4 line))))
    (when varstr
      (setq varlst
	    (cl-loop for elem in (split-string varstr ", *")
		     collect (split-string elem " *: *") into result
		     finally return result)))
    (list outp descr varlst))
  )

(defun org-listcruncher--sparse-to-table (sparselst &optional order)
  "Return list of all unique keys of the list of alists in SPARSELST.

If a list is provided in the ORDER argument, the table columns
will be ordered according to this list.  The list may contain only
a subset of the items.  The remaining columns will be added in the
original order."
  (let* ((keylst
	  ;; list of all unique keys of the list of alists in SPARSELST
	  (cl-loop for alst in sparselst
		   with reslst = nil
		   collect (mapcar (lambda (kvpair) (car kvpair))  alst) into reslst
		   finally return (seq-uniq (apply #'append  reslst))))
	 (orderedlst (append order
			     (cl-loop for elm in order
				      do (setq keylst (delete elm keylst))
				      finally return keylst)))
	 ;; for each key, find var values in each given row in sparselist
	 (rows
	  (cl-loop for alst in sparselst
		   with reslst = nil
		   collect (mapcar (lambda (key) (or (cadr (assoc key alst))
						     ""))
				   orderedlst
				   ) into reslst
				     finally return reslst
				     )))
    (append `(,orderedlst) '(hline) rows)))


;;;###autoload
(defun org-listcruncher-to-table (listname &optional order)
  "Return a table structure based on parsing the Org list with name LISTNAME.

If a list is provided in the ORDER argument, the table columns
will be ordered according to this list.  The list may contain only
a subset of the items.  The remaining columns will be added in the
original order."
  (let ((lst
	 (save-excursion
	   (goto-char (point-min))
	   (unless (search-forward-regexp (concat  "^[ \t]*#\\\+NAME: .*" listname) nil t)
	     (error "No list of this name found: %s" listname))
	   (forward-line 1)
	   (org-list-to-lisp))))
    (org-listcruncher--sparse-to-table
     (cadr (org-listcruncher--parselist lst nil nil))
     order))
  )

(defun org-listcruncher--parselist (lst inheritvars resultlst)
  "Parse an org list into a table structure.

LST is a list as produced from `org-list-to-lisp'.  INHERITVARS is
an association list of (varname value) pairs that constitute the
inherited variable values from the parent.  RESULTLST contains the
current result structure in form of a list of association lists.  Each
contained association list corresponds to a later table row."
  (let ((ltype (car lst))
	(itemstructs (cdr lst))
	retvarlst)
    (setq retvarlst
	  (cl-loop for struct in itemstructs
		   with joinedsubvarlst = nil
		   do (let ((itemtext (car struct))
			    (sublist (cadr struct))
			    itemvarlst subtreevarlst outvarlst)
			;; parse this item
			(let* ((prsitem (apply org-listcruncher-parse-fn `(,itemtext)))
			       (outp (car prsitem))
			       (descr (nth 1 prsitem))
			       (itemvarlst (nth 2 prsitem)))
			  ;; (princ (format "DEBUG: item [%s] varlst: %s\n" descr itemvarlst))
			  ;; if item has a sublist, recurse with this sublist and get varlst of this tree
			  (when sublist
			    (let ((parseresult (org-listcruncher--parselist sublist
									    (append itemvarlst inheritvars)
									    resultlst)))
			      (setq subtreevarlst (car parseresult))
			      (setq resultlst (cadr parseresult)))
			    ;;(princ (format "DEBUG: received subtreevarlst %s\n" subtreevarlst))
			    )
			  ;; only prepare an output line if this item is flagged as an output item
			  (when outp
			    ;; the current item's description always is placed first in the list
			    (setq outvarlst (append `(("description" ,descr)) subtreevarlst itemvarlst inheritvars))
			    (setq resultlst (append resultlst (list outvarlst)))
			    ;; (princ (format "line: %s\n   varlst: %s\n"
			    ;; 		   descr
			    ;; 		   outvarlst))
			    )
			  ;; accumulate all item's varlists for returning to parent item
			  (setq joinedsubvarlst (append subtreevarlst itemvarlst joinedsubvarlst))))
		   ;; we return the consolidated varlst of this tree
		   finally return joinedsubvarlst))
    (list retvarlst resultlst)
    ))

(provide 'org-listcruncher)
;;; org-listcruncher.el ends here
