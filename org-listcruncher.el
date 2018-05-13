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

;;; Code:
(require 'org)
(require 'cl-lib)

(defun org-listcruncher-parseitem-default (line)
  (let (outp descr varstr varlst)
    (if (string-match "\\\(item:\\\)? *\\\([^(]*\\\) *\\\((\\\(.*\\\))\\\)?" line)
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

(defun org-listcruncher-parselist (lst inheritvars)
  (let ((ltype (car lst))
	(itemstructs (cdr lst)))
    (cl-loop for struct in itemstructs
	     with joinedsubvarlst = nil
	     do (let ((itemtext (car struct))
		      (sublist (cadr struct))
		      itemvarlst subtreevarlst outvarlst)
		  ;; parse this item
		  (let* ((prsitem (org-listcruncher-parseitem-default itemtext))
			 (outp (car prsitem))
			 (descr (nth 1 prsitem))
			 (itemvarlst (nth 2 prsitem)))
		    ;; (princ (format "DEBUG: item [%s] varlst: %s\n" descr itemvarlst))
		    ;; if item has a sublist, recurse with this sublist and get varlst of this tree
		    (when sublist
		      (setq subtreevarlst (org-listcruncher-parselist sublist
								      (append itemvarlst inheritvars)))
		      ;;(princ (format "DEBUG: received subtreevarlst %s\n" subtreevarlst))
		      )
		    ;; only prepare an output line if this item is flagged as an output item
		    (when outp
		      (setq outvarlst (append subtreevarlst itemvarlst inheritvars))
		      (princ (format "line: %s\n   varlst: %s\n"
				     descr
				     outvarlst
				     )))
		    ;; accumulate all item's varlists for returning to parent item
		    (setq joinedsubvarlst (append subtreevarlst itemvarlst joinedsubvarlst))))
	     ;; we return the consolidated varlst of this tree
	     finally return joinedsubvarlst)))

(provide 'org-listcruncher)
;;; org-listcruncher.el ends here
