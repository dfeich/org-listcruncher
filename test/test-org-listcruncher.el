(require 'ert)
(require 'org-listcruncher)

(defvar testfile "./test-org-listcruncher.org")



(ert-deftest parsitem-default1 ()
  (should (equal
	   (org-listcruncher-parseitem-default
	    "item: First item (kCHF: 15, recurrence: 1, end-year: 2020)")
	   '(t "First item " (("kCHF" "15") ("recurrence" "1") ("end-year" "2020"))))))

(ert-deftest parsitem-default2 ()
  (let ((res (org-listcruncher-parseitem-default
	      "First item (kCHF: 15, recurrence: 1, end-year: 2020)")))
    (should (equal
	     (nth 2 res)
	     '(("kCHF" "15") ("recurrence" "1") ("end-year" "2020"))))
    (should (eq (car res) nil))))

(ert-deftest sparse-to-table1 ()
  (should (equal  (org-listcruncher--sparse-to-table '((("a" 1) ("b" 2))
						       (("c" 3) ("a" -1))))
		  '(("a" "b" "c")
		    hline
		    (1 2 "")
		    (-1 "" 3)))))

(ert-deftest sparse-to-table1 ()
  (should (equal  (org-listcruncher--sparse-to-table '((("a" 1) ("b" 2))
						       (("c" 3) ("a" -1)))
						     '("b" "c"))
		  '(("b" "c" "a")
		    hline (2 "" 1)
		    ("" 3 -1)))))

(ert-deftest integr-list-to-table ()
  (should (equal
	   (with-temp-buffer
	     (insert
	      "* Test list
   #+NAME: lsttest
   - item: First item (kCHF: 15, recurrence: 1, end-year: 2020)
     - modification of the first item (kCHF: 20)
     - another modification of the first item (other: 500)
       - modification of the modification (other: 299)
   - item: second item (kCHF: 50, recurrence: 4, end-year: 2026)
   - category (recurrence: 2, end-year: 2024)
     - item: a category item A (kCHF: 10)
     - item: a category item B (kCHF: 20)
     - item: a category item C (kCHF: 30)
       - a modification to category item C (kCHF: 25, recurrence: 3)
")
	     (org-mode)
	     (org-listcruncher-to-table "lsttest")
	     )
	   '(("description" "other" "kCHF" "recurrence" "end-year")
	     hline
	     ("First item " "299" "20" "1" "2020")
	     ("second item " "" "50" "4" "2026")
	     ("a category item A " "" "10" "2" "2024")
	     ("a category item B " "" "20" "2" "2024")
	     ("a category item C " "" "25" "3" "2024")))))
