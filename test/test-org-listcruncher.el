(require 'ert)
(require 'org-listcruncher)
(require 'seq)

(defvar test-order '(member
		     parseitem-default1
		     parseitem-default2
		     parseitem-default3
		     parseitem-default4
		     mk-parseitem-default1
		     mk-parseitem-default2
		     mk-parseitem-default3
		     sparse-to-table1
		     sparse-to-table2
		     consolidate-vals1
		     integr-get-field1
		     integr-list-to-table1))


(defvar testfile "./test-org-listcruncher.org")
(defvar testlist1 "*Test

   #+NAME: lstTest
   - item: item X modified by replacing values (amount: 15, recurrence: 1, end-year: 2020)
     - modification of item X (amount: 20)
     - another modification of item X (other: 500)
       - modification of the modification (other: 299)
   - illustrating inheritance (recurrence: 2, end-year: 2024)
     - item: item A. Some longer explanation that may run over
       multiple lines. Let's add another line for
       good measure (amount: 10)
     - item: item B (amount: 20)
     - item: item C (amount: 30)
       - a modification to item C (amount: 25, recurrence: 3)
   - item: item Y modified by operations (amount: 50, recurrence: 4, end-year: 2026)
     - modification by an operation (amount: +50)
     - modification by an operation (amount: *1.5)
   - item: item Z entered in scientific format (amount: 1e3, recurrence: 3, end-year: 2025)
     - modification by an operation (amount: -1e2)

")


(ert-deftest parseitem-default1 ()
  (should (equal
	   (org-listcruncher-parseitem-default
	    "item: First item (amount: 15, recurrence: 1, end-year: 2020)")
	   '(t "First item" (("amount" "15") ("recurrence" "1") ("end-year" "2020"))))))

(ert-deftest parseitem-default2 ()
  (should (equal
	   (org-listcruncher-parseitem-default
	    "*item:* First item (amount: 15, recurrence: 1, end-year: 2020)")
	   '(t "First item" (("amount" "15") ("recurrence" "1") ("end-year" "2020"))))))

(ert-deftest parseitem-default3 ()
  (let ((res (org-listcruncher-parseitem-default
	      "First item (amount: 15, recurrence: 1, end-year: 2020)")))
    (should (equal
	     (nth 2 res)
	     '(("amount" "15") ("recurrence" "1") ("end-year" "2020"))))
    (should (eq (car res) nil))))

;; test for more restrictive parsing of the key/val pairs syntax
(ert-deftest parseitem-default4 ()
  (should (equal
	   (org-listcruncher-parseitem-default
	    "*item:* First item (amount 15, recurrence 1, end-year 2020)")
	   '(t "First item" nil))))

(ert-deftest mk-parseitem-default1 ()
  (should (equal
	   (funcall (org-listcruncher-mk-parseitem-default :tag "row:")
		    "row: First item (amount: 15, recurrence: 1, end-year: 2020)")
	   '(t "First item" (("amount" "15") ("recurrence" "1") ("end-year" "2020"))))))

(ert-deftest mk-parseitem-default2 ()
  (should (equal
	   (funcall (org-listcruncher-mk-parseitem-default :tag "row:"
							   :bra "<<"
							   :ket ">>")
		    "row: First item <<amount: 15, recurrence: 1, end-year: 2020>>")
	   '(t "First item" (("amount" "15") ("recurrence" "1") ("end-year" "2020"))))))

(ert-deftest mk-parseitem-default3 ()
  (should (equal
	   (funcall (org-listcruncher-mk-parseitem-default :tag "\\*"
            						   :endtag "\\*"
							   :bra "("
							   :ket ")")
		    "*one item is heavy* and colored (weight: 20, color: green)")
	   '(t "one item is heavy" (("weight" "20") ("color" "green"))))))


(ert-deftest sparse-to-table1 ()
  (should (equal  (org-listcruncher--sparse-to-table '((("a" 1) ("b" 2))
						       (("c" 3) ("a" -1))))
		  '(("a" "b" "c")
		    hline
		    (1 2 "")
		    (-1 "" 3)))))

(ert-deftest sparse-to-table2 ()
  (should (equal  (org-listcruncher--sparse-to-table '((("a" 1) ("b" 2))
						       (("c" 3) ("a" -1)))
						     '("b" "c"))
		  '(("b" "c" "a")
		    hline (2 "" 1)
		    ("" 3 -1)))))

(ert-deftest consolidate-vals1 ()
  (should
   (equal
    100.0
    (org-listcruncher-consolidate-default "amount" '(("description" "First item ")
						     ("amount" "/2")
						     ("amount" "+100")
						     ("amount" "1e2")
						     ("amount" "+20")
						     ("amount" "123")
						     ("recurrence" "1")
						     ("end-year" "2020"))))))


(ert-deftest integr-get-field1 ()
  (should (equal
	   (with-temp-buffer
	     (insert testlist1)
	     (org-mode)
	     (org-listcruncher-get-field "lstTest" "item B" "amount"))
	   "20")))

(ert-deftest integr-list-to-table1 ()
  (should (equal
	   (with-temp-buffer
	     (insert testlist1)
	     (org-mode)
	     (org-listcruncher-to-table "lstTest"))
	   '(("description" "other" "amount" "recurrence" "end-year")
	     hline
	     ("item X modified by replacing values" "299" "20" "1" "2020")
	     ("item A" "" "10" "2" "2024")
	     ("item B" "" "20" "2" "2024")
	     ("item C" "" "25" "3" "2024")
	     ("item Y modified by operations" "" 150.0 "4" "2026")
	     ("item Z entered in scientific format" "" 900.0 "3" "2025")))))
