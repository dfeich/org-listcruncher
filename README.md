
# Table of Contents

1.  [Org listcruncher](#org5382e12)
    1.  [Installation](#orgf9da8f9)
    2.  [Example usage](#org7ba79f2)


<a id="org5382e12"></a>

# Org listcruncher

[![img](https://travis-ci.org/dfeich/org-listcruncher.svg?branch=master)](https://travis-ci.org/dfeich/org-listcruncher.svg?branch=master)

org-listcruncher provides a way to convert org-mode lists into
a table structure following specific semantics. 


<a id="orgf9da8f9"></a>

## Installation

I will submit ist as a MELPA package.

If you want to test it already, just clone this repository, make sure that it is
in your emacs search path and load the package using

    (require 'org-listcruncher)


<a id="org7ba79f2"></a>

## Example usage

Write a planning list and give it a name using the appropriate Org syntax (e.g. `#+NAME: lstTest`).
Here is an example

-   item: item X modified by replacing values (amount: 15, recurrence: 1, end-year: 2020)
    -   modification of item X (amount: 20)
    -   another modification of item X (other: 500)
        -   modification of the modification (other: 299)
-   illustrating inheritance (recurrence: 2, end-year: 2024)
    -   item: item A. Some longer explanation that may run over
        multiple lines (amount: 10)
    -   item: item B (amount: 20)
    -   item: item C (amount: 30)
        -   a modification to item C (amount: 25, recurrence: 3)
-   item: item Y modified by operations (amount: 50, recurrence: 4, end-year: 2026)
    -   modification by an operation (amount: +50)
    -   modification by an operation (amount: \*1.5)
-   item: item Z entered in scientific format (amount: 1e3, recurrence: 3, end-year: 2025)
    -   modification by an operation (amount: -1e2)

We can use org-listcruncher to convert this list into a table   

    (org-listcruncher-to-table listname)

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">description</th>
<th scope="col" class="org-right">other</th>
<th scope="col" class="org-right">amount</th>
<th scope="col" class="org-right">recurrence</th>
<th scope="col" class="org-right">end-year</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">item X modified by replacing values</td>
<td class="org-right">299</td>
<td class="org-right">20</td>
<td class="org-right">1</td>
<td class="org-right">2020</td>
</tr>


<tr>
<td class="org-left">item A</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">10</td>
<td class="org-right">2</td>
<td class="org-right">2024</td>
</tr>


<tr>
<td class="org-left">item B</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">20</td>
<td class="org-right">2</td>
<td class="org-right">2024</td>
</tr>


<tr>
<td class="org-left">item C</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">25</td>
<td class="org-right">3</td>
<td class="org-right">2024</td>
</tr>


<tr>
<td class="org-left">item Y modified by operations</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">150.0</td>
<td class="org-right">4</td>
<td class="org-right">2026</td>
</tr>


<tr>
<td class="org-left">item Z entered in scientific format</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">900.0</td>
<td class="org-right">3</td>
<td class="org-right">2025</td>
</tr>
</tbody>
</table>

The rules for writing such a planning list are

1.  Each line contains a tag defining wheter the line will become a table row. For this
    example I defined this as the string "item:". Rows without such a tag just serve as
    metadata.
2.  A string following the output tag "item:" is taken as the description of the table row.
3.  Each line can contain any number of key/value pairs in parentheses in the form
    `(key1: val1, key2: val2, ...)`
4.  Lines of lower hierarchical order in the list inherit their default settings for key/values
    from the upper items.
5.  The key value of a higher order item can be overwritten by a new new value for the same key
    in a lower order line.
6.  If a given value is of the form +10, -10, /10, \*10, i.e. an operator followed by a number,
    the operation is carried out on the previous value of the respective key.

We can also provide an additional argument to affect the order of
columns in which the table is produced.

    (org-listcruncher-to-table listname '("description" "amount" "recurrence"))

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">description</th>
<th scope="col" class="org-right">amount</th>
<th scope="col" class="org-right">recurrence</th>
<th scope="col" class="org-right">other</th>
<th scope="col" class="org-right">end-year</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">item X modified by replacing values</td>
<td class="org-right">20</td>
<td class="org-right">1</td>
<td class="org-right">299</td>
<td class="org-right">2020</td>
</tr>


<tr>
<td class="org-left">item A</td>
<td class="org-right">10</td>
<td class="org-right">2</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">2024</td>
</tr>


<tr>
<td class="org-left">item B</td>
<td class="org-right">20</td>
<td class="org-right">2</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">2024</td>
</tr>


<tr>
<td class="org-left">item C</td>
<td class="org-right">25</td>
<td class="org-right">3</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">2024</td>
</tr>


<tr>
<td class="org-left">item Y modified by operations</td>
<td class="org-right">150.0</td>
<td class="org-right">4</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">2026</td>
</tr>


<tr>
<td class="org-left">item Z entered in scientific format</td>
<td class="org-right">900.0</td>
<td class="org-right">3</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">2025</td>
</tr>
</tbody>
</table>

It is also possible to directly obtain single table field values based on defining the
row and column through the string corresponding to an item's description and its
column name:

    (org-listcruncher-get-field listname "item B" "amount")

    20

