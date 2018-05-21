
# Table of Contents

1.  [Org listcruncher](#org35a3980)
    1.  [Example usage](#org23bde50)


<a id="org35a3980"></a>

# Org listcruncher

org-listcruncher provides a way to convert org-mode lists into
a table structure following specific semantics.

This is currently still in the initial development. As soon as I have completed the
main features, I will submit ist as a MELPA package.

If you want to test it already, just clone this repository, make sure that it is
in your emacs search path and load the package using

    (require 'org-listcruncher)


<a id="org23bde50"></a>

## Example usage

Write a planning list and give it a name using the appropriate Org syntax (e.g. `#+NAME: lsttest`).
Here is an example

-   item: First item (kCHF: 15, recurrence: 1, until: 2020)
    -   modification of the first item (kCHF: 20)
    -   another modification of the first item (other: 500)
        -   modification of the modification (other: 299)
-   item: second item (kCHF: 50, recurrence: 4)
-   category (recurrence: 5)
    -   item: a category item A (kCHF: 10)
    -   item: a category item B (kCHF: 20)
    -   item: a category item C (kCHF: 30)
        -   a modification to category item C (kCHF: 25, recurrence: 3)

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

Now we can use org-listcruncher to convert this list into a table   

    (org-listcruncher-to-table lname)

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
<th scope="col" class="org-right">kCHF</th>
<th scope="col" class="org-right">recurrence</th>
<th scope="col" class="org-right">until</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">First item</td>
<td class="org-right">299</td>
<td class="org-right">20</td>
<td class="org-right">1</td>
<td class="org-right">2020</td>
</tr>


<tr>
<td class="org-left">second item</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">50</td>
<td class="org-right">4</td>
<td class="org-right">&#xa0;</td>
</tr>


<tr>
<td class="org-left">a category item A</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">10</td>
<td class="org-right">5</td>
<td class="org-right">&#xa0;</td>
</tr>


<tr>
<td class="org-left">a category item B</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">20</td>
<td class="org-right">5</td>
<td class="org-right">&#xa0;</td>
</tr>


<tr>
<td class="org-left">a category item C</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">25</td>
<td class="org-right">3</td>
<td class="org-right">&#xa0;</td>
</tr>
</tbody>
</table>

We can also provide an additional argument to affect the order in which the table is rendered.

    (org-listcruncher-to-table lname '("description" "kCHF" "recurrence"))

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
<th scope="col" class="org-right">kCHF</th>
<th scope="col" class="org-right">recurrence</th>
<th scope="col" class="org-right">other</th>
<th scope="col" class="org-right">until</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">First item</td>
<td class="org-right">20</td>
<td class="org-right">1</td>
<td class="org-right">299</td>
<td class="org-right">2020</td>
</tr>


<tr>
<td class="org-left">second item</td>
<td class="org-right">50</td>
<td class="org-right">4</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">&#xa0;</td>
</tr>


<tr>
<td class="org-left">a category item A</td>
<td class="org-right">10</td>
<td class="org-right">5</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">&#xa0;</td>
</tr>


<tr>
<td class="org-left">a category item B</td>
<td class="org-right">20</td>
<td class="org-right">5</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">&#xa0;</td>
</tr>


<tr>
<td class="org-left">a category item C</td>
<td class="org-right">25</td>
<td class="org-right">3</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">&#xa0;</td>
</tr>
</tbody>
</table>

