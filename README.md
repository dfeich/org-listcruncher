
# Table of Contents

1.  [Org listcruncher](#org1c7a274)
    1.  [Installation](#org0b7cd0e)
    2.  [Example usage](#org5bd6b01)
    3.  [List writing rules](#orgf52e97b)
    4.  [Using alternate parsing functions](#orge2c34b4)
    5.  [Configuration](#orgdab564d)
    6.  [Further examples](#orgfa5abf0)
        1.  [Adding a table formula to the resulting table](#org41f93e3)
    7.  [Changes](#org408dc59)
        1.  [version 1.0: API change](#org9f7fa3a)
        2.  [version 1.2: change for using operator values](#org14ee24e)
    8.  [Running tests](#org5f84de0)


<a id="org1c7a274"></a>

# Org listcruncher

[![img](https://travis-ci.org/dfeich/org-listcruncher.svg?branch=master)](https://travis-ci.org/dfeich/org-listcruncher)
[![img](https://melpa.org/packages/org-listcruncher-badge.svg)](https://melpa.org/#/org-listcruncher)

org-listcruncher is a planning tool. Planning using lists is a very
natural approach, and in terms of a data structure it is similar to
a mind map. Emacs Org-mode makes it very easy and efficient to work
with lists, since it offers a lot of functionality for restructuring
and modifying them.

org-listcruncher provides a way to convert such Org-mode lists into
table structure following specific semantics. This data structure
can then be operated on by other code blocks, or it can just be
exported. The list retains all the comments, you can override values
and keep the history of changes inside of it, while still being able
to derive an easy data structure as an input for other planning
tools downstream.

I've used it for the initial stage of planning for bigger projects.
It was ideal for producing the often complex tables required by
formal project proposal templates, and at some point I just exported
the basic structures as a good starting point for continuing with a
typical project management software.


<a id="org0b7cd0e"></a>

## Installation

You can get the package from [MELPA](https://melpa.org/#/org-listcruncher) using emacs' package manager.

If you are using John Wiegley's `use-package` (which I recommend), just put the following line
into your `~/emacs.d/init.el` (or `~/.emacs`)

    (use-package org-listcruncher)

Or more barebones, just `require` it.

    (require 'org-listcruncher)


<a id="org5bd6b01"></a>

## Example usage

Write a planning list and give it a name using the appropriate Org
syntax (e.g. `#+NAME: lstTest`). Here is an example (look at the
[raw form of this README.org](https://raw.githubusercontent.com/dfeich/org-listcruncher/master/README.org) to see the Org source buffer with all
the markup)

-   **item:** item X modified by replacing values (amount: 15, responsible: Peter, end-year: 2020)
    -   modification of item X (amount: 20)
    -   another modification of item X (other: 500)
        -   modification of the modification (other: 299)
-   illustrating inheritance (responsible: Mary, end-year: 2024)
    -   **item:** item A. Some longer explanation that may run over
        multiple lines (amount: 10)
    -   **item:** item B (amount: 20)
    -   **item:** item C (amount: 30)
        -   a modification to item C (amount: 25, responsible: Paul)
-   **item:** item Y modified by operations (amount: 50, responsible: Peter, end-year: 2026)
    -   modification by an operation (amount: +=50)
    -   modification by an operation (amount: \*=1.5)
-   **item:** item Z entered in scientific format (amount: 1e3, responsible: Mary, end-year: 2025)
    -   modification by an operation (amount: -=1e2)
-   **item:** illustrating += and -= with strings (amount: 1000, responsible: Peter, end-year: 2025)
    -   adding a string (responsible: +=Paul, end-year: +=1)
    -   adding a string (responsible: +=Mary, end-year: +=1)
    -   removing a string (responsible: -=Peter)

We can use org-listcruncher to convert this list into a table   

    (org-listcruncher-to-table listname)

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-left" />

<col  class="org-right" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">description</th>
<th scope="col" class="org-right">other</th>
<th scope="col" class="org-right">amount</th>
<th scope="col" class="org-left">responsible</th>
<th scope="col" class="org-right">end-year</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">item X modified by replacing values</td>
<td class="org-right">299</td>
<td class="org-right">20</td>
<td class="org-left">Peter</td>
<td class="org-right">2020</td>
</tr>


<tr>
<td class="org-left">item A</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">10</td>
<td class="org-left">Mary</td>
<td class="org-right">2024</td>
</tr>


<tr>
<td class="org-left">item B</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">20</td>
<td class="org-left">Mary</td>
<td class="org-right">2024</td>
</tr>


<tr>
<td class="org-left">item C</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">25</td>
<td class="org-left">Paul</td>
<td class="org-right">2024</td>
</tr>


<tr>
<td class="org-left">item Y modified by operations</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">150.0</td>
<td class="org-left">Peter</td>
<td class="org-right">2026</td>
</tr>


<tr>
<td class="org-left">item Z entered in scientific format</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">900.0</td>
<td class="org-left">Mary</td>
<td class="org-right">2025</td>
</tr>


<tr>
<td class="org-left">illustrating += and -= with strings</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">1000</td>
<td class="org-left">Paul Mary</td>
<td class="org-right">2027</td>
</tr>
</tbody>
</table>

We can also provide an additional argument to affect the order of
columns in which the table is produced.

    (org-listcruncher-to-table listname :order '("description" "amount" "responsible"))

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-right" />

<col  class="org-left" />

<col  class="org-right" />

<col  class="org-right" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">description</th>
<th scope="col" class="org-right">amount</th>
<th scope="col" class="org-left">responsible</th>
<th scope="col" class="org-right">other</th>
<th scope="col" class="org-right">end-year</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">item X modified by replacing values</td>
<td class="org-right">20</td>
<td class="org-left">Peter</td>
<td class="org-right">299</td>
<td class="org-right">2020</td>
</tr>


<tr>
<td class="org-left">item A</td>
<td class="org-right">10</td>
<td class="org-left">Mary</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">2024</td>
</tr>


<tr>
<td class="org-left">item B</td>
<td class="org-right">20</td>
<td class="org-left">Mary</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">2024</td>
</tr>


<tr>
<td class="org-left">item C</td>
<td class="org-right">25</td>
<td class="org-left">Paul</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">2024</td>
</tr>


<tr>
<td class="org-left">item Y modified by operations</td>
<td class="org-right">150.0</td>
<td class="org-left">Peter</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">2026</td>
</tr>


<tr>
<td class="org-left">item Z entered in scientific format</td>
<td class="org-right">900.0</td>
<td class="org-left">Mary</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">2025</td>
</tr>


<tr>
<td class="org-left">illustrating += and -= with strings</td>
<td class="org-right">1000</td>
<td class="org-left">Paul Mary</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">2027</td>
</tr>
</tbody>
</table>

It is also possible to directly obtain single table field values based on defining the
row and column through the string corresponding to an item's description and its
column name:

    (org-listcruncher-get-field listname "item B" "amount")

    20


<a id="orgf52e97b"></a>

## List writing rules

The rules for writing such a planning list are

1.  Each line contains a tag defining whether the line will become a table row. For this
    example I defined this as the string "item:". Rows without such a tag just serve as
    metadata.
2.  A string following the output tag "item:" is taken as the description of the table row.
3.  Each line can contain any number of key/value pairs in parentheses in the form
    `(key1: val1, key2: val2, ...)`
4.  Lines of lower hierarchical order in the list inherit their default settings for key/values
    from the upper items.
5.  The key value of a higher order item can be overwritten by a new new value for the same key
    in a lower order line.
6.  If a given value is of the form +=10, -=10, /=10, \*=10, i.e. an operator followed by a number,
    the operation is carried out on the previous value of the respective key.
    (Note: this changed in version 1.2, since the original use of "-10" did not
    allow differentiating between subtracting 10 or setting value to "-10". The
    old syntax is still allowed for all operators except "-")
7.  If a given value is of the form +=word then "word" is
    added to the previous string value for this key, using space as a separator. If
    -=word is used, then "word" is removed from the previous string
    value. This allows building lists of words.


<a id="orge2c34b4"></a>

## Using alternate parsing functions

You can define arbitrary parsing functions for the list items. They must
obey the following API:

The function receives a list item (a string) as its single
argument. It must return a list (`OUTP, DESCR, VARLST`), where

-   `OUTP` is a boolean indicating whether this list item will become a table
    row
-   `DESCR` is the description string appearing in the table's "description" column
    (so this is only relevant for OUTP=True lines)
-   `VARLST` is the list of key/value pairs corresponding to the column name /
    values.

Simple example functions for this purpose can be generated using
the `org-listcruncher-mk-parseitem-default` generator function. It
allows modifying the tag that decides whether a list item will
become a table row. It also permits changing the description's
terminating tag and the brackets for the key/value pairs. E.g. if I
would like to match for "row:" instead for "item:", and if I would
like to use square brackets, I can obtain such a function by
executing.

    (org-listcruncher-mk-parseitem-default :tag"\\*?row:\\*?" :bra "[" :ket "]")

Let's test it using this modified list:

-   **row:** item X modified by replacing values [amount: 15, recurrence: 1, end-year: 2020].
    -   modification of item X [amount: 20]
    -   another modification of item X [other: 500]
        -   modification of the modification [other: 299]
-   illustrating inheritance [recurrence: 2, end-year: 2024]
    -   **row:** item A. Some longer explanation that may run over
        multiple lines [amount: 10]
    -   **row:** item B [amount: 20]
    -   **row:** item C [amount: 30]
        -   a modification to item C [amount: 25, recurrence: 3]
-   **row:** item Y modified by operations [amount: 50, recurrence: 4, end-year: 2026]
    -   modification by an operation [amount: +50]
    -   modification by an operation [amount: \*1.5]
-   **row:** item Z entered in scientific format [amount: 1e3, recurrence: 3, end-year: 2025]
    -   modification by an operation [amount: -=1e2]

We invoke org-listcruncher with the above parsing function:

    (org-listcruncher-to-table listname
                               :parsefn (org-listcruncher-mk-parseitem-default
                                         :tag "\\*?row:\\*?"
                                         :bra "["
                                         :ket "]")
                               :order '("description" "amount" "recurrence"))

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

And another variant allowing to write the list with minimal markup for the tag:
Here any line beginning with a bold markup string becomes a row with the description
being taken as that string. I just define as tag/endtag the markup character "\*".

-   Defaults (color: white, form: cube, weight: 10)
    -   **one item is heavy** (weight: 20)
    -   **another is lighter** (weight: 5)
        -   it has other distinguishing features (color: green, form: disk)
    -   **item three** is the default

    (org-listcruncher-to-table listname
                                 :parsefn (org-listcruncher-mk-parseitem-default
                                           :tag "\\*"
                                           :endtag "\\*"
                                           :bra "("
                                           :ket ")"))

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-right" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">description</th>
<th scope="col" class="org-right">weight</th>
<th scope="col" class="org-left">color</th>
<th scope="col" class="org-left">form</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">one item is heavy</td>
<td class="org-right">20</td>
<td class="org-left">white</td>
<td class="org-left">cube</td>
</tr>


<tr>
<td class="org-left">another is lighter</td>
<td class="org-right">5</td>
<td class="org-left">green</td>
<td class="org-left">disk</td>
</tr>


<tr>
<td class="org-left">item three</td>
<td class="org-right">10</td>
<td class="org-left">white</td>
<td class="org-left">cube</td>
</tr>
</tbody>
</table>


<a id="orgdab564d"></a>

## Configuration

The way that the table structure is created from the list can be
customized by providing own implementations of the parsing function
and of the consolidation function that combines the parsed
key/value pairs into a table.

The current implementations are examples that are sufficient for
the above use cases.

One can easily imagine much more sophisticated parsing
functions which e.g. could be applied to a cooking recipe written
with minimal concessions as to syntax. From such a recipe one could
then derive a table of ingredients, their amounts, and cooking
times; all ready for being displayed as a table, to calculate the
adapted amounts according to the number of expected guests, and
entering the items onto your shopping list.

I am planning to provide more sophisticated parsing and
consolidation functions to choose from (and naturally would be
happy to receive any additions from contributors).

The default functions that are used can be configured using
the following customization variables.

-   **`org-listcruncher-parse-fn`:** This variable defines the default
    parsing function to use if you call the org-listcruncher
    functions without an explicit `:parsefn` keyword agument.

-   **org-listcruncher-consolidate-fn:** This variable defines the
    default function for consolidating all the values that a certain
    key got assigned for a list item. The function must accept two
    arguments: KEY and LIST. KEY is the key (i.e. column value) of
    the row that one is interested in. LIST contains all the values
    for the KEY in that row, i.e. it will contain any redefinitions
    of the key value in subitems of this list item. The consolidation
    function basically defines how these values get combined into the
    single value that we will assign to the column in this row. The
    default function either replaces the previous value or allows
    values with operators (e.g. +=10, \*=0.5) to modify the previous
    value. Refer to the default function
    `org-listcruncher-consolidate-default` documentation.


<a id="orgfa5abf0"></a>

## Further examples


<a id="org41f93e3"></a>

### Adding a table formula to the resulting table

The primary goal of `org-listcruncher-to-table` is to return a data structure
(an org table structure) that can be used for further processing by code, e.g.
in a babel block.

Often, one will be mainly interested in a fast way to produce an
org table that one immediately wants to process with the standard
org table functions. Here is an easy recipe that adds a table row
for the added up total and an according org table formula.

    (princ (orgtbl-to-orgtbl
       (append (org-listcruncher-to-table listname) '(hline ("Total")))
       '(:tend "#+TBLFM: @>$3=vsum(@I..@II)")))

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-left" />

<col  class="org-right" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">description</th>
<th scope="col" class="org-right">other</th>
<th scope="col" class="org-right">amount</th>
<th scope="col" class="org-left">responsible</th>
<th scope="col" class="org-right">end-year</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">item X modified by replacing values</td>
<td class="org-right">299</td>
<td class="org-right">20</td>
<td class="org-left">Peter</td>
<td class="org-right">2020</td>
</tr>


<tr>
<td class="org-left">item A</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">10</td>
<td class="org-left">Mary</td>
<td class="org-right">2024</td>
</tr>


<tr>
<td class="org-left">item B</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">20</td>
<td class="org-left">Mary</td>
<td class="org-right">2024</td>
</tr>


<tr>
<td class="org-left">item C</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">25</td>
<td class="org-left">Paul</td>
<td class="org-right">2024</td>
</tr>


<tr>
<td class="org-left">item Y modified by operations</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">150.0</td>
<td class="org-left">Peter</td>
<td class="org-right">2026</td>
</tr>


<tr>
<td class="org-left">item Z entered in scientific format</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">900.0</td>
<td class="org-left">Mary</td>
<td class="org-right">2025</td>
</tr>


<tr>
<td class="org-left">illustrating += and -= with strings</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">1000</td>
<td class="org-left">Paul Mary</td>
<td class="org-right">2027</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">Total</td>
<td class="org-right">&#xa0;</td>
<td class="org-right">2125.</td>
<td class="org-left">&#xa0;</td>
<td class="org-right">&#xa0;</td>
</tr>
</tbody>
</table>

**Note:** Since this source block is not returning a table data structure, but an
already rendered org table string, one needs to use `:results output`. Since we
also do not want the result to be put into an org example block, we also need
to add the `raw` flag. So, the whole org block looks like this.

    #+BEGIN_SRC elisp :results output raw :var listname="lstTest" :exports both :post lobPostAlignTables(*this*)
      (princ (orgtbl-to-orgtbl
            (append (org-listcruncher-to-table listname) '(hline ("Total")))
            '(:tend "#+TBLFM: @>$3=vsum(@I..@II)")))
    #+END_SRC

In this block I also use a post function: `:post lobPostAlignTables`. This function will
format the table and trigger the formula calculation, so that the user does not need to
do it himself. This function and others can be obtained from [my library of babel example.](https://github.com/dfeich/org-babel-examples/blob/master/library-of-babel/dfeich-lob.org)


<a id="org408dc59"></a>

## Changes


<a id="org9f7fa3a"></a>

### version 1.0: API change

I apologize for a backwards incompatible API change for
`org-listcruncher-to-table listname` and
`org-listcruncher-get-field listname`, which now both accept
keyword parameters. This will make the functions more future proof
when further function arguments need to be introduced.


<a id="org14ee24e"></a>

### version 1.2: change for using operator values

The original syntax of e.g. "-10" did not allow differentiating
between subtracting 10 or setting value to "-10". Therefore the
operator use is now defined using the operator followed by the
equal sign: `-=`, `*=`, etc. The old syntax is still
working to keep backward compatibility, but it is discouraged.


<a id="org5f84de0"></a>

## Running tests

If you have a local cask installation you can run `make test`. Else, you
can invoke the test manually like this

    emacs --batch -q -l org-listcruncher.el -l test/test-org-listcruncher.el \
          --eval "(ert-run-tests-batch-and-exit test-order)"

