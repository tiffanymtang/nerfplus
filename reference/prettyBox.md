# Pretty box

Wrapper around
[`shinydashboardPlus::box()`](https://shinydashboardPlus.rinterface.com/reference/box.html).

## Usage

``` r
prettyBox(
  ...,
  title = NULL,
  width = 12,
  color = NULL,
  status = "primary",
  solidHeader = TRUE,
  box_only = FALSE
)
```

## Arguments

- ...:

  Arguments passed to
  [`shinydashboardPlus::box()`](https://shinydashboardPlus.rinterface.com/reference/box.html).

- title:

  Optional title.

- width:

  The width of the box, using the Bootstrap grid system. This is used
  for row-based layouts. The overall width of a region is 12, so the
  default valueBox width of 4 occupies 1/3 of that width. For
  column-based layouts, use `NULL` for the width; the width is set by
  the column that contains the box.

- color:

  Box outline color.

- status:

  The status of the item This determines the item's background color.
  Valid statuses are defined as follows:

  - `primary`: \#3c8dbc

  - `success`: \#00a65a

  - `info`: \#00c0ef

  - `warning`: \#f39c12

  - `danger`: \#f56954

  - `navy`: \#001F3F

  - `teal`: \#39CCCC

  - `purple`: \#605ca8

  - `orange`: \#ff851b

  - `maroon`: \#D81B60

  - `black`: \#111111

  Only primary, success, info, warning and danger are compatible with
  solidHeader!

- solidHeader:

  Should the header be shown with a solid color background?

- box_only:

  If `TRUE`, only return the box and not the box wrapped inside
  [`shiny::fluidRow()`](https://rdrr.io/pkg/shiny/man/fluidPage.html).
