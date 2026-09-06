# Pretty dashboard header

Wrapper around
[`shinydashboard::dashboardHeader()`](https://rdrr.io/pkg/shinydashboard/man/dashboardHeader.html)
that allows for the title to be positioned to the left or center.

## Usage

``` r
prettyDashboardHeader(
  title,
  title_position = c("left", "center"),
  title_style = list(),
  ...
)
```

## Arguments

- title:

  An optional title to show in the header bar.. By default, this will
  also be used as the title shown in the browser's title bar. If you
  want that to be different from the text in the dashboard header bar,
  set the `title` in
  [`dashboardPage`](https://rdrr.io/pkg/shinydashboard/man/dashboardPage.html).

- title_position:

  Position of the title, either "left" or "center".

- title_style:

  (Optional) list of CSS styling options for the title.

- ...:

  Arguments passed to
  [`shinydashboard::dashboardHeader()`](https://rdrr.io/pkg/shinydashboard/man/dashboardHeader.html).
