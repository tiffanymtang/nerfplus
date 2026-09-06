# Basic Plot Options

Basic Plot Options

## Usage

``` r
plotOptionsUI(
  id,
  multicol = FALSE,
  heatmap = FALSE,
  strip_options = FALSE,
  total_width = 12,
  height = 500,
  x_axis_text_size = 12,
  y_axis_text_size = 12,
  legend_text_size = 12,
  strip_text_size = 14,
  x_axis_title_size = 14,
  y_axis_title_size = 14,
  legend_title_size = 14,
  title_size = 16,
  axis_line_width = 1,
  x_text_angle = FALSE,
  strip_text_color = "white",
  bg_color = "grey98",
  other_options = NULL
)
```

## Arguments

- id:

  Unique identifier.

- multicol:

  Logical. If `TRUE`, use multiple columns.

- heatmap:

  Logical. If `TRUE`, include heatmap options.

- strip_options:

  Logical. If `TRUE`, include strip options.

- total_width:

  Total width of html element.

- height:

  Height of html element.

- x_axis_text_size:

  Default size of x-axis text.

- y_axis_text_size:

  Default size of y-axis text.

- legend_text_size:

  Default size of legend text.

- strip_text_size:

  Default size of strip text.

- x_axis_title_size:

  Default size of x-axis title.

- y_axis_title_size:

  Default size of y-axis title.

- legend_title_size:

  Default size of legend title.

- title_size:

  Default size of title.

- axis_line_width:

  Default width of axis lines.

- x_text_angle:

  Default angle of x-axis text. If `FALSE`, x-axis text is horizontal.
  Otherwise, it is angled 45 degrees.

- strip_text_color:

  Default color of strip text.

- bg_color:

  Default background color.

- other_options:

  Additional shiny widgets/tags to add to plot options.

## Value

List of shiny tags.
