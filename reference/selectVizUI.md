# Select visualization type (table, ggplot, plotly)

Select visualization type (table, ggplot, plotly)

## Usage

``` r
selectVizUI(
  id,
  choices = c("ggplot", "plotly", "table"),
  selected = NULL,
  individual = FALSE,
  size = "normal",
  justified = FALSE,
  float_right = FALSE,
  ...
)
```

## Arguments

- id:

  Unique identifier.

- choices:

  Choices must be some subset of "ggplot", "plotly", "table"

- selected:

  The initially selected value. If not specified, then it defaults to
  the first item in `choices`. To start with no items selected, use
  `character(0)`.

- individual:

  If TRUE, buttons are separated.

- size:

  Size of the buttons ('xs', 'sm', 'normal', 'lg')

- justified:

  If TRUE, fill the width of the parent div

- float_right:

  Logical. Whether or not to float the buttons to the right.

- ...:

  Additional arguments to pass to radio_group_buttons
