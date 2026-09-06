# Basic Table Options

Basic Table Options

## Usage

``` r
tableOptionsUI(
  id,
  digits = NA,
  digits_label = "Digits",
  sigfig = FALSE,
  sigfig_label = "Use Significant Digits",
  total_width = NULL
)
```

## Arguments

- id:

  Unique identifier.

- digits:

  Default number of digits to display.

- digits_label:

  Displayed label for digits input.

- sigfig:

  Logical. If `TRUE`, use significant digits.

- sigfig_label:

  Displayed label for significant digits checkbox.

- total_width:

  Total width of html element.

## Value

List of shiny tags.
