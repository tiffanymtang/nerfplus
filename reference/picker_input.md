# Wrapper around `pickerInput()` to select one choice

Wrapper around `pickerInput()` to select one choice but with different
default settings.

## Usage

``` r
picker_input(inputId, label, choices, selected = NULL, options = NULL, ...)
```

## Arguments

- inputId:

  The `input` slot that will be used to access the value.

- label:

  Display label for the control, or `NULL` for no label.

- choices:

  List of values to select from. If elements of the list are named then
  that name rather than the value is displayed to the user.

- selected:

  The initially selected value (or multiple values if
  `multiple = TRUE`). If not specified then defaults to the first value
  for single-select lists and no values for multiple select lists.

- options:

  List of options, see
  [pickerOptions](https://dreamrs.github.io/shinyWidgets/reference/pickerOptions.html)
  for all available options. To limit the number of selection possible,
  see example below.

- ...:

  Additional arguments to pass to
  [`shinyWidgets::pickerInput()`](https://dreamrs.github.io/shinyWidgets/reference/pickerInput.html).
