# Wrapper around `radioGroupButtons()`

Wrapper around `radioGroupButtons()` but with different default
settings.

## Usage

``` r
radio_group_buttons(
  inputId,
  label,
  choices,
  selected = NULL,
  status = "default",
  size = "sm",
  justified = FALSE,
  ...
)
```

## Arguments

- inputId:

  The `input` slot that will be used to access the value.

- label:

  Display label for the control, or `NULL` for no label.

- choices:

  List of values to select from (if elements of the list are named then
  that name rather than the value is displayed to the user). If this
  argument is provided, then `choiceNames` and `choiceValues` must not
  be provided, and vice-versa. The values should be strings; other types
  (such as logicals and numbers) will be coerced to strings.

- selected:

  The initially selected value. If not specified, then it defaults to
  the first item in `choices`. To start with no items selected, use
  `character(0)`.

- status:

  Add a class to the buttons, you can use Bootstrap status like 'info',
  'primary', 'danger', 'warning' or 'success'. Or use an arbitrary
  strings to add a custom class, e.g. : with `status = "custom-class"`,
  buttons will have class `btn-custom-class`.

- size:

  Size of the buttons ('xs', 'sm', 'normal', 'lg')

- justified:

  If TRUE, fill the width of the parent div

- ...:

  Additional arguments to pass to
  [`shinyWidgets::radioGroupButtons()`](https://dreamrs.github.io/shinyWidgets/reference/radioGroupButtons.html).
