# Wrapper around `prettyRadioButtons()`

Wrapper around `prettyRadioButtons()` but with different default
settings.

## Usage

``` r
radio_buttons(
  inputId,
  label,
  choices,
  selected = NULL,
  status = "primary",
  animation = "jelly",
  icon = shiny::icon("check"),
  bigger = TRUE,
  ...
)
```

## Arguments

- inputId:

  The `input` slot that will be used to access the value.

- label:

  Display label for the control.

- choices:

  List of values to show radio buttons for. If elements of the list are
  named then that name rather than the value is displayed to the user.
  If this argument is provided, then `choiceNames` and `choiceValues`
  must not be provided, and vice-versa. The values should be strings;
  other types (such as logicals and numbers) will be coerced to strings.

- selected:

  The values that should be initially selected, (if not specified then
  defaults to the first value).

- status:

  Add a class to the radio, you can use Bootstrap status like 'info',
  'primary', 'danger', 'warning' or 'success'.

- animation:

  Add an animation when radio is checked, a value between `smooth`,
  `jelly`, `tada`, `rotate`, `pulse`.

- icon:

  Optional, display an icon on the radio, must be an icon created with
  `icon`.

- bigger:

  Scale the radio a bit bigger (`TRUE` or `FALSE`).

- ...:

  Additional arguments to pass to
  [`shinyWidgets::prettyRadioButtons()`](https://dreamrs.github.io/shinyWidgets/reference/prettyRadioButtons.html).
