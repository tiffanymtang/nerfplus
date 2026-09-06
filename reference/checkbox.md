# Wrapper around `prettyCheckbox()`

Wrapper around `prettyCheckbox()` with different default settings.

## Usage

``` r
checkbox(
  inputId,
  label,
  value = FALSE,
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

- value:

  Initial value (`TRUE` or `FALSE`).

- status:

  Add a class to the checkbox, you can use Bootstrap status like 'info',
  'primary', 'danger', 'warning' or 'success'.

- animation:

  Add an animation when checkbox is checked, a value between `smooth`,
  `jelly`, `tada`, `rotate`, `pulse`.

- icon:

  Optional, display an icon on the checkbox, must be an icon created
  with `icon`.

- bigger:

  Scale the checkboxes a bit bigger (`TRUE` or `FALSE`).

- ...:

  Additional arguments to pass to
  [`shinyWidgets::prettyCheckbox()`](https://dreamrs.github.io/shinyWidgets/reference/prettyCheckbox.html).
