# Wrapper around `materialSwitch()`

Wrapper around `materialSwitch()` with different default settings.

## Usage

``` r
material_switch(
  inputId,
  label,
  value = TRUE,
  status = "primary",
  right = FALSE,
  inline = FALSE,
  ...
)
```

## Arguments

- inputId:

  The `input` slot that will be used to access the value.

- label:

  Input label.

- value:

  TRUE or FALSE.

- status:

  Color, must be a valid Bootstrap status : default, primary, info,
  success, warning, danger.

- right:

  Should the the label be on the right? default to FALSE.

- inline:

  Display the input inline, if you want to place buttons next to each
  other.

- ...:

  Additional arguments to pass to
  [`shinyWidgets::materialSwitch()`](https://dreamrs.github.io/shinyWidgets/reference/materialSwitch.html).
