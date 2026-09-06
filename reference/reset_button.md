# Reset button

Customized action button using `actionButton()`.

## Usage

``` r
reset_button(inputId = "reset_input", label = "Reset all inputs", ...)
```

## Arguments

- inputId:

  The `input` slot that will be used to access the value.

- label:

  The contents of the button or link–usually a text label, but you could
  also use any other HTML, like an image.

- ...:

  Additional arguments to pass to
  [`shiny::actionButton()`](https://rdrr.io/pkg/shiny/man/actionButton.html).
