# Reset all inputs

Reset all inputs to their default values. Note that
[`shinyjs::useShinyjs()`](https://rdrr.io/pkg/shinyjs/man/useShinyjs.html)
needs to be includes in the UI for this to work.

## Usage

``` r
reset_inputs(input, session, reset_input_ids = NULL)
```

## Arguments

- input:

  Shiny input.

- session:

  Shiny session.

- reset_input_ids:

  A character vector of input IDs to reset. If NULL, all inputs will be
  reset.
