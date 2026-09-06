# Add spinner when loading

Wrapper around
[`shinycssloaders::withSpinner()`](https://rdrr.io/pkg/shinycssloaders/man/withSpinner.html)
with a different default color.

## Usage

``` r
add_spinner(obj, spinner = TRUE, color = "#18bc9c")
```

## Arguments

- obj:

  Object to add spinner to.

- spinner:

  Logical. Whether or not to add spinner.

- color:

  The color of the spinner in hex format. Ignored if `image` is used.

## Examples

``` r
## DON'T RUN
# shiny::htmlOutput({NAME OF UI ELEMENT}) |> add_spinner()
```
