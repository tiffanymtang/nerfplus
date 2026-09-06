# Run Shiny NeRF+ Interpret application

Run Shiny NeRF+ Interpret application

## Usage

``` r
run_app(
  data_list = NULL,
  object = NULL,
  fi_results = NULL,
  loo_results = NULL,
  max_request_size = 100 * 1024^2,
  ...
)
```

## Arguments

- data_list:

  A list containing the data used for interpretation. Should have the
  same format as the `data_list` output from
  [`interpret_nerfplus()`](https://tiffanymtang.github.io/nerfplus/reference/interpret_nerfplus.md).

- object:

  The fitted NeRF+ model. Should have the same format as the `object`
  output from
  [`interpret_nerfplus()`](https://tiffanymtang.github.io/nerfplus/reference/interpret_nerfplus.md).

- fi_results:

  A list containing the feature importance results. Should have the same
  format as the `fi_results` output from
  [`interpret_nerfplus()`](https://tiffanymtang.github.io/nerfplus/reference/interpret_nerfplus.md).

- loo_results:

  A list containing the sample influence results. Should have the same
  format as the `loo_results` output from
  [`interpret_nerfplus()`](https://tiffanymtang.github.io/nerfplus/reference/interpret_nerfplus.md).

- max_request_size:

  Maximum request size for file uploads. Defaults to 100 MB.

- ...:

  Additional arguments to pass to
  [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html)

## Value

A shiny application

## Examples

``` r
# \donttest{
if (interactive()) {
  # launch app
  run_app()
}

# or run with arguments
data(example_data)
nerfplus_out <- nerfplus(
  x = example_data$x, y = example_data$y, A = example_data$A,
  lambda_netcoh = 1,
  lambda_embed = 0.1,
  lambda_raw = 2,
  lambda_stump = 3,
  family = "linear", embedding = "laplacian", sample_split = "none"
)
out <- interpret_nerfplus(
  nerfplus_out, x = example_data$x, y = example_data$y, A = example_data$A,
  xtest = example_data$xtest, ytest = example_data$ytest,
  A_full = example_data$A_full
)

if (interactive()) {
  run_app(
    data_list = out$data_list,
    object = out$object,
    fi_results = out$fi_results,
    loo_results = out$loo_results
  )
}
# }
```
