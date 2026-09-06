# Fit regularization path for logistic regression with network cohesion

Fit regularization path for logistic regression with network cohesion

## Usage

``` r
rnc_logistic_path(
  x,
  y,
  A,
  x_test = NULL,
  y_test = NULL,
  A_full = NULL,
  lambda_grid,
  nodeids = NULL,
  nodeids_test = NULL,
  newton_maxit = 50,
  newton_tol = 1e-04,
  ...
)
```
