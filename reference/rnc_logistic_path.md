# Fit regularization path for logistic regression with network cohesion

Fit regularization path for logistic regression with network cohesion

## Usage

``` r
rnc_logistic_path(
  x,
  y,
  A = NULL,
  x_test = NULL,
  y_test = NULL,
  A_full = NULL,
  L_unreg = NULL,
  L_full_unreg = NULL,
  lambda_grid,
  nodeids = NULL,
  nodeids_test = NULL,
  newton_maxit = 50,
  newton_tol = 1e-04,
  ...
)
```
