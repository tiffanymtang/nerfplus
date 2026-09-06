# Fit scaled logistic regression with a precomputed network Laplacian

Fit scaled logistic regression with a precomputed network Laplacian

## Usage

``` r
rnc_logistic_scaled_laplacian(
  x,
  y,
  L,
  lambda_netcoh,
  lambda_x = 0,
  init = NULL,
  newton_maxit = 50,
  newton_tol = 1e-04,
  verbose = FALSE
)
```
