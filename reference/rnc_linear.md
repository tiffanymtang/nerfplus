# Fit linear regression with network cohesion

Fit linear regression with network cohesion

## Usage

``` r
rnc_linear(
  x,
  y,
  A,
  nodedegrees = NULL,
  nodeids = NULL,
  lambda_netcoh,
  lambda_x = 0,
  lambda_l = 0.05,
  low_dim = NULL
)
```

## Arguments

- x:

  A numeric matrix or data frame of predictors (features); size n x p.
  Should be centered so that each column has mean 0.

- y:

  A numeric vector of responses of length n. Should be centered so that
  the mean is 0.

- A:

  An adjacency matrix representing the network structure.

- nodedegrees:

  (Optional) A vector of node degrees. If provided, it is used to
  compute the graph Laplacian. If not provided, it is computed from the
  adjacency matrix `A`.

- nodeids:

  (Optional) vector of node IDs of length n. If provided, node IDs
  indicate the rows of A, corresponding to each sample. If not provided,
  the rows of A are assumed to be in the same order as the rows of x and
  y.

- lambda_netcoh:

  Regularization parameter for the network cohesion term.

- lambda_x:

  (Optional) Regularization parameter for the (linear) covariates.
  Default is 0 (i.e., no regularization).

- lambda_l:

  (Optional) Regularization parameter for the graph Laplacian. Default
  is 0.05.

- low_dim:

  (Optional) If `TRUE`, the algorithm will use a naive solver for
  low-dimensional problems. Default is `NULL`, which will use the navie
  low-dimensional solver if the number of covariates is \<= 1/5 \* the
  number of samples. Only used if `family = "linear"`.
