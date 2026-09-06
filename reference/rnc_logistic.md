# Fit logistic regression with network cohesion

Fit logistic regression with network cohesion

## Usage

``` r
rnc_logistic(
  x,
  y,
  A,
  nodedegrees = NULL,
  nodeids = NULL,
  lambda_netcoh,
  lambda_x = 0,
  lambda_l = 0.05,
  init = NULL,
  newton_maxit = 50,
  newton_tol = 1e-04,
  verbose = FALSE
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

- init:

  (Optional) initial values for the optimization algorithm to fit
  logistic regression. Ignored for linear regression.

- newton_maxit:

  Maximum number of Newton iterations when fitting logistic regression.
  Default is 50. Ignored for linear regression.

- newton_tol:

  Tolerance for convergence of Newton iterations when fitting logistic
  regression. Default is 1e-4. Ignored for linear regression.

- verbose:

  Logical indicating whether to print progress messages.
