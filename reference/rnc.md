# Fit regression with network cohesion

This function fits a regression model with network cohesion
regularization. This code has been adapted from the `netcoh` R package
with minor tweaks for increased speed and flexibility. Currently, it can
handle linear and logistic regression only.

## Usage

``` r
rnc(
  x,
  y,
  A,
  nodedegrees = NULL,
  nodeids = NULL,
  lambda_netcoh,
  lambda_x = 0,
  lambda_l = 0.05,
  family = c("linear", "logistic"),
  low_dim = NULL,
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

- family:

  A character string indicating the type of model to fit. Currently,
  only "linear" and "logistic" are supported.

- low_dim:

  (Optional) If `TRUE`, the algorithm will use a naive solver for
  low-dimensional problems. Default is `NULL`, which will use the navie
  low-dimensional solver if the number of covariates is \<= 1/5 \* the
  number of samples. Only used if `family = "linear"`.

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

## Value

A list with the following components:

- `alpha`: n x 1 matrix of coefficients corresponding to the individual
  node effects

- `beta`: p x 1 matrix of coefficients corresponding to the covariates

- `lambda_netcoh`: Regularization parameter for the network cohesion
  term.

- `lambda_x`: Regularization parameter for the covariates.

- `lambda_l`: Regularization parameter for the graph Laplacian.

- `nalpha_train`: Number of distinct nodes in the training set (i.e.,
  number of rows in `A`).

- `family`: The type of model fitted, either "linear" or "logistic".

## Examples

``` r
data(example_data)
rnc_fit <- rnc(
  x = example_data$x, y = example_data$y, A = example_data$A,
  lambda_netcoh = 1, lambda_x = 0.5
)
```
