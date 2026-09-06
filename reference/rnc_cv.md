# Fit regression with network cohesion with cross-validation wrapper

This function fits a regression model with network cohesion
regularization with a cross-validation wrapper. This code has been
adapted from the `netcoh` R package with minor tweaks for increased
speed and flexibility. Currently, it can handle linear and logistic
regression only.

## Usage

``` r
rnc_cv(
  x,
  y,
  A,
  nodeids = NULL,
  lambdas_netcoh,
  lambdas_x = 0,
  lambdas_l = 0.05,
  lambda_grid = NULL,
  family = c("linear", "logistic"),
  cv = 5,
  cv_foldids = NULL,
  refit = TRUE,
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

- nodeids:

  (Optional) vector of node IDs of length n. If provided, node IDs
  indicate the rows of A, corresponding to each sample. If not provided,
  the rows of A are assumed to be in the same order as the rows of x and
  y.

- lambdas_netcoh:

  Vector or list of regularization parameters for the network cohesion
  term.

- lambdas_x:

  Vector or list of regularization parameters for the (linear)
  covariates. Default is 0 (i.e., no regularization).

- lambdas_l:

  (Optional) Vector or list of regularization parameters for the graph
  Laplacian.

- lambda_grid:

  (Optional) A data frame with columns `lambda_netcoh`, `lambda_x`, and
  `lambda_l`, where each row specifies a different set of regularization
  parameters to try. If `NULL`, a grid of regularization parameters will
  be generated based upon all possible combination of the provided
  `lambdas_netcoh`, `lambdas_x`, and `lambdas_l`.

- family:

  A character string indicating the type of model to fit. Currently,
  only "linear" and "logistic" are supported.

- cv:

  Number of cross-validation folds. Default is 5.

- cv_foldids:

  (Optional) List of length `cv`, where each component in the list is a
  vector of sample indices in that fold. If `NULL` (default),
  cross-validation folds will be created randomly.

- refit:

  Logical indicating whether or not to refit tuned model on full
  training set after cross-validation. Default is `TRUE`.

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

- `lambda_grid`: A data frame with the regularization parameters used in
  the cross-validation.

- `best_params`: A data frame with the best regularization parameters
  found during cross-validation.

- `cv_errs`: A matrix of cross-validation errors, where each row
  corresponds to a set of regularization parameters and each column
  corresponds to a fold. If regression, the RMSE is returned. If
  classification, the AUROC is returned.

- `cv_means`: A vector of mean cross-validation errors for each set of
  regularization parameters.

- `cv_sds`: A vector of standard deviations of cross-validation errors
  for each set of regularization parameters.

- Other components from the fitted model if `refit = TRUE` (see output
  of
  [`rnc()`](https://tiffanymtang.github.io/nerfplus/reference/rnc.md)).

## Examples

``` r
data(example_data)

# fit RNC with CV
cv_fit <- rnc_cv(
  x = example_data$x, y = example_data$y, A = example_data$A,
  lambdas_netcoh = c(0, 0.1, 1),
  lambdas_x = c(0.1, 0.5)
)

# fit RNC with CV, specifying lambda grid manually (equivalent to above)
lambda_grid <- expand.grid(
  lambda_netcoh = c(0, 0.1, 1),
  lambda_x = c(0.1, 0.5),
  lambda_l = 0.05
)
cv_fit <- rnc_cv(
  x = example_data$x, y = example_data$y, A = example_data$A,
  lambda_grid = lambda_grid
)
```
