# Arguments that are shared by multiple functions

Arguments that are shared by multiple functions

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

- family:

  A character string indicating the type of model to fit. Currently,
  only "linear" and "logistic" are supported.

- lambda_netcoh:

  Regularization parameter for the network cohesion term.

- lambda_l:

  (Optional) Regularization parameter for the graph Laplacian. Default
  is 0.05.

- lambdas_netcoh:

  Vector or list of regularization parameters for the network cohesion
  term.

- lambdas_l:

  (Optional) Vector or list of regularization parameters for the graph
  Laplacian.

- cv:

  Number of cross-validation folds. Default is 5.

- cv_foldids:

  (Optional) List of length `cv`, where each component in the list is a
  vector of sample indices in that fold. If `NULL` (default),
  cross-validation folds will be created randomly.

- refit:

  Logical indicating whether or not to refit tuned model on full
  training set after cross-validation. Default is `TRUE`.

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

- parallel:

  Logical indicating whether to use parallel processing.

- num.threads:

  Number of threads to use for parallel processing. Default is 1.
  Ignored if `parallel = FALSE`.
