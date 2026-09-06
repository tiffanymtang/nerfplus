# Fit Network-assisted Random Forest+ (NeRF+) with Cross-Validation

Fit Network-assisted Random Forest+ (NeRF+) with Cross-Validation

## Usage

``` r
nerfplus_cv(
  x,
  y,
  A = NULL,
  nodeids = NULL,
  cv = 5,
  cv_foldids = NULL,
  family = c("linear", "logistic"),
  include_raw = TRUE,
  include_netcoh = TRUE,
  embedding = NULL,
  embedding_options = list(ndim = 2, regularization = 0.5, varimax = FALSE, center =
    TRUE, scale = TRUE),
  standardize_x = TRUE,
  normalize_stump = FALSE,
  sample_split = c("none", "oob", "inbag"),
  ntrees = 500,
  ntrees_cv = ntrees,
  mtry = NULL,
  lambdas_netcoh,
  lambdas_embed = NULL,
  lambdas_raw = NULL,
  lambdas_stump,
  lambdas_l = 0.05,
  parallel = FALSE,
  num.threads = 1,
  ...
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

- cv:

  Number of cross-validation folds. Default is 5.

- cv_foldids:

  (Optional) List of length `cv`, where each component in the list is a
  vector of sample indices in that fold. If `NULL` (default),
  cross-validation folds will be created randomly.

- family:

  A character string indicating the type of model to fit. Currently,
  only "linear" and "logistic" are supported.

- include_raw:

  Logical indicating whether to include the raw covariates in the NeRF+
  model. Default is `TRUE`.

- include_netcoh:

  Logical indicating whether to include the individual node effects and
  network cohesion regularization in the NeRF+ model. Default is `TRUE`.

- embedding:

  Embedding type(s), at least one of "adjacency", "laplacian", score",
  or NULL (i.e., do not include any network embedding features).
  Alternatively, can directly input an n x d matrix of network embedding
  features corresponding to `x`.

- embedding_options:

  A list of options for the network embedding. Ignored if
  `embedding = NULL`. If provided, the list should contain the following
  components:

  - `ndim`: Number of dimensions in the embedding (default is 2).

  - `regularization`: Regularization parameter for the adjacency matrix
    (default is 0.5).

  - `varimax`: Whether to apply varimax rotation to the embedding
    (default is FALSE).

  - `center`: Whether to center the embedding so that each column has
    mean 0 (default is TRUE).

  - `scale`: Whether to scale the embedding so that first embedding
    component column has SD 1 (default is TRUE). All other embedding
    components are scaled, proportional to their eigenvalues.

- standardize_x:

  Logical indicating whether to standardize the covariates so that each
  column has mean 0 and SD 1. Default is `TRUE`.

- normalize_stump:

  Logical indicating whether to normalize the decision stump features by
  number of samples in children nodes. Default is `FALSE`.

- sample_split:

  Character string indicating how to split the samples for training the
  model; one of "none" (default), "oob", or "inbag". If "none", all
  samples are used for estimating coefficients in NeRF+. If "oob", only
  out-of-bag samples are used for estimating coefficients in NeRF+. If
  "inbag", only in-bag samples are used for estimating coefficients in
  NeRF+.

- ntrees:

  Number of trees in ensemble.

- ntrees_cv:

  Number of trees that will be tuned using cross-validation. Default is
  `ntrees` (i.e., every tree will be tuned). Reduce this number to speed
  up the cross-validation process. For all trees that aren't tuned, the
  hyperparameter will be chosen randomly from the tuned trees.

- mtry:

  Number of features to consider at each split. Default is the number of
  features / 3 for regression and the square root of the number of
  features for classification.

- lambdas_netcoh:

  Vector of regularization parameters for the network cohesion term.

- lambdas_embed:

  Vector of regularization parameters for the network embedding
  features. If `NULL`, the regularization parameter corresponding to the
  network embedding features will be equal to the regularization
  parameter for the raw covariates.

- lambdas_raw:

  Vector of regularization parameters for the raw covariate features. If
  `NULL`, the regularization parameter for the raw covariates will be
  equal to the regularization parameter for the decision stump features.

- lambdas_stump:

  Vector of regularization parameters for the decision stump features.

- lambdas_l:

  Vector of regularization parameters for the graph Laplacian.

- parallel:

  Logical indicating whether to use parallel processing.

- num.threads:

  Number of threads to use for parallel processing. Default is 1.
  Ignored if `parallel = FALSE`.

- ...:

  Additional arguments passed to the
  [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md)
  function for fitting the random forest model.

## Value

A list containing the following:

- `rf_fit`: The fitted random forest model object from
  [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md).

- `nerfplus_fits`: A list of fitted NeRF+ models for each tree in the
  random forest using the tuned hyperparameters. Each element of the
  list is a fitted model object that can be used to make predictions.

- `cv_losses`: A list of ntrees_cv data frames containing the
  cross-validation losses for each tree and each fold. Each item in the
  list corresponds to a tree in the random forest. Each row in the data
  frame corresponds to a different set of hyperparameters.

- `best_cv_params`: A data frame containing the used hyperparameters for
  each tree in the random forest.

- `tree_infos`: A list of tree information objects for each tree in the
  random forest.

- `pre_rf_preprocessing_info`: A list containing preprocessing
  information for the NeRF+ model; output of
  [`fit_pre_rf_preprocessing()`](https://tiffanymtang.github.io/nerfplus/reference/nerfplus_preprocessing.md).

- `regularization_params`: A list containing the regularization
  parameters used in the NeRF+ model

- `model_info`: A list containing information about the model, such as
  `family`, `include_raw`, `include_netcoh`, `normalize_stump`, and
  `sample_split`.

- `unordered_factors`: A character vector of variable names that are
  unordered factors.

## Examples

``` r
# \donttest{
data(example_data)
nerfplus_cv_out <- nerfplus_cv(
  x = example_data$x, y = example_data$y, A = example_data$A,
  lambdas_netcoh = c(0.1, 1),
  lambdas_embed = c(0, 0.1),
  lambdas_raw = c(1, 2),
  lambdas_stump = c(1, 2),
  family = "linear", embedding = "laplacian", sample_split = "none"
)
# }
```
