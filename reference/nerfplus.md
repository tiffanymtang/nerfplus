# Fit Network-assisted Random Forest+ (NeRF+)

Fit Network-assisted Random Forest+ (NeRF+)

## Usage

``` r
nerfplus(
  x,
  y,
  A = NULL,
  nodeids = NULL,
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
  mtry = NULL,
  lambda_netcoh,
  lambda_embed = lambda_raw,
  lambda_raw = lambda_stump,
  lambda_stump,
  lambda_l = 0.05,
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

- mtry:

  Number of features to consider at each split. Default is the number of
  features / 3 for regression and the square root of the number of
  features for classification.

- lambda_netcoh:

  Regularization parameter for the network cohesion term. Can be either
  a scalar or a vector of length `ntrees`, specifying the regularization
  parameter for each tree. Ignored if `include_netcoh = FALSE`.

- lambda_embed:

  Regularization parameter for the network embedding features. Default
  is same as `lambda_raw`. Can be either a scalar or a vector of length
  `ntrees`, specifying the regularization parameter for each tree.
  Ignored if `embedding = NULL`.

- lambda_raw:

  Regularization parameter for the raw covariates. Default is same as
  `lambda_stump`. Can be either a scalar or a vector of length `ntrees`,
  specifying the regularization parameter for each tree. Ignored if
  `include_raw = FALSE`.

- lambda_stump:

  Regularization parameter for the decision stump features. Can be
  either a scalar or a vector of length `ntrees`, specifying the
  regularization parameter for each tree.

- lambda_l:

  (Optional) Regularization parameter for the graph Laplacian. Default
  is 0.05. Can be either a scalar or a vector of length `ntrees`,
  specifying the regularization parameter for each tree.

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
  random forest. Each element of the list is a fitted model object that
  can be used to make predictions.

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
data(example_data)
nerfplus_out <- nerfplus(
  x = example_data$x, y = example_data$y, A = example_data$A,
  lambda_netcoh = 1,
  lambda_embed = 0.1,
  lambda_raw = 2,
  lambda_stump = 3,
  family = "linear", embedding = "laplacian", sample_split = "none"
)
```
