# Preprocessing helper functions prior to fitting the RF in NeRF+ models

These functions fit or apply preprocessing steps prior to fitting RF in
NeRF+ models. Specifically, this function standardizes the numeric
features to have mean 0 and SD 1 (if `standardize = TRUE`) and augments
the data with (standardized) network embeddings (if `embedding` is
specified).

## Usage

``` r
fit_pre_rf_preprocessing(
  x,
  A = NULL,
  standardize = TRUE,
  embedding = NULL,
  embedding_options = list(ndim = 2, regularization = 0.5, varimax = FALSE, center =
    FALSE, scale = FALSE),
  nodeids = NULL
)

apply_pre_rf_preprocessing(
  preprocess_fit,
  x,
  x_embed = NULL,
  A_full,
  nodeids = NULL
)
```

## Arguments

- x:

  A numeric matrix or data frame of predictors (features); size n x p.
  Should be centered so that each column has mean 0.

- A:

  An adjacency matrix representing the network structure.

- standardize:

  Logical indicating whether to standardize numeric features in `x` to
  have mean 0 and SD 1. Defaults to `TRUE`.

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

- nodeids:

  (Optional) vector of node IDs of length n. If provided, node IDs
  indicate the rows of A, corresponding to each sample. If not provided,
  the rows of A are assumed to be in the same order as the rows of x and
  y.

- preprocess_fit:

  Output of `fit_pre_rf_preprocessing()` to be applied to new data.

- x_embed:

  Optional embedding data frame or matrix, whose rows are aligned with
  those in `x`. If provided, it will be used to augment the input `x`
  data. Only needed if training embeddings were manually inputted.

- A_full:

  An adjacency matrix representing the network structure for the full
  set of nodes (training + testing nodes in that order).

## Value

For `fit_pre_rf_preprocessing()`, a list containing the following
components:

- `x`: The preprocessed data frame

- `standardize_x`: Logical indicating whether the raw data was
  standardized

- `x_center_factors`: Named numeric vector of means used for
  standardization

- `x_scale_factors`: Named numeric vector of standard deviations used
  for standardization

- `embedding`: The method used for embedding

- `embedding_fit`: The output of the network embedding fit

- `embed_center_factors`: Named numeric vector of means used for
  standardizing the embeddings

- `embed_scale_factors`: Named numeric vector of standard deviations
  used for standardizing the embeddings

- `nodeids`: The node IDs provided for the embeddings.

For `apply_pre_rf_preprocessing()`, a data frame with the preprocessed
data.
