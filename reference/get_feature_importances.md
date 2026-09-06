# Compute feature importances for a NeRF+ model

This function computes global and local feature importances given a
fitted NeRF+ model.

## Usage

``` r
get_feature_importances(
  object,
  x,
  x_embed = NULL,
  y = NULL,
  A_full,
  nodeids = NULL,
  method = c("permute", "mdi+", "local"),
  B = 10,
  metric = NULL
)
```

## Arguments

- object:

  A fitted NeRF+ model object.

- x:

  A data frame or matrix used to compute the feature importances.

- x_embed:

  An optional data frame or matrix of network embeddings corresponding
  to the samples in `x`. Only needed if training embeddings were
  manually inputted.

- y:

  A vector of responses.

- A_full:

  An adjacency matrix representing the network structure for the full
  set of nodes (training + testing nodes in that order)

- nodeids:

  (Optional) vector of node IDs of length n. If provided, node IDs
  indicate the rows of A, corresponding to each sample. If not provided,
  the rows of A are assumed to be in the same order as the rows of x and
  y.

- method:

  A character string indicating the method to use for computing feature
  importances. Options are:

  - `"permute"`: Permutation-based global feature importance.

  - `"mdi+"`: MDI+ global feature importance.

  - `"local"`: Local feature importance.

- B:

  Number of samples for permutation-based feature importance. Ignored if
  `method` is not `"permute"`.

- metric:

  A function to compute the metric used for global feature importances.
  Defaults to R-squared for regression and AUROC for classification.
  Ignored if `method` is `"local"`.

## Value

If `method` is `"permute"` or `"mdi+"`, a tibble with two columns:

- `var`: The name of the feature.

- `importance`: The computed feature importance score. If `method` is
  `"local"`, an n x p

## Examples

``` r
# \donttest{
data(example_data)
nerfplus_out <- nerfplus(
  x = example_data$x, y = example_data$y, A = example_data$A,
  lambda_netcoh = 1,
  lambda_embed = 0.1,
  lambda_raw = 2,
  lambda_stump = 3,
  family = "linear", embedding = "laplacian", sample_split = "none"
)
fi_out <- get_feature_importances(
  nerfplus_out, x = example_data$xtest, y = example_data$ytest,
  A_full = example_data$A_full, method = "mdi+"
)
# }
```
