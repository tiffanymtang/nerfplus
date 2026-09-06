# Compute MDI+ feature importance for a tree in NeRF+

Compute MDI+ feature importance for a tree in NeRF+

## Usage

``` r
tree_mdiplus_fi(
  tree_object,
  x,
  y,
  A_full = NULL,
  nodeids = NULL,
  metric,
  x_means = NULL,
  grouped_features = NULL,
  L_full = NULL,
  solve_cache = NULL
)
```

## Arguments

- tree_object:

  A fitted tree object from NeRF+.

- x:

  A data frame or matrix used to compute the feature importances.

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

- metric:

  A function to compute the metric used for global feature importances.
  Defaults to R-squared for regression and AUROC for classification.
  Ignored if `method` is `"local"`.

- x_means:

  A named vector of means for each feature in `x`. If `NULL`, the means
  are computed from `x`.

- grouped_features:

  A list of features to group together for permutation importance. Each
  element of the list should be a character vector of feature names. If
  `NULL`, each feature is treated as a separate group. Typically, this
  is the output of `get_grouped_tree_features()`.

## Value

A tibble with two columns:

- `var`: The name of the feature.

- `importance`: The computed feature importance score.
