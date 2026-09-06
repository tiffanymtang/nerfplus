# Compute permutation importance for a tree in NeRF+

Compute permutation importance for a tree in NeRF+

## Usage

``` r
tree_permute_fi(
  tree_object,
  x,
  y,
  A_full = NULL,
  nodeids = NULL,
  metric,
  grouped_features = NULL,
  B = 10,
  return_preds = FALSE
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

- grouped_features:

  A list of features to group together for permutation importance. Each
  element of the list should be a character vector of feature names. If
  `NULL`, each feature is treated as a separate group. Typically, this
  is the output of `get_grouped_tree_features()`.

- B:

  Number of bootstrap samples for permutation-based feature importance.
  Alternatively, a list of permutation indices can be passed to `B`,
  where each element is a vector of indices for permuting features.

- return_preds:

  Logical indicating whether to return the predictions for each
  permutation. If `TRUE`, the function returns a list of predictions for
  each permutation. If `FALSE`, it returns the computed feature
  importance scores.

## Value

If `return_preds` is `FALSE`, a tibble with two columns:

- `var`: The name of the feature.

- `importance`: The computed feature importance score. If `return_preds`
  is `TRUE`, a list where each element corresponds to a feature group
  and contains a list of predictions for each permutation.
