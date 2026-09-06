# Get the Psi matrix in NeRF+ containing decision stump features

These helper functions fit or apply a previously fitted Psi mapping
(i.e., decision stump feature mapping) to data.

## Usage

``` r
fit_psi(
  x,
  tree_info,
  tree_paths,
  node_preds,
  unordered_factors = NULL,
  normalize = FALSE,
  inbag_counts = NULL,
  as_matrix = FALSE
)

apply_psi(
  x,
  tree_info,
  tree_paths,
  node_preds,
  unordered_factors = NULL,
  psi_unique_values = NULL,
  as_matrix = FALSE
)
```

## Arguments

- x:

  A data frame or matrix containing the data to feed through decision
  tree to obtain the Psi (i.e., decision stump features) matrix.

- tree_info:

  Output of
  [`ranger::treeInfo()`](http://imbs-hl.github.io/ranger/reference/treeInfo.md)
  for a single tree.

- tree_paths:

  List of size ntrees with paths for each tree; typically the output of
  [`get_forest_paths()`](https://tiffanymtang.github.io/nerfplus/reference/get_forest_paths.md).

- node_preds:

  Matrix of terminal node predictions for each tree in the random
  forest; typically, output from
  `predict(rf_fit, x, type = "terminalNodes")$predictions`, where
  `rf_fit` is a fitted `ranger` object and `x` is the data frame or
  matrix.

- unordered_factors:

  Vector of column names corresponding to unordered factor variables in
  the data.

- normalize:

  Logical indicating whether to normalize the Psi matrix by number of
  training samples in each child node. Defaults to `FALSE`.

- inbag_counts:

  List of size ntrees with inbag counts for each tree; typically the
  output from `rf_fit$inbag.counts`, where `rf_fit` is a fitted `ranger`
  object. Ignored if `normalize = FALSE`.

- psi_unique_values:

  A named list of unique values for each Psi feature, typically obtained
  from the output of `fit_psi()`.

## Value

For `fit_psi()`, a list of two:

- `psi`: A data frame containing the Psi matrix, where each column
  corresponds to a decision stump feature

- `psi_unique_values`: A named list of unique values for each Psi
  feature (only returned if `normalize = TRUE`). For `apply_psi()`, a
  data frame containing the Psi matrix, where each column corresponds to
  a decision stump feature.
