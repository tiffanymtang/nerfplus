# Shared arguments for NeRF+ utility functions

Shared arguments for NeRF+ utility functions

## Arguments

- tree_info:

  Output of
  [`ranger::treeInfo()`](http://imbs-hl.github.io/ranger/reference/treeInfo.md)
  for a single tree.

- tree_infos:

  List of size ntrees with each entry being the output of
  [`ranger::treeInfo()`](http://imbs-hl.github.io/ranger/reference/treeInfo.md).

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

- inbag_counts:

  List of size ntrees with inbag counts for each tree; typically the
  output from `rf_fit$inbag.counts`, where `rf_fit` is a fitted `ranger`
  object. Ignored if `normalize = FALSE`.

- include_raw:

  Logical indicating whether to include the raw covariates in the NeRF+
  model. Default is `TRUE`.
