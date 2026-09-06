# Augment Psi matrix in NeRF+ with raw features

These helper functions augment the Psi matrix in NeRF+ with raw
features. Moreover, the raw features are dummy-coded if they are
categorical.

## Usage

``` r
fit_augmentation(x, psi, tree_info = NULL, include_raw = TRUE)

apply_augmentation(
  x,
  psi,
  tree_info = NULL,
  include_raw = TRUE,
  dummy_fit = NULL
)
```

## Arguments

- x:

  A data frame or matrix containing the raw features.

- psi:

  A data frame or matrix containing the Psi matrix, where each column
  corresponds to a decision stump feature.

- tree_info:

  Output of
  [`ranger::treeInfo()`](http://imbs-hl.github.io/ranger/reference/treeInfo.md)
  for a single tree.

- include_raw:

  Logical indicating whether to include the raw covariates in the NeRF+
  model. Default is `TRUE`.

- dummy_fit:

  A previously fitted dummy coding model; typically the output of
  [`fit_dummy_code()`](https://tiffanymtang.github.io/nerfplus/reference/dummy_code.md).
  If `include_raw = TRUE`, this is used to dummy-code raw features.

## Value

For `fit_augmentation()`, a list containing:

- `x`: A matrix containing the augmented data, which includes both the
  raw features (if `include_raw = TRUE`) and the Psi features.

- `dummy_fit`: The fitted dummy coding model, which dummy-codes
  categorical features. For `apply_augmentation()`, a matrix containing
  the augmented data, which includes both the raw features (if
  `include_raw = TRUE`) and the Psi features. If `include_raw = FALSE`,
  only the Psi features are returned.
