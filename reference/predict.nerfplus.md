# Predict method for a NeRF+ model

Predict method for a NeRF+ model

## Usage

``` r
# S3 method for class 'nerfplus'
predict(
  object,
  x,
  x_embed = NULL,
  A_full,
  nodeids = NULL,
  type = c("response", "alpha"),
  return_all = FALSE
)
```

## Arguments

- object:

  A fitted NeRF+ model object.

- x:

  A data frame or matrix of new data for which predictions are to be
  made.

- x_embed:

  Optional embedding data frame or matrix, whose rows are aligned with
  those in `x`. If provided, it will be used to augment the input `x`
  data. Only needed if training embeddings were manually inputted.

- A_full:

  An adjacency matrix representing the network structure for the full
  set of nodes (training + testing nodes in that order). Note: if
  `nrow(x) == nrow(A_full)`, then `x` is assumed to be the training
  data.

- nodeids:

  (Optional) vector of node IDs of length equal to nrows in `x`. If
  provided, node IDs indicate the rows of A_full, corresponding to each
  sample. If not provided, the rows of A_full are assumed to be in the
  order of (x_train, x).

- type:

  Type of prediction to return; one of "response" (default) or "alpha".
  If "response", the predicted values are returned. If "alpha", the
  estimated individual node effects are returned.

- return_all:

  If `TRUE`, returns a list of predictions for each tree in the
  ensemble. If `FALSE` (default), returns the average prediction across
  all trees.

## Value

If `return_all = FALSE`, this function returns a vector of predicted
values (if `type = "response"`) or estimated individual node effects (if
`type = "alpha"`). If `return_all = TRUE`, this function returns a list
of predicted values or estimated individual node effects, where each
element in the list corresponds to a different tree in the ensemble.

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
predicted_y <- predict(
  nerfplus_out, x = example_data$xtest, A_full = example_data$A_full,
  type = "response"
)
estimated_alphas <- predict(
  nerfplus_out, x = example_data$xtest, A_full = example_data$A_full,
  type = "alpha"
)
```
