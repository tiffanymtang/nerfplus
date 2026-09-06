# Compute Leave-One-Out (LOO) predictions and parameter changes for a NeRF+ model

This function computes leave-one-out (LOO) predictions and parameter
changes for a fitted NeRF+ model. For each sample, it computes the
model's predictions and parameter estimates when that sample is left out
of the training data. This is useful for assessing model stability and
identifying influential observations.

## Usage

``` r
get_loo(
  object,
  x,
  x_embed = NULL,
  y,
  A = NULL,
  xtest = NULL,
  xtest_embed = NULL,
  ytest = NULL,
  A_full = NULL,
  nodeids = NULL,
  nodeids_test = NULL,
  metric = NULL,
  return_all = FALSE
)
```

## Arguments

- object:

  A fitted NeRF+ model object.

- x:

  A data frame or matrix containing the training data.

- x_embed:

  An optional data frame or matrix of network embeddings corresponding
  to the training samples. Only needed if training embeddings were
  manually inputted.

- y:

  A vector of responses for the training data.

- A:

  An adjacency matrix representing the network structure for the
  training nodes.

- xtest:

  An optional data frame or matrix containing the test data.

- xtest_embed:

  An optional data frame or matrix of network embeddings corresponding
  to the test samples.

- ytest:

  An optional vector of responses for the test data.

- A_full:

  An optional adjacency matrix representing the network structure for
  the full set of nodes (training + testing nodes in that order).

- nodeids:

  An optional vector of node IDs of length n. If provided, node IDs
  indicate the rows of A corresponding to each sample. If not provided,
  the rows of A are assumed to be in the same order as the rows of x and
  y.

- nodeids_test:

  An optional vector of node IDs for the test data.

- metric:

  An optional function to compute the metric used for evaluating
  prediction changes. If NULL, defaults to RMSE for linear models and
  AUROC for logistic models.

- return_all:

  Logical indicating whether to return all intermediate results for each
  tree in the forest. Defaults to FALSE.

## Value

A list containing:

- loo_params:

  A list of LOO parameter estimates for each tree.

- change_alphas:

  Matrix of changes in alpha parameters when each sample is left out.

- mean_change_alphas:

  Mean change in alpha parameters across all trees.

- mean_change_betas:

  Mean change in beta parameters across all trees.

- loo_preds:

  Matrix of LOO predictions, where each column corresponds to
  predictions when a different sample is left out.

- loo_preds_test:

  Matrix of LOO predictions on test data (if provided).

- mean_change_error_test:

  Mean change in prediction error on test data across all trees (if
  provided).

- mean_change_preds_test:

  Mean change in predictions on test data across all trees (if
  provided).

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
loo_out <- get_loo(
  nerfplus_out,
  x = example_data$x, y = example_data$y, A = example_data$A,
  xtest = example_data$xtest, ytest = example_data$ytest,
  A_full = example_data$A_full
)
# }
```
