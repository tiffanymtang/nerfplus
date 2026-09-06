# Interpret NeRF+ Model

Wrapper function around
[`get_feature_importances()`](https://tiffanymtang.github.io/nerfplus/reference/get_feature_importances.md)
and
[`get_loo()`](https://tiffanymtang.github.io/nerfplus/reference/get_loo.md).
Helpful to format results correctly for input into NeRF+ Interpreter
Shiny application.

## Usage

``` r
interpret_nerfplus(
  object,
  x,
  x_embed = NULL,
  y,
  A,
  nodeids = NULL,
  xtest,
  xtest_embed = NULL,
  ytest,
  A_full,
  nodeids_test = NULL,
  methods = c("permute", "mdi+", "local", "loo"),
  B = 10,
  metric = NULL,
  save = FALSE,
  save_dir = "results_nerfplus"
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

- nodeids:

  An optional vector of node IDs of length n. If provided, node IDs
  indicate the rows of A corresponding to each sample. If not provided,
  the rows of A are assumed to be in the same order as the rows of x and
  y.

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

- nodeids_test:

  An optional vector of node IDs for the test data.

- methods:

  A character vector of methods to run. Can include any of "permute",
  "mdi+", "local", and "loo".

- B:

  Number of permutations to use for permutation feature importance.

- metric:

  A function to compute the metric used for global feature importances.
  Defaults to R-squared for regression and AUROC for classification.

- save:

  Whether to save the results to disk.

- save_dir:

  Directory to save results to if `save = TRUE`.

## Value

A list with the following components:

- object:

  The fitted NeRF+ model.

- data_list:

  A list containing the data used for interpretation.

- fi_results:

  A list containing the feature importance results.

- loo_results:

  A list containing the sample influence results.

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
out <- interpret_nerfplus(
  nerfplus_out, x = example_data$x, y = example_data$y, A = example_data$A,
  xtest = example_data$xtest, ytest = example_data$ytest,
  A_full = example_data$A_full
)
# }
```
