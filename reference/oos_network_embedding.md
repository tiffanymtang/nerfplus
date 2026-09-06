# Out-of-sample network embedding method

This function takes in out-of-sample points (which were not available at
training time) and embeds them in a previously trained network embedding
space.

## Usage

``` r
oos_network_embedding(embedding_fit, A, A_full)
```

## Arguments

- embedding_fit:

  Previously trained network embedding; should be output of
  [`fit_network_embedding()`](https://tiffanymtang.github.io/nerfplus/reference/fit_network_embedding.md).

- A:

  Adjacency matrix corresponding to the training data points.

- A_full:

  Adjacency matrix representing the full set of nodes (training +
  testing nodes in that order).

## Value

A list of two:

- `X`: A list of matrices, each corresponding to a method in
  `embedding_fit$method`, where each matrix has `ndim` columns
  representing the embedding for the out-of-sample points.

- `method`: The method(s) used for embedding.

## Examples

``` r
data(example_data)
embedding_fit <- fit_network_embedding(
  example_data$A, ndim = 2, method = "laplacian"
)
oos_embedding <- oos_network_embedding(
  embedding_fit, A = example_data$A, A_full = example_data$A_full
)
```
