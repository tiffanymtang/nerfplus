# Wrapper to fit various network embedding methods

Wrapper to fit various network embedding methods

## Usage

``` r
fit_network_embedding(
  A,
  ndim = 2,
  method = c("adjacency", "laplacian", "score"),
  regularization = 0.5,
  varimax_rotation = FALSE
)
```

## Arguments

- A:

  Adjacency matrix.

- ndim:

  Number of dimensions in embedding.

- method:

  Embedding type(s), at least one of "adjacency", "laplacian", or
  score".

- regularization:

  Regularization parameter for the adjacency matrix.

- varimax_rotation:

  Whether to apply varimax rotation to the embedding.

## Value

A list containing the following components:

- `X`: A list of matrices, each corresponding to a method in `method`,
  where each matrix has `ndim` columns representing the embedding.

- `eval_ls`: A list of eigenvalues corresponding to each method in
  `method`.

- `method`: The method(s) used for embedding.

- `regularization`: The regularization parameter used for the adjacency
  matrix.

## Examples

``` r
data(example_data)
embedding_fit <- fit_network_embedding(
  example_data$A, ndim = 2, method = "laplacian"
)
```
