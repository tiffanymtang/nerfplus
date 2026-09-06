# Predict method for rnc linear regression objects

Predict method for rnc linear regression objects

## Usage

``` r
predict_rnc_linear(object, x, A_full, nodeids = NULL)
```

## Arguments

- object:

  An object of class `rnc` containing the fitted model.

- x:

  A numeric matrix or data frame of predictors (features) to make
  predictions for; size n x p.

- A_full:

  An adjacency matrix representing the network structure for the full
  set of nodes (training + testing nodes in that order)

- nodeids:

  (Optional) vector of node IDs of length n. If provided, node IDs
  indicate the rows of A_full, corresponding to each sample. If not
  provided, the rows of A_full are assumed to be in the order of
  (x_train, x).
