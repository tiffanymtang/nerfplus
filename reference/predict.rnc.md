# Predict method for rnc (regression with network cohesion) objects

Predict method for rnc (regression with network cohesion) objects

## Usage

``` r
# S3 method for class 'rnc'
predict(object, x, A_full, nodeids = NULL, ...)
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

- ...:

  Not used.

## Value

A list of two:

- `y`: A numeric vector of predictions for the response variable.

- `alpha`: A numeric vector of node-specific effects (intercepts).

## Examples

``` r
data(example_data)

# demonstrate prediction for RNC fit
rnc_fit <- rnc(
  x = example_data$x, y = example_data$y, A = example_data$A,
  lambda_netcoh = 1, lambda_x = 0.5
)
predicted_out <- predict(
  rnc_fit, x = example_data$xtest, A_full = example_data$A_full
)

# demonstrate prediction for RNC CV fit
cv_fit <- rnc_cv(
  x = example_data$x, y = example_data$y, A = example_data$A,
  lambdas_netcoh = c(0, 0.1, 1),
  lambdas_x = c(0.1, 0.5)
)
predicted_out <- predict(
  cv_fit, x = example_data$xtest, A_full = example_data$A_full
)
```
