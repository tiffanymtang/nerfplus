# Conformal prediction method for NeRF+

This function generates conformal prediction intervals for a fitted
NeRF+ model, using a specified calibration dataset to compute the
nonconformity scores (measured as the absolute residuals between the
observed responses and the predictions) and a specified test dataset for
which predictions and prediction intervals are to be generated.

## Usage

``` r
nerfplus_conformal(
  object,
  x_cal,
  x_cal_embed = NULL,
  y_cal,
  x_test,
  x_test_embed = NULL,
  A_full,
  nodeids_cal = NULL,
  nodeids_test = NULL,
  alpha = 0.05
)
```

## Arguments

- object:

  A fitted NeRF+ model object.

- x_cal:

  A data frame or matrix of calibration data.

- x_cal_embed:

  Optional embedding data frame or matrix for the calibration data,
  whose rows are aligned with those in `x_cal`. If provided, it will be
  used to augment the input `x_cal` data. Only needed if training
  embeddings were manually inputted.

- y_cal:

  A vector of observed responses for the calibration data.

- x_test:

  A data frame or matrix of test data for which predictions are to be
  made.

- x_test_embed:

  Optional embedding data frame or matrix for the test data, whose rows
  are aligned with those in `x_test`. If provided, it will be used to
  augment the input `x_test` data. Only needed if training embeddings
  were manually inputted.

- A_full:

  An adjacency matrix representing the network structure for the full
  set of nodes (training + calibration + testing nodes in that order,
  unless `nodeids_cal` and `nodeids_test` are provided, in which case
  the order of nodes in `A_full` should align with the order of node IDs
  in `nodeids_cal` and `nodeids_test`).

- nodeids_cal:

  (Optional) vector of node IDs for the calibration data, of length
  equal to nrows in `x_cal`. If provided, node IDs indicate the rows of
  A_full, corresponding to each calibration sample. If not provided, the
  rows of A_full are assumed to be in the order of (x_train, x_cal,
  x_test).

- nodeids_test:

  (Optional) vector of node IDs for the test data, of length equal to
  nrows in `x_test`. If provided, node IDs indicate the rows of A_full,
  corresponding to each test sample. If not provided, the rows of A_full
  are assumed to be in the order of (x_train, x_cal, x_test).

- alpha:

  Significance level for conformal prediction intervals. Default is 0.05
  for 95% prediction intervals.

## Value

A tibble with columns `pred`, `lower_bound`, and `upper_bound`,
containing the predicted values and the corresponding lower and upper
bounds of the conformal prediction intervals for each sample in the test
data.

## Examples

``` r
data(example_data)
train_idx <- 1:(nrow(example_data$x) / 2)
cal_idx <- (nrow(example_data$x) / 2 + 1):nrow(example_data$x)
x_train <- example_data$x[train_idx, ]
y_train <- example_data$y[train_idx]
x_cal <- example_data$x[cal_idx, ]
y_cal <- example_data$y[cal_idx]
nerfplus_out <- nerfplus(
  x = x_train, y = y_train, A = example_data$A[train_idx, train_idx],
  lambda_netcoh = 1,
  lambda_embed = 0.1,
  lambda_raw = 2,
  lambda_stump = 3,
  family = "linear", embedding = "laplacian", sample_split = "none"
)
conformal_out <- nerfplus_conformal(
  nerfplus_out,
  x_cal = x_cal, y_cal = y_cal,
  x_test = example_data$xtest,
  A_full = example_data$A_full,
  alpha = 0.05
)
conformal_out |>
  dplyr::mutate(
   contains_true_y = y_cal >= lower_bound & y_cal <= upper_bound
  ) |>
  dplyr::summarize(
   coverage = mean(contains_true_y)
  )
#> # A tibble: 1 × 1
#>   coverage
#>      <dbl>
#> 1     0.65
```
