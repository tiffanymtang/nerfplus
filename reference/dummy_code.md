# Dummy code categorical features

Dummy code categorical features

## Usage

``` r
fit_dummy_code(x)

apply_dummy_code(dummy_fit, x)
```

## Arguments

- x:

  A data frame or matrix containing the data to be dummy-coded.

- dummy_fit:

  A previously fitted dummy coding model; typically the output of
  `fit_dummy_code()`.

## Value

For `fit_dummy_code()`, a list containing:

- `dummy_fit`: The fitted dummy coding model, which can be used to
  dummy-code new data.

- `x`: A data frame or matrix containing the dummy-coded data. For
  `apply_dummy_code()`, a data frame or matrix containing the
  dummy-coded data.
