# Evaluate r-squared with NA handling

This function computes the R-squared value and returns 0 (instead of NA)
if the estimate is a constant vector.

## Usage

``` r
rsq_narm_vec(truth, estimate, ...)
```

## Arguments

- truth:

  A numeric vector of true values.

- estimate:

  A numeric vector of estimated values.

- ...:

  Additional arguments passed to
  [`yardstick::rsq_vec()`](https://yardstick.tidymodels.org/reference/rsq.html)

## Value

A numeric value representing the R-squared value, or 0 if the estimate
is a constant vector.
