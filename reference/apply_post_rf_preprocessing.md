# Postprocessing helper functions after fitting the RF in NeRF+ models

This function applies some processing steps to the data after fitting
the RF in NeRF+ models. Specifically, it converts factor levels to
numeric values based on the levels stored in the RF model.

## Usage

``` r
apply_post_rf_preprocessing(object, x)
```

## Arguments

- object:

  A `ranger` object that has been fitted to the data.

- x:

  A data frame or matrix containing the data to be processed.

## Value

A data frame or matrix with the same structure as `x`, but with factor
levels converted to numeric values based on the levels stored in
`object`.
