# Example dataset for NeRF+ demonstration

Example dataset for NeRF+ demonstration

## Usage

``` r
example_data
```

## Format

### `example_data`

A list with the following named components:

- x:

  Training covariate data matrix with 80 rows and 10 columns.

- xtest:

  Test covariate data matrix with 40 rows and 10 columns.

- y:

  Training response vector with 80 elements.

- ytest:

  Test response vector with 40 elements.

- A:

  Training adjacency matrix with 80 rows and 80 columns.

- A_full:

  Full adjacency matrix with 120 rows and 120 columns.

The covariate data matrix was generated from a standard normal
distribution. The adjacency matrix was generated from a stochastic block
model with 3 blocks. The responses were generated from a linear model
with an additive network block effect and a main covariate effect from
the first covariate only.
