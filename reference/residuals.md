# Extract residuals of stochastic metafrontier models

This function returns the residuals' values from stochastic metafrontier
models estimated with
[`sfametafrontier`](https://SulmanOlieko.github.io/metafrontieR/reference/sfametafrontier.md).

## Usage

``` r
# S3 method for class 'sfametafrontier'
residuals(object, ...)
```

## Arguments

- object:

  A stochastic metafrontier model returned by
  [`sfametafrontier`](https://SulmanOlieko.github.io/metafrontieR/reference/sfametafrontier.md).

- ...:

  Currently ignored.

## Value

`residuals` returns a vector of residuals values.

## Note

The residuals values are ordered in the same way as the corresponding
observations in the dataset used for the estimation.

## See also

[`sfametafrontier`](https://SulmanOlieko.github.io/metafrontieR/reference/sfametafrontier.md),
for the stochastic metafrontier analysis model fitting function using
cross-sectional or pooled data.
