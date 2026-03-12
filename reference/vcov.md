# Compute variance-covariance matrix of stochastic metafrontier models

`vcov` computes the variance-covariance matrix of the maximum likelihood
(ML) coefficients from stochastic metafrontier models estimated with
[`sfametafrontier`](https://SulmanOlieko.github.io/metafrontieR/reference/sfametafrontier.md).

## Usage

``` r
# S3 method for class 'sfametafrontier'
vcov(object, ...)
```

## Arguments

- object:

  A stochastic metafrontier model returned by
  [`sfametafrontier`](https://SulmanOlieko.github.io/metafrontieR/reference/sfametafrontier.md).

- ...:

  Currently ignored

## Value

The variance-covariance matrix of the maximum likelihood coefficients is
returned.

## Details

The variance-covariance matrix is obtained by the inversion of the
negative Hessian matrix. Depending on the distribution and the
`'hessianType'` option, the analytical/numeric Hessian or the bhhh
Hessian is evaluated.

## See also

[`sfametafrontier`](https://SulmanOlieko.github.io/metafrontieR/reference/sfametafrontier.md),
for the stochastic metafrontier analysis model fitting function using
cross-sectional or pooled data.
