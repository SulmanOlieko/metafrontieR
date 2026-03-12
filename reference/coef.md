# Extract coefficients of stochastic metafrontier models

From an object of class `'summary.sfametafrontier'`, `coef` extracts the
coefficients, their standard errors, z-values, and (asymptotic)
P-values.

From on object of class `'sfametafrontier'`, it extracts only the
estimated coefficients.

## Usage

``` r
# S3 method for class 'sfametafrontier'
coef(object, ...)

# S3 method for class 'summary.sfametafrontier'
coef(object, ...)
```

## Arguments

- object:

  A stochastic metafrontier model returned by
  [`sfametafrontier`](https://SulmanOlieko.github.io/metafrontieR/reference/sfametafrontier.md),
  or an object of class `'summary.sfametafrontier'`.

- ...:

  Currently ignored.

## Value

For objects of class `'summary.sfametafrontier'`, `coef` returns a
matrix with four columns. Namely, the estimated coefficients, their
standard errors, z-values, and (asymptotic) P-values.

For objects of class `'sfametafrontier'`, `coef` returns a numeric
vector of the estimated coefficients.

## See also

[`sfametafrontier`](https://SulmanOlieko.github.io/metafrontieR/reference/sfametafrontier.md),
for the stochastic metafrontier analysis model fitting function using
cross-sectional or pooled data.
