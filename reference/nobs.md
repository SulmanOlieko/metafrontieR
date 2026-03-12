# Extract total number of observations used in frontier models

This function extracts the total number of 'observations' from a fitted
point frontier model.

## Usage

``` r
# S3 method for class 'sfametafrontier'
nobs(object, ...)
```

## Arguments

- object:

  a `sfametafrontier` object for which the number of total observations
  is to be extracted.

- ...:

  Currently ignored.

## Value

A single number, normally an integer.

## Details

`nobs` gives the number of observations actually used by the estimation
procedure.

## See also

[`sfametafrontier`](https://SulmanOlieko.github.io/metafrontieR/reference/sfametafrontier.md),
for the stochastic metafrontier analysis model fitting function using
cross-sectional or pooled data
