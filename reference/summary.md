# Summary of results for stochastic metafrontier models

Create and print summary results for stochastic metafrontier models
returned by
[`sfametafrontier`](https://SulmanOlieko.github.io/metafrontieR/reference/sfametafrontier.md).

## Usage

``` r
# S3 method for class 'sfametafrontier'
summary(object, ...)

# S3 method for class 'summary.sfametafrontier'
print(x, digits = max(3, getOption("digits") - 2), ...)
```

## Arguments

- object:

  An object of class `'sfametafrontier'` returned by the function
  [`sfametafrontier`](https://SulmanOlieko.github.io/metafrontieR/reference/sfametafrontier.md).

- ...:

  Currently ignored.

- x:

  An object of class `'summary.sfametafrontier'`.

- digits:

  Numeric. Number of digits displayed in values.

## Value

The `summary` method returns a list of class `'summary.sfametafrontier'`
that contains the same elements as an object returned by
[`sfametafrontier`](https://SulmanOlieko.github.io/metafrontieR/reference/sfametafrontier.md)
with the following additional elements:

- AIC:

  Akaike information criterion.

- BIC:

  Bayesian information criterion.

- HQIC:

  Hannan-Quinn information criterion.

- metaRes:

  Matrix of metafrontier estimates, their standard errors, z-values, and
  asymptotic P-values.

- effStats:

  A list of efficiency statistics including group means and class
  membership probabilities.

- grpSummaries:

  A list of summary objects for each group model.

## See also

[`sfametafrontier`](https://SulmanOlieko.github.io/metafrontieR/reference/sfametafrontier.md),
for the stochastic metafrontier analysis model fitting function for
cross-sectional or pooled data.
