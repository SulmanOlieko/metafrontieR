# Extract information criteria of stochastic metafrontier models

`ic` returns information criterion from stochastic metafrontier models
estimated with
[`sfametafrontier`](https://SulmanOlieko.github.io/metafrontieR/reference/sfametafrontier.md).

## Usage

``` r
# S3 method for class 'sfametafrontier'
ic(object, IC = "AIC", ...)
```

## Arguments

- object:

  A stochastic metafrontier model returned by
  [`sfametafrontier`](https://SulmanOlieko.github.io/metafrontieR/reference/sfametafrontier.md).

- IC:

  Character string. Information criterion measure. Three criteria are
  available:

  - `'AIC'` for Akaike information criterion (default)

  - `'BIC'` for Bayesian information criterion

  - `'HQIC'` for Hannan-Quinn information criterion

  .

- ...:

  Currently ignored.

## Value

`ic` returns the value of the information criterion (AIC, BIC or HQIC)
of the maximum likelihood coefficients.

## Details

The different information criteria are computed as follows:

- AIC: \\-2 \log{LL} + 2 \* K\\

- BIC: \\-2 \log{LL} + \log{N} \* K\\

- HQIC: \\-2 \log{LL} + 2 \log{\left\[\log{N}\right\]} \* K\\

where \\LL\\ is the maximum likelihood value, \\K\\ the number of
parameters estimated and \\N\\ the number of observations.

## See also

[`sfametafrontier`](https://SulmanOlieko.github.io/metafrontieR/reference/sfametafrontier.md),
for the stochastic metafrontier analysis model fitting function using
cross-sectional or pooled data.
