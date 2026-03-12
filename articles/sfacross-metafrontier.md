# Standard SFA Metafrontier (groupType = "sfacross")

## Overview

When observed group labels are available (e.g., farm size, region,
ownership type), `groupType = "sfacross"` fits a separate
cross-sectional stochastic frontier for each group using
[`sfaR::sfacross()`](https://rdrr.io/pkg/sfaR/man/sfacross.html). The
group-specific results are then used to estimate the common metafrontier
using any of four methods.

## Data Preparation

We use the `ricephil` dataset from `sfaR`, which contains 344 Filipino
rice farms. We create three technology groups based on farm area
terciles.

``` r
library(metafrontieR)
data("ricephil", package = "sfaR")

ricephil$group <- cut(
  ricephil$AREA,
  breaks        = quantile(ricephil$AREA, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
  labels        = c("small", "medium", "large"),
  include.lowest = TRUE
)

table(ricephil$group)
#>  small medium  large
#>    125    104    115
```

## Method 1: LP Metafrontier

The **linear programming (LP)** envelope minimises the sum of absolute
deviations from group frontier predictions while satisfying a convexity
constraint. No stochastic parameters are estimated for the metafrontier
itself.

``` r
meta_lp <- sfametafrontier(
  formula    = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data       = ricephil,
  group      = "group",
  S          = 1,
  udist      = "hnormal",
  groupType  = "sfacross",
  metaMethod = "lp"
)
summary(meta_lp)
```

> **Note:** Since the LP metafrontier is estimated via linear
> programming, no estimated parameters are returned for the metafrontier
> level. The LP envelope is fully determined by the group frontier
> predictions.

## Method 2: QP Metafrontier

The **quadratic programming (QP)** envelope minimises the sum of
*squared* deviations from group frontier predictions. Unlike LP, QP
produces a smooth envelope that is differentiable everywhere, and it
returns estimated coefficients with standard errors.

``` r
meta_qp <- sfametafrontier(
  formula    = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data       = ricephil,
  group      = "group",
  S          = 1,
  udist      = "hnormal",
  groupType  = "sfacross",
  metaMethod = "qp"
)
summary(meta_qp)
```

## Method 3: Stochastic Metafrontier — Huang et al. (2014)

The **two-stage stochastic metafrontier** of Huang, Huang & Liu (2014)
uses the group-specific *fitted frontier values* as the dependent
variable in a second-stage pooled SFA. The technology gap U and noise V
are estimated stochastically, which naturally bounds the metatechnology
ratio MTR ∈ (0, 1\].

``` r
meta_huang <- sfametafrontier(
  formula     = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data        = ricephil,
  group       = "group",
  S           = 1,
  udist       = "hnormal",
  groupType   = "sfacross",
  metaMethod  = "sfa",
  sfaApproach = "huang"
)
summary(meta_huang)
```

## Method 4: Stochastic Envelope — O’Donnell et al. (2008)

The **O’Donnell et al. (2008)** approach uses the LP deterministic
envelope as the dependent variable in the second-stage SFA, rather than
the group-specific fitted values. This mixed deterministic–stochastic
approach embeds the envelope within an SFA framework.

> **Warning:** MTR values \> 1 can arise with this method when the
> second-stage SFA estimates near-zero inefficiency. If this occurs,
> consider using `metaMethod = "lp"` or `sfaApproach = "huang"` instead.

``` r
meta_ordonnell <- sfametafrontier(
  formula     = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data        = ricephil,
  group       = "group",
  S           = 1,
  udist       = "hnormal",
  groupType   = "sfacross",
  metaMethod  = "sfa",
  sfaApproach = "ordonnell"
)
summary(meta_ordonnell)
```

## Comparing Methods

All four methods use identical group-level estimates. The differences
arise only in how the metafrontier is computed:

| Method          | Metafrontier Coefficients Returned | MTR Bounded ≤ 1? |
|-----------------|------------------------------------|------------------|
| LP              | No (envelope rule)                 | Yes              |
| QP              | Yes (with SE)                      | Yes              |
| SFA (huang)     | Yes (with SE)                      | Yes              |
| SFA (ordonnell) | Yes (with SE)                      | Not guaranteed   |

## Extracting Efficiencies

All models return firm-level efficiency estimates via
[`efficiencies()`](https://SulmanOlieko.github.io/metafrontieR/reference/efficiencies.md):

``` r
eff <- efficiencies(meta_lp)
head(eff)

# Subset for a specific group
eff_small <- eff[eff$group == "small", ]
summary(eff_small[, c("TE_group_BC", "TE_meta_BC", "MTR_BC")])
```

## Other Extractors

``` r
coef(meta_qp)          # metafrontier coefficients
vcov(meta_qp)          # variance-covariance matrix
logLik(meta_lp)        # log-likelihood
ic(meta_lp)            # AIC, BIC, HQIC
nobs(meta_lp)          # number of observations
fitted(meta_lp)        # fitted values
residuals(meta_lp)     # residuals
```
