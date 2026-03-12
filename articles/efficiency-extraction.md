# Extracting Efficiencies and MTRs

## Overview

Once a `metafrontier` model has been fitted, a range of S3 methods are
available to extract and inspect results. This vignette demonstrates all
of them using a simple `sfacross` + LP example.

## Setup: Fit a Model

``` r
library(metafrontieR)
data("ricephil", package = "sfaR")

ricephil$group <- cut(
  ricephil$AREA,
  breaks        = quantile(ricephil$AREA, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
  labels        = c("small", "medium", "large"),
  include.lowest = TRUE
)

meta_lp <- sfametafrontier(
  formula    = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data       = ricephil,
  group      = "group",
  S          = 1,
  udist      = "hnormal",
  groupType  = "sfacross",
  metaMethod = "lp"
)
```

## `summary()` — Full Model Summary

Prints the complete model output including group-specific SFA results,
metafrontier coefficients (if available), and efficiency statistics by
group.

``` r
summary(meta_lp)
```

## `efficiencies()` — Firm-Level Efficiency and MTR Scores

Returns a data frame with one row per observation containing all
efficiency estimates and metatechnology ratios. All estimators are
available for all `groupType` values, though the exact columns vary
slightly by model type.

``` r
eff <- efficiencies(meta_lp)
head(eff)
```

### Column Reference

| Column | `sfacross` | `sfalcmcross` | `sfaselectioncross` |
|----|:--:|:--:|:--:|
| `id` | ✓ | ✓ | ✓ |
| `group` / `Group_c` | ✓ | ✓ | ✓ |
| `u_g` | ✓ | ✓ | ✓ |
| `TE_group_JLMS` | ✓ | ✓ | ✓ |
| `TE_group_BC` | ✓ | ✓ | ✓ |
| `TE_group_BC_reciprocal` | ✓ | ✓ | ✓ |
| `uLB_g`, `uUB_g` | ✓ | — | — |
| `m_g`, `TE_group_mode` | ✓ | — | — |
| `PosteriorProb_c`, `PosteriorProb_c1` … | — | ✓ | — |
| `u_meta` | ✓ | ✓ | ✓ |
| `TE_meta_JLMS` | ✓ | ✓ | ✓ |
| `TE_meta_BC` | ✓ | ✓ | ✓ |
| `MTR_JLMS` | ✓ | ✓ | ✓ |
| `MTR_BC` | ✓ | ✓ | ✓ |

### Subsetting by Group

``` r
# All small farms
eff_small <- eff[eff$group == "small", ]

# Descriptive statistics
summary(eff_small[, c("TE_group_BC", "TE_meta_BC", "MTR_BC")])

# Group-level means
aggregate(cbind(TE_group_BC, TE_meta_BC, MTR_BC) ~ group, data = eff, FUN = mean)
```

### Visualising the Distribution

``` r
# MTR distribution by group (base R)
boxplot(MTR_BC ~ group, data = eff,
        main = "Metatechnology Ratio by Farm Size",
        xlab = "Group", ylab = "MTR (BC)",
        col  = c("#e8f5ef", "#b2dfdb", "#80cbc4"))

# Histogram of TE_meta_BC
hist(eff$TE_meta_BC, breaks = 30,
     main = "Distribution of Metafrontier TE",
     xlab = "TE_meta_BC", col = "#2d8f65", border = "white")
```

## `coef()` — Estimated Coefficients

Returns the metafrontier coefficient vector (for QP and SFA methods;
`NULL` for LP).

``` r
# First fit a QP model
meta_qp <- sfametafrontier(
  formula    = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data       = ricephil, group = "group", S = 1, udist = "hnormal",
  groupType  = "sfacross", metaMethod = "qp"
)
coef(meta_qp)
```

## `vcov()` — Variance-Covariance Matrix

Returns the variance-covariance matrix of the metafrontier coefficients
(for models that estimate metafrontier parameters).

``` r
vcov(meta_qp)
```

## `logLik()` — Log-Likelihood

Returns the total log-likelihood value of the model (sum of group-level
log-likelihoods plus the metafrontier log-likelihood where applicable).

``` r
logLik(meta_lp)
```

## `ic()` — Information Criteria

Returns all three information criteria: AIC, BIC, and HQIC.

``` r
ic(meta_lp)
#>        AIC       BIC      HQIC
#> 1  184.579  253.710   212.113
```

## `nobs()` — Number of Observations

``` r
nobs(meta_lp)  # Total observations across all groups
```

## `fitted()` — Fitted Values

Returns the fitted frontier values from the model.

``` r
fv <- fitted(meta_lp)
head(fv)
```

## `residuals()` — Residuals

Returns the composite error residuals from the group-level stochastic
frontier models.

``` r
res <- residuals(meta_lp)
head(res)
```

## Comparing Multiple Models

You can compare information criteria across methods to select the best
model:

``` r
meta_lp    <- sfametafrontier(log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
                               data = ricephil, group = "group", S = 1,
                               udist = "hnormal", groupType = "sfacross",
                               metaMethod = "lp")
meta_qp    <- sfametafrontier(log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
                               data = ricephil, group = "group", S = 1,
                               udist = "hnormal", groupType = "sfacross",
                               metaMethod = "qp")
meta_huang <- sfametafrontier(log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
                               data = ricephil, group = "group", S = 1,
                               udist = "hnormal", groupType = "sfacross",
                               metaMethod = "sfa", sfaApproach = "huang")

# Combined information criteria table
models <- list(LP = meta_lp, QP = meta_qp, Huang = meta_huang)
do.call(rbind, lapply(names(models), function(nm) {
  ic_vals <- ic(models[[nm]])
  data.frame(Model = nm, AIC = ic_vals[["AIC"]],
             BIC = ic_vals[["BIC"]], HQIC = ic_vals[["HQIC"]])
}))
```
