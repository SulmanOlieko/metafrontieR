# Sample Selection SFA Metafrontier (groupType = "sfaselectioncross")

## Overview

**Sample selection bias** arises when the observed sample is not a
random draw from the population. For example:

- Only firms above a revenue threshold are surveyed.
- Only farms that adopted a technology are observed using it.
- Participation in a programme is voluntary, so only volunteers are
  observed.

If selection into the sample is correlated with firm efficiency,
ignoring this leads to biased frontier estimates. `sfaselectioncross`
implements the **two-step ML estimator of Greene (2010)**, leveraging
the sample selection correction provided via `sfaR` (Dakpo et al. 2022),
which corrects for this bias using a probit selection equation (Heckman
1979 correction).

The selection model requires: - A **binary selection indicator** `d` (1
= selected/observed, 0 = not selected). - A **selection equation
formula** `selectionF` specifying which variables drive selection. At
least one variable must appear in `selectionF` but not in the main
frontier formula. - Only selected observations (`d == 1`) participate in
the frontier and receive efficiency estimates. Efficiency for
non-selected observations is `NA`.

## Data Preparation (Simulated Example)

We simulate data following the approach in the `sfaR` documentation:

``` r
library(metafrontieR)

N <- 2000; set.seed(12345)
z1 <- rnorm(N); z2 <- rnorm(N)
v1 <- rnorm(N); v2 <- rnorm(N)
g  <- rnorm(N)
e1 <- v1
e2 <- 0.7071 * (v1 + v2)
ds <- z1 + z2 + e1
d  <- ifelse(ds > 0, 1, 0)        # 1 = selected into the sample
group <- ifelse(g > 0, 1, 0)      # two technology groups
u  <- abs(rnorm(N))
x1 <- abs(rnorm(N)); x2 <- abs(rnorm(N))
y  <- abs(x1 + x2 + e2 - u)
dat <- as.data.frame(cbind(y = y, x1 = x1, x2 = x2,
                            z1 = z1, z2 = z2, d = d, group = group))

# About 50% of observations are selected
table(dat$d)
#>    0    1
#> 1013  987
```

## Method 1: sfaselectioncross + LP Metafrontier

``` r
meta_sel_lp <- sfametafrontier(
  formula    = log(y) ~ log(x1) + log(x2),
  selectionF = d ~ z1 + z2,      # selection equation: d is the binary indicator
  data       = dat,
  group      = "group",
  S          = 1L,
  udist      = "hnormal",
  groupType  = "sfaselectioncross",
  modelType  = "greene10",        # Greene (2010) two-step ML correction
  lType      = "kronrod",         # integration method for the selection likelihood
  Nsub       = 100,               # number of sub-intervals for numerical integration
  uBound     = Inf,
  method     = "bfgs",
  itermax    = 2000,
  metaMethod = "lp"
)
summary(meta_sel_lp)
```

> **Note:** The `selectionF` argument is compulsory for
> `groupType = "sfaselectioncross"`. The left-hand side must be the
> binary selection variable (`d`). At least one regressor in the
> selection equation should *not* appear in the main frontier formula
> (exclusion restriction for identification).

## Method 2: sfaselectioncross + QP Metafrontier

``` r
meta_sel_qp <- sfametafrontier(
  formula    = log(y) ~ log(x1) + log(x2),
  selectionF = d ~ z1 + z2,
  data       = dat,
  group      = "group",
  S          = 1L,
  udist      = "hnormal",
  groupType  = "sfaselectioncross",
  modelType  = "greene10",
  lType      = "kronrod",
  Nsub       = 100,
  uBound     = Inf,
  method     = "bfgs",
  itermax    = 2000,
  metaMethod = "qp"
)
summary(meta_sel_qp)
```

## Method 3: sfaselectioncross + SFA (Huang)

``` r
meta_sel_huang <- sfametafrontier(
  formula     = log(y) ~ log(x1) + log(x2),
  selectionF  = d ~ z1 + z2,
  data        = dat,
  group       = "group",
  S           = 1L,
  udist       = "hnormal",
  groupType   = "sfaselectioncross",
  modelType   = "greene10",
  lType       = "kronrod",
  Nsub        = 100,
  uBound      = Inf,
  method      = "bfgs",
  itermax     = 2000,
  metaMethod  = "sfa",
  sfaApproach = "huang"
)
summary(meta_sel_huang)
```

## Method 4: sfaselectioncross + SFA (O’Donnell)

``` r
meta_sel_odonnell <- sfametafrontier(
  formula     = log(y) ~ log(x1) + log(x2),
  selectionF  = d ~ z1 + z2,
  data        = dat,
  group       = "group",
  S           = 1L,
  udist       = "hnormal",
  groupType   = "sfaselectioncross",
  modelType   = "greene10",
  lType       = "kronrod",
  Nsub        = 100,
  uBound      = Inf,
  method      = "bfgs",
  itermax     = 2000,
  metaMethod  = "sfa",
  sfaApproach = "ordonnell"
)
summary(meta_sel_odonnell)
```

## Interpreting the Selection Correction

The first-stage probit model estimates the selection probability. The
key additional parameter in the frontier model is `rho` — the
correlation between the selection equation error and the frontier
equation noise.

``` r
# The rho parameter appears in the summary output:
# ----------------------------------------------------------------
#              Selection bias parameter
# ----------------------------------------------------------------
#           Coefficient Std. Error z value  Pr(>|z|)
# rho          0.89550    0.28696  3.1207  0.001804 **

# A significant rho indicates selection bias IS present and the
# correction is important.
```

| `rho` value | Interpretation |
|----|----|
| ≈ 0, p \> 0.05 | No significant selection bias; standard SFA may be sufficient |
| \> 0, p \< 0.05 | Positive selection — efficient firms are more likely selected |
| \< 0, p \< 0.05 | Negative selection — inefficient firms are more likely selected |

## Extracting Efficiencies

Only selected observations (those with `d == 1`) receive efficiency
estimates:

``` r
eff_sel <- efficiencies(meta_sel_lp)

# Non-selected observations have NA efficiencies
sum(is.na(eff_sel$TE_group_BC))   # count of non-selected obs

# Subset for selected observations in group 1
sel_grp1 <- eff_sel[eff_sel$group == 1 & !is.na(eff_sel$TE_group_BC), ]
summary(sel_grp1[, c("TE_group_BC", "TE_meta_BC", "MTR_BC")])
```
