# metafrontieR: An R package for metafrontier analysis.
<img src="man/figures/logo.png" align="right" height="139" alt="metafrontieR logo" />

[![CodeFactor](https://www.codefactor.io/repository/github/SulmanOlieko/metafrontieR/badge)](https://www.codefactor.io/repository/github/SulmanOlieko/metafrontieR)
[![R-CMD-check](https://github.com/SulmanOlieko/metafrontieR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SulmanOlieko/metafrontieR/actions/workflows/R-CMD-check.yaml)
[![](https://img.shields.io/badge/devel%20version-1.0.0-darkred.svg)](https://github.com/SulmanOlieko/metafrontieR)
[![](https://img.shields.io/badge/license-GPL-blue)](https://github.com/SulmanOlieko/metafrontieR)
[![](https://img.shields.io/github/languages/code-size/SulmanOlieko/metafrontieR.svg)](https://github.com/SulmanOlieko/metafrontieR)

> **Metafrontier Analysis Routines**

An R package for implementing various metafrontier analyses for productivity and performance benchmarking, assessing technical efficiencies, computing metatechnology ratios (MTRs), and measuring technology gaps for firms operating under different technologies.

`metafrontieR` provides routines for:

1. **Deterministic envelope metafrontier** via **linear programming (LP)** and **quadratic programming (QP)**, following [Battese, Rao & O'Donnell (2004)](https://doi.org/10.1023/B:PROD.0000012454.06094.29) and [O'Donnell, Rao & Battese (2008)](https://doi.org/10.1007/s00181-007-0119-4).
2. **Stochastic two-stage metafrontier** following [Huang, Huang & Liu (2014)](https://doi.org/10.1007/s11123-014-0402-2).

In addition, the package implements:

- **Latent class stochastic metafrontier analysis** — when technology groups are unobserved, the latent class model (LCM) automatically identifies classes and routes them to the metafrontier.
- **Sample selection correction metafrontier models** — corrects for sample selection bias following [Greene (2010)](https://doi.org/10.1007/s11123-009-0159-1).

> **Dependency:** `metafrontieR` depends on the [`sfaR`](https://CRAN.R-project.org/package=sfaR) package by [Dakpo, Desjeux & Latruffe (2023)](https://CRAN.R-project.org/package=sfaR), which provides the underlying stochastic frontier estimation routines for all group-level models.

---

## Installation

```r
# Install devtools if not already installed
if (!require("devtools")) install.packages("devtools")

# Install metafrontieR from GitHub
devtools::install_github("SulmanOlieko/metafrontieR")
```

---

## Usage Examples

The following sections provide comprehensive examples covering all three group-level model types and all four metafrontier methods. Each section demonstrates the full workflow: data preparation → group frontier estimation → metafrontier → efficiency and MTR extraction.

---

## Section 1: Standard SFA Group Frontier (`groupType = "sfacross"`)

Group boundaries are observed (a farm-size variable). Each group's frontier is estimated separately by `sfacross` from the `sfaR` package.

### Data Preparation

```r
library(metafrontieR)
data("ricephil", package = "sfaR")

# Create three technology groups based on farm area terciles
ricephil$group <- cut(ricephil$AREA,
  breaks = quantile(ricephil$AREA, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
  labels = c("small", "medium", "large"),
  include.lowest = TRUE
)
table(ricephil$group)
```

```plaintext
 small medium  large 
   125    104    115 
```

---

### 1a. LP Metafrontier (`groupType = "sfacross"`, `metaMethod = "lp"`)

Deterministic linear programming envelope (Battese, Rao & O'Donnell, 2004). The metafrontier parameter vector minimises the sum of absolute deviations while staying at or above all group frontier predictions.

```r
meta_sfacross_lp <- sfametafrontier(
  formula    = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data       = ricephil,
  group      = "group",
  S          = 1,
  udist      = "hnormal",
  groupType  = "sfacross",
  metaMethod = "lp"
)
summary(meta_sfacross_lp)
```

<details>
  <summary>Toggle to see the output</summary>

```plaintext
============================================================ 
Stochastic Metafrontier Analysis
Metafrontier method: Linear Programming (LP) Metafrontier 
Stochastic Production/Profit Frontier, e = v - u 
Group approach     : Stochastic Frontier Analysis 
Group estimator    : sfacross 
Group optim solver : BFGS maximization 
Groups ( 3 ): small, medium, large 
Total observations : 344 
Distribution       : hnormal 
============================================================ 

------------------------------------------------------------ 
Group: small (N = 125)  Log-likelihood: -50.98578
------------------------------------------------------------ 
               Coefficient Std. Error z value  Pr(>|z|)    
(Intercept)      -1.587445   0.512745 -3.0960  0.001962 ** 
log(AREA)         0.240139   0.118343  2.0292  0.042441 *  
log(LABOR)        0.434645   0.122915  3.5361  0.000406 ***
log(NPK)          0.305164   0.057015  5.3523 8.682e-08 ***
Zu_(Intercept)   -1.450932   0.298670 -4.8580 1.186e-06 ***
Zv_(Intercept)   -2.934055   0.354013 -8.2880 < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

  Variance & Efficiency Statistics (delta-method SEs):
                              Estimate Std. Error z value  Pr(>|z|)    
Sigma-squared(u)              0.234352   0.069994  3.3482 0.0008135 ***
Sigma(u)                      0.484099   0.072293  6.6963 2.137e-11 ***
Sigma-squared(v)              0.053181   0.018827  2.8248 0.0047317 ** 
Sigma(v)                      0.230610   0.040819  5.6495 1.609e-08 ***
Lambda = sigma(u)/sigma(v)    2.099212   0.628650  3.3392 0.0008401 ***
E[u]                          0.386255          -       -         -    
E[exp(-u)]                    0.706426          -       -         -    
-----[ Tests vs. No Inefficiency ]-----
Chisq = 2*[LogL(H0)-LogL(H1)]  =   7.63398 
Kodde-Palm C*:  95%: 2.70554   99%: 5.41189 
Log likelihood status: successful convergence  

[... Group medium and large output omitted for brevity ...]

------------------------------------------------------------ 
Metafrontier Coefficients (lp):
  (LP: deterministic envelope - no estimated parameters)

------------------------------------------------------------ 
Efficiency Statistics (group means):
------------------------------------------------------------ 
       N_obs N_valid TE_group_BC TE_group_JLMS TE_meta_BC TE_meta_JLMS  MTR_BC MTR_JLMS
small    125     125     0.71065       0.70090    0.64126      0.63256 0.89981  0.90007
medium   104     104     0.71253       0.70965    0.68204      0.67938 0.95597  0.95703
large    115     115     0.74772       0.74406    0.72186      0.71844 0.96521  0.96558

Overall:
TE_group_BC=0.7236  TE_group_JLMS=0.7182
TE_meta_BC=0.6817   TE_meta_JLMS=0.6769
MTR_BC=0.9403     MTR_JLMS=0.9421
------------------------------------------------------------ 
Total Log-likelihood: -74.28939 
AIC: 184.5788   BIC: 253.7103   HQIC: 212.113 
```
</details>

```r
# Retrieve individual efficiency and MTR estimates
ef_lp <- efficiencies(meta_sfacross_lp)
head(ef_lp[ef_lp$group == "small", ])
```

---

### 1b. QP Metafrontier (`groupType = "sfacross"`, `metaMethod = "qp"`)

Quadratic programming envelope — minimises the sum of squared deviations from group frontier predictions subject to the envelope constraint.

```r
meta_sfacross_qp <- sfametafrontier(
  formula    = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data       = ricephil,
  group      = "group",
  S          = 1,
  udist      = "hnormal",
  groupType  = "sfacross",
  metaMethod = "qp"
)
summary(meta_sfacross_qp)
```

<details>
  <summary>Toggle to see the output</summary>

```plaintext
============================================================ 
Stochastic Metafrontier Analysis
Metafrontier method: Quadratic Programming (QP) Metafrontier 
... [group coefficients identical to LP above] ...

------------------------------------------------------------ 
Metafrontier Coefficients (qp):
              Estimate Std. Error z value  Pr(>|z|)    
(Intercept) -0.6117795  0.0291793 -20.966 < 2.2e-16 ***
log(AREA)    0.3937843  0.0073209  53.789 < 2.2e-16 ***
log(LABOR)   0.2791273  0.0077215  36.150 < 2.2e-16 ***
log(NPK)     0.2409454  0.0046846  51.434 < 2.2e-16 ***

------------------------------------------------------------ 
Efficiency Statistics (group means):
------------------------------------------------------------ 
       N_obs N_valid TE_group_BC TE_group_JLMS TE_meta_BC TE_meta_JLMS  MTR_BC MTR_JLMS
small    125     125     0.71065       0.70090    0.64037      0.63156 0.89972  0.89972
medium   104     104     0.71253       0.70965    0.66998      0.66727 0.94053  0.94053
large    115     115     0.74772       0.74406    0.72290      0.71937 0.96676  0.96676

Overall:
TE_group_BC=0.7236  TE_group_JLMS=0.7182
TE_meta_BC=0.6777   TE_meta_JLMS=0.6727
MTR_BC=0.9357     MTR_JLMS=0.9357
```
</details>

---

### 1c. Two-stage SFA Metafrontier — Huang et al. (2014) (`sfaApproach = "huang"`)

The group-specific fitted frontier values serve as the dependent variable in a second-stage pooled SFA. The technology gap `U` and noise `V` are estimated stochastically.

```r
meta_sfacross_huang <- sfametafrontier(
  formula     = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data        = ricephil,
  group       = "group",
  S           = 1,
  udist       = "hnormal",
  groupType   = "sfacross",
  metaMethod  = "sfa",
  sfaApproach = "huang"
)
summary(meta_sfacross_huang)
```

<details>
  <summary>Toggle to see the output</summary>

```plaintext
============================================================ 
Stochastic Metafrontier Analysis
Metafrontier method: SFA Metafrontier [Huang et al. (2014), two-stage] 
SFA approach       : huang 
Group estimator    : sfacross 
Groups ( 3 ): small, medium, large 
Total observations : 344 
============================================================ 

[... group output identical to LP/QP above ...]

------------------------------------------------------------ 
Metafrontier Coefficients (sfa):
Meta-optim solver  : BFGS maximization 
              Estimate Std. Error z value  Pr(>|z|)    
(Intercept) -0.6114342  0.0414990 -14.734 < 2.2e-16 ***
log(AREA)    0.3937848  0.0072782  54.105 < 2.2e-16 ***
log(LABOR)   0.2791270  0.0076764  36.361 < 2.2e-16 ***
log(NPK)     0.2409454  0.0046573  51.735 < 2.2e-16 ***

  Meta-frontier Variance & Efficiency Statistics:
                                Estimate Std. Error z value Pr(>|z|)    
Sigma-squared(u)              1.8630e-07 3.2100e-05  0.0058   0.9954    
Sigma(u)                      4.3162e-04 3.7185e-02  0.0116   0.9907    
Sigma-squared(v)              1.4832e-03 1.1369e-04 13.0462   <2e-16 ***
Sigma(v)                      3.8512e-02 1.4760e-03 26.0925   <2e-16 ***
E[u]                          3.4438e-04          -       -        -    
E[exp(-u)]                    9.9966e-01          -       -        -    

[Note: Degenerate second-stage is expected with near-identical group fitted value surfaces]
```
</details>

---

### 1d. Two-stage SFA Metafrontier — O'Donnell et al. (2008) (`sfaApproach = "ordonnell"`)

The LP deterministic envelope is used as the dependent variable in the second stage; the SFA quantifies the stochastic variation around the envelope.

```r
meta_sfacross_odonnell <- sfametafrontier(
  formula     = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data        = ricephil,
  group       = "group",
  S           = 1,
  udist       = "hnormal",
  groupType   = "sfacross",
  metaMethod  = "sfa",
  sfaApproach = "ordonnell"
)
summary(meta_sfacross_odonnell)
```

---

## Section 2: Latent Class SFA Group Frontier (`groupType = "sfalcmcross"`)

When technology groups are unobserved, a pooled latent class model (`sfalcmcross`) is fitted on all data. The resulting class assignments (by maximum posterior probability) serve as the technology groups for the metafrontier.

### Data Preparation

```r
data("utility", package = "sfaR")
# No group variable needed for pooled LCM (groupType = "sfalcmcross" with no group argument)
```

---

### 2a. LCM + LP Metafrontier

```r
meta_lcm_lp <- sfametafrontier(
  formula    = log(tc/wf) ~ log(y) + log(wl/wf) + log(wk/wf),
  data       = utility,
  S          = -1,
  groupType  = "sfalcmcross",
  lcmClasses = 2,
  metaMethod = "lp"
)
summary(meta_lcm_lp)
```

<details>
  <summary>Toggle to see the output</summary>

```plaintext
============================================================ 
Stochastic Metafrontier Analysis
Metafrontier method: Linear Programming (LP) Metafrontier 
Stochastic Cost Frontier, e = v + u 
Group approach     : Latent Class Stochastic Frontier Analysis 
Group estimator    : sfalcmcross 
  (Pooled LCM - latent classes used as groups)
Groups ( 2 ): Class_1, Class_2 
Total observations : 791 
Distribution       : hnormal 
============================================================ 

------------------------------------------------------------ 
Pooled LCM (2 classes) on all data (N = 791)  Log-likelihood: 61.35325
------------------------------------------------------------

  -- Latent Class 1 --
  Frontier:
            Coefficient  Std. Error z value  Pr(>|z|)    
(Intercept) -1.4472e+00  3.9123e-05  -36992 < 2.2e-16 ***
log(y)       8.4541e-01  2.3364e-06  361846 < 2.2e-16 ***
log(wl/wf)   3.5408e-01  4.4754e-06   79118 < 2.2e-16 ***
log(wk/wf)   4.2883e-01  1.3682e-05   31343 < 2.2e-16 ***
  Sigma_u=0.4136  Sigma_v=0.0000  Gamma=1.0000

  -- Latent Class 2 --
  Frontier:
            Coefficient  Std. Error   z value  Pr(>|z|)    
(Intercept) -2.0490e+00  2.5608e-05 -80011.07 < 2.2e-16 ***
log(y)       1.0079e+00  4.1082e-04   2453.37 < 2.2e-16 ***
log(wl/wf)  -2.5916e-02  6.3375e-05   -408.93 < 2.2e-16 ***
log(wk/wf)   8.8450e-01  6.9315e-05  12760.74 < 2.2e-16 ***
  Sigma_u=0.2218  Sigma_v=0.0835  Gamma=0.8759  Lambda=2.6573

  -- Class Membership (logit) --
                Coefficient  Std. Error  z value  Pr(>|z|)    
Cl1_(Intercept) -6.0163e-01  5.0453e-07 -1192458 < 2.2e-16 ***

------------------------------------------------------------ 
Metafrontier Coefficients (lp):
  (LP: deterministic envelope - no estimated parameters)

------------------------------------------------------------ 
Efficiency Statistics (group means):
------------------------------------------------------------ 
        N_obs N_valid TE_group_BC TE_group_JLMS TE_meta_BC TE_meta_JLMS  MTR_BC MTR_JLMS
Class_1   202     202     0.68291       0.68291    0.68291      0.68291 1.00000  1.00000
Class_2   589     589     0.85142       0.84951    0.85142      0.84951 1.00000  1.00000

Overall:
TE_group_BC=0.7672  TE_group_JLMS=0.7662
TE_meta_BC=0.7672   TE_meta_JLMS=0.7662
MTR_BC=1.0000     MTR_JLMS=1.0000

------------------------------------------------------------ 
Posterior Class Membership (pooled LCM):
------------------------------------------------------------ 
        % assigned Mean post. prob.
Class 1       25.5            0.354
Class 2       74.5            0.646
------------------------------------------------------------ 
Total Log-likelihood: 61.35325 
AIC: -96.70649   BIC: -35.95362
```
</details>

```r
# Retrieve efficiencies including per-class posterior probabilities
ef_lcm_lp <- efficiencies(meta_lcm_lp)
# Columns include: Group_c, u_g, TE_group_JLMS, TE_group_BC, TE_group_BC_reciprocal,
#                  PosteriorProb_c, PosteriorProb_c1, PosteriorProb_c2,
#                  PriorProb_c1, PriorProb_c2, u_c1, u_c2,
#                  teBC_c1, teBC_c2, u_meta, TE_meta_JLMS, TE_meta_BC, MTR_JLMS, MTR_BC
```

---

### 2b. LCM + QP Metafrontier

```r
meta_lcm_qp <- sfametafrontier(
  formula    = log(tc/wf) ~ log(y) + log(wl/wf) + log(wk/wf),
  data       = utility,
  S          = -1,
  groupType  = "sfalcmcross",
  lcmClasses = 2,
  metaMethod = "qp"
)
summary(meta_lcm_qp)
```

---

### 2c. LCM + Two-stage SFA Metafrontier — Huang et al. (2014)

```r
meta_lcm_huang <- sfametafrontier(
  formula     = log(tc/wf) ~ log(y) + log(wl/wf) + log(wk/wf),
  data        = utility,
  S           = -1,
  groupType   = "sfalcmcross",
  lcmClasses  = 2,
  metaMethod  = "sfa",
  sfaApproach = "huang"
)
summary(meta_lcm_huang)
```

<details>
  <summary>Toggle to see the output</summary>

```plaintext
============================================================ 
Stochastic Metafrontier Analysis
Metafrontier method: SFA Metafrontier [Huang et al. (2014), two-stage] 
SFA approach       : huang
Group estimator    : sfalcmcross 
Groups ( 2 ): Class_1, Class_2 
Total observations : 791 
============================================================ 

[... pooled LCM output as above ...]

------------------------------------------------------------ 
Metafrontier Coefficients (sfa):
Meta-optim solver  : BFGS maximization 
              Estimate Std. Error  z value  Pr(>|z|)    
(Intercept) -2.2495079  0.0686629 -32.7616 < 2.2e-16 ***
log(y)       0.9909918  0.0024687 401.4291 < 2.2e-16 ***
log(wl/wf)   0.0399586  0.0106166   3.7638 0.0001674 ***
log(wk/wf)   0.7890986  0.0143801  54.8745 < 2.2e-16 ***

  Meta-frontier Variance & Efficiency Statistics:
Sigma-squared(u)              2.7127e-02 1.5306e-03  17.7234 < 2.2e-16 ***
Sigma(u)                      1.6470e-01 4.6465e-03  35.4468 < 2.2e-16 ***
Sigma-squared(v)              6.8006e-04 8.8176e-05   7.7125 1.234e-14 ***
Sigma(v)                      2.6078e-02 1.6906e-03  15.4250 < 2.2e-16 ***
Gamma = sigma(u)^2/sigma^2    9.7554e-01 3.4242e-03 284.8977 < 2.2e-16 ***
Lambda = sigma(u)/sigma(v)    6.3159e+00 4.5324e-01  13.9348 < 2.2e-16 ***
LR Chisq (test of no inefficiency) = 342.07861 ***

------------------------------------------------------------ 
Efficiency Statistics (group means):
------------------------------------------------------------ 
        N_obs N_valid TE_group_BC TE_group_JLMS TE_meta_BC TE_meta_JLMS  MTR_BC MTR_JLMS
Class_1   202     202     0.68291       0.68291    0.51861      0.51845 0.76693  0.76669
Class_2   589     589     0.85142       0.84951    0.80511      0.80308 0.94559  0.94532

Overall:
TE_group_BC=0.7672  TE_group_JLMS=0.7662
TE_meta_BC=0.6619   TE_meta_JLMS=0.6608
MTR_BC=0.8563     MTR_JLMS=0.8560
------------------------------------------------------------ 
Total Log-likelihood: 820.9822 
AIC: -1603.964   BIC: -1515.172
```
</details>

---

### 2d. LCM + O'Donnell et al. (2008) Stochastic Metafrontier

```r
meta_lcm_odonnell <- sfametafrontier(
  formula     = log(tc/wf) ~ log(y) + log(wl/wf) + log(wk/wf),
  data        = utility,
  S           = -1,
  groupType   = "sfalcmcross",
  lcmClasses  = 2,
  metaMethod  = "sfa",
  sfaApproach = "ordonnell"
)
summary(meta_lcm_odonnell)
```

---

## Section 3: Sample Selection SFA Group Frontier (`groupType = "sfaselectioncross"`)

When the observed sample is not random (e.g., only firms above a revenue threshold are surveyed), sample selection bias can distort frontier estimates. `sfaselectioncross` corrects for this using the two-step approach of Greene (2010). Only selected observations (`d == 1`) participate in the frontier and metafrontier; efficiency estimates for non-selected observations are `NA`.

### Data Preparation (Simulated)

```r
N <- 2000; set.seed(12345)
z1 <- rnorm(N); z2 <- rnorm(N)
v1 <- rnorm(N); v2 <- rnorm(N)
g  <- rnorm(N)
e1 <- v1
e2 <- 0.7071 * (v1 + v2)
ds <- z1 + z2 + e1
d  <- ifelse(ds > 0, 1, 0)        # binary selection indicator: 1 = selected
group <- ifelse(g > 0, 1, 0)      # two technology groups
u  <- abs(rnorm(N))
x1 <- rnorm(N); x2 <- rnorm(N)
y  <- x1 + x2 + e2 - u
dat <- as.data.frame(cbind(y=y, x1=x1, x2=x2, z1=z1, z2=z2, d=d, group=group))
# About 50% of observations are selected:
table(dat$d)
```

```plaintext
  0   1 
 987 1013 
```

---

### 3a. sfaselectioncross + LP Metafrontier

```r
meta_sel_lp <- sfametafrontier(
  formula    = y ~ x1 + x2,
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
  metaMethod = "lp"
)
summary(meta_sel_lp)
```

<details>
  <summary>Toggle to see the output</summary>

```plaintext
============================================================ 
Stochastic Metafrontier Analysis
Metafrontier method: Linear Programming (LP) Metafrontier 
Stochastic Production/Profit Frontier, e = v - u 
Group approach     : Sample Selection Stochastic Frontier Analysis 
Group estimator    : sfaselectioncross 
Group optim solver : BFGS maximization 
Groups ( 2 ): 0, 1 
Total observations : 2000 
Distribution       : hnormal 
============================================================ 

------------------------------------------------------------ 
Group: 0 (N = 994)  Log-likelihood: -920.25257
------------------------------------------------------------
  Frontier equation:
            Coefficient Std. Error z value Pr(>|z|)    
(Intercept)    0.246183   0.136535  1.8031  0.07138 .  
x1             0.932107   0.049870 18.6909  < 2e-16 ***
x2             1.030221   0.047076 21.8844  < 2e-16 ***
  Var(u) parameters:
               Coefficient Std. Error z value Pr(>|z|)
Zu_(Intercept)     0.33533    0.26007  1.2894   0.1973
  Var(v) parameters:
               Coefficient Std. Error z value Pr(>|z|)
Zv_(Intercept)    -0.21841    0.16628 -1.3135    0.189
  Selection bias parameter (rho):
    Coefficient Std. Error z value  Pr(>|z|)    
rho     0.70836    0.10708  6.6155 3.703e-11 ***

  Variance & Efficiency Statistics:
Sigma-squared(u)              1.398400   0.363684  3.8451 0.0001205 ***
Sigma(u)                      1.182540   0.153773  7.6902 1.469e-14 ***
Gamma = sigma(u)^2/sigma^2    0.635003   0.092925  6.8335 8.286e-12 ***
Lambda = sigma(u)/sigma(v)    1.318994   0.264411  4.9884 6.087e-07 ***
E[u]                          0.943530                                   
E[exp(-u)]                    0.476861                                   
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Group: 1 (N = 1006)  Log-likelihood: -979.61766
------------------------------------------------------------
  Frontier equation:
            Coefficient Std. Error z value Pr(>|z|)    
(Intercept)    0.052396   0.133373  0.3929   0.6944    
x1             1.033472   0.047833 21.6057   <2e-16 ***
x2             1.044373   0.054269 19.2442   <2e-16 ***
  Selection bias parameter (rho):
    Coefficient Std. Error z value  Pr(>|z|)    
rho     0.88016    0.10556  8.3376 < 2.2e-16 ***

  Variance & Efficiency Statistics:
Sigma-squared(u)              1.369321   0.418188  3.2744  0.001059 ** 
Sigma(u)                      1.170180   0.178685  6.5488 5.799e-11 ***
E[u]                          0.933669                                   
E[exp(-u)]                    0.479768                                   
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Metafrontier Coefficients (lp):
  (LP: deterministic envelope - no estimated parameters)

------------------------------------------------------------ 
Efficiency Statistics (group means):
------------------------------------------------------------ 
  N_obs N_valid TE_group_BC TE_group_JLMS TE_meta_BC TE_meta_JLMS  MTR_BC MTR_JLMS
0   994     489     0.41302       0.37188    0.41254      0.37145 0.99892  0.99892
1  1006     498     0.40229       0.36337    0.33354      0.30120 0.82816  0.82816

Overall:
TE_group_BC=0.4077  TE_group_JLMS=0.3676
TE_meta_BC=0.3730   TE_meta_JLMS=0.3363
MTR_BC=0.9135     MTR_JLMS=0.9135
[N_valid: selected observations only; non-selected observations have NA efficiencies]
------------------------------------------------------------ 
Total Log-likelihood: -1899.87 
AIC: 3823.74   BIC: 3890.951
```
</details>

```r
# Efficiencies: non-selected observations have NA
ef_sel_lp <- efficiencies(meta_sel_lp)
# Selected observations in group 0:
head(ef_sel_lp[ef_sel_lp$group == 0 & !is.na(ef_sel_lp$TE_group_BC), ])
```

---

### 3b. sfaselectioncross + QP Metafrontier

```r
meta_sel_qp <- sfametafrontier(
  formula    = y ~ x1 + x2,
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

<details>
  <summary>Toggle to see the output</summary>

```plaintext
[... group output same as LP above ...]

------------------------------------------------------------ 
Metafrontier Coefficients (qp):
              Estimate Std. Error z value  Pr(>|z|)    
(Intercept) 0.24720807 0.00027233  907.77 < 2.2e-16 ***
x1          0.93504406 0.00027298 3425.32 < 2.2e-16 ***
x2          1.03090304 0.00028044 3675.96 < 2.2e-16 ***

Efficiency Statistics (group means):
  N_obs N_valid TE_group_BC TE_group_JLMS TE_meta_BC TE_meta_JLMS  MTR_BC MTR_JLMS
0   994     489     0.41302       0.37188    0.41227      0.37121 0.99818  0.99818
1  1006     498     0.40229       0.36337    0.33314      0.30084 0.82722  0.82722

Overall:
TE_group_BC=0.4077  TE_group_JLMS=0.3676
TE_meta_BC=0.3727   TE_meta_JLMS=0.3360
MTR_BC=0.9127     MTR_JLMS=0.9127
```
</details>

---

### 3c. sfaselectioncross + Two-stage SFA Metafrontier — Huang et al. (2014)

```r
meta_sel_huang <- sfametafrontier(
  formula     = y ~ x1 + x2,
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
  simType     = "halton",
  Nsim        = 300,
  prime       = 2L,
  burn        = 10,
  antithetics = FALSE,
  seed        = 12345,
  method      = "bfgs",
  itermax     = 2000,
  metaMethod  = "sfa",
  sfaApproach = "huang"
)
summary(meta_sel_huang)
```

<details>
  <summary>Toggle to see the output</summary>

```plaintext
============================================================ 
Stochastic Metafrontier Analysis
Metafrontier method: SFA Metafrontier [Huang et al. (2014), two-stage] 
SFA approach       : huang 
Group estimator    : sfaselectioncross 
Groups ( 2 ): 0, 1 
Total observations : 2000 
============================================================ 

[... group output as above ...]

------------------------------------------------------------ 
Metafrontier Coefficients (sfa):
Meta-optim solver  : BFGS maximization 
             Estimate Std. Error  z value Pr(>|z|)    
(Intercept) 0.1482335  0.1139691   1.3006   0.1934    
x1          0.9895976  0.0034280 288.6802   <2e-16 ***
x2          1.0337159  0.0035217 293.5249   <2e-16 ***

  Meta-frontier Variance & Efficiency Statistics:
Sigma-squared(u)              2.6613e-06 4.6583e-04  0.0057   0.9954    
Sigma-squared(v)              1.1523e-02 5.4559e-04 21.1204   <2e-16 ***
E[u]                          1.3016e-03          -       -        -    
E[exp(-u)]                    9.9870e-01          -       -        -    

Efficiency Statistics (group means):
  N_obs N_valid TE_group_BC TE_group_JLMS TE_meta_BC TE_meta_JLMS  MTR_BC MTR_JLMS
0   994     489     0.41302       0.37188    0.41248      0.37140 0.99871  0.99871
1  1006     498     0.40229       0.36337    0.40176      0.36290 0.99869  0.99869

Overall:
TE_group_BC=0.4077  TE_group_JLMS=0.3676
TE_meta_BC=0.4071   TE_meta_JLMS=0.3671
MTR_BC=0.9987     MTR_JLMS=0.9987
```
</details>

---

### 3d. sfaselectioncross + O'Donnell et al. (2008) Stochastic Metafrontier

```r
meta_sel_odonnell <- sfametafrontier(
  formula     = y ~ x1 + x2,
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

---

## Output: Efficiency and Metatechnology Ratio Extraction

The `efficiencies()` function returns a data frame with one row per observation containing group-specific and metafrontier efficiency estimates and MTRs. The columns present depend on `groupType`:

| Column | Description |
|---|----|
| `group` / `Group_c` | Technology group identifier |
| `u_g` | Group-specific inefficiency — Jondrow et al. (1982) |
| `TE_group_JLMS` | Group TE — Jondrow et al. (1982): exp(−*u*) |
| `TE_group_BC` | Group TE — Battese & Coelli (1988): E[exp(−*u*)\|*ε*] |
| `TE_group_BC_reciprocal` | Reciprocal of Battese & Coelli (1988) group TE |
| `uLB_g`, `uUB_g` | Confidence bounds for *u* (*sfacross* only) |
| `m_g`, `TE_group_mode` | Mode-based inefficiency and TE (*sfacross* only) |
| `PosteriorProb_c`, `PosteriorProb_c1` … | Posterior class probabilities (*sfalcmcross* only) |
| `u_meta` | Metafrontier technology gap *U* |
| `TE_meta_JLMS` | Metafrontier TE (JLMS basis): TE_group_JLMS × MTR |
| `TE_meta_BC` | Metafrontier TE (BC basis): TE_group_BC × MTR |
| `MTR_JLMS` | Metatechnology ratio (JLMS basis) |
| `MTR_BC` | Metatechnology ratio (BC basis) |

```r
# Example: extract and print for group 1 selected farms only
ef_sel_lp <- efficiencies(meta_sel_lp)
sel_grp1  <- ef_sel_lp[ef_sel_lp$group == 1 & !is.na(ef_sel_lp$TE_group_BC), ]
summary(sel_grp1[, c("TE_group_BC", "TE_meta_BC", "MTR_BC")])
```

---

## References

- Battese, G. E., & Coelli, T. J. (1988). Prediction of firm-level technical efficiencies with a generalized frontier production function and panel data. *Journal of Econometrics*, 38(3), 387–399. <https://doi.org/10.1016/0304-4076(88)90053-X>
- Battese, G. E., Rao, D. S. P., & O'Donnell, C. J. (2004). A metafrontier production function for estimation of technical efficiencies and technology gaps for firms operating under different technologies. *Journal of Productivity Analysis*, 21(1), 91–103. <https://doi.org/10.1023/B:PROD.0000012454.06094.29>
- Dakpo, K. H., Desjeux, Y., & Latruffe, L. (2023). sfaR: Stochastic Frontier Analysis using R. R package version 1.0.1. <https://CRAN.R-project.org/package=sfaR>
- Greene, W. (2010). A stochastic frontier model with correction for sample selection. *Journal of Productivity Analysis*, 34(1), 15–24. <https://doi.org/10.1007/s11123-009-0159-1>
- Huang, C. J., Huang, T.-H., & Liu, N.-H. (2014). A new approach to estimating the metafrontier production function based on a stochastic frontier framework. *Journal of Productivity Analysis*, 42(3), 241–254. <https://doi.org/10.1007/s11123-014-0402-2>
- Jondrow, J., Lovell, C. A. K., Materov, I. S., & Schmidt, P. (1982). On the estimation of technical inefficiency in the stochastic frontier production function model. *Journal of Econometrics*, 19(2–3), 233–238. <https://doi.org/10.1016/0304-4076(82)90004-5>
- O'Donnell, C. J., Rao, D. S. P., & Battese, G. E. (2008). Metafrontier frameworks for the study of firm-level efficiencies and technology ratios. *Empirical Economics*, 34(2), 231–255. <https://doi.org/10.1007/s00181-007-0119-4>
- Orea, L., & Kumbhakar, S. C. (2004). Efficiency measurement using a latent class stochastic frontier model. *Empirical Economics*, 29(1), 169–183. <https://doi.org/10.1007/s00181-003-0184-2>
