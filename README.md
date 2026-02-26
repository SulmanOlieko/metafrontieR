# metafrontieR

> **Metafrontier Analysis Routines**

An R package for implementing various metafrontier analyses for productivity/performance benchmarking, assessing efficiencies, and measuring technology gaps for firms operating under different technologies.

It contains routines for implementing:
1. The **deterministic envelope** proposed by [O'Donnell et al. (2008)](https://doi.org/10.1007/s00181-007-0119-4) via linear and quadratic programming, as well as second-stage SFA.
2. The **stochastic metafrontier** proposed by [Huang et al. (2014)](https://doi.org/10.1007/s11123-014-0402-2).

The package also has functionalities for implementing **latent class stochastic metafrontier analysis** and **sample selection correction stochastic metafrontier models**.

> **Note:** The package heavily depends on the `sfaR` package by [Dakpo et al. (2023)](https://github.com/hdakpo/sfaR).

---

## Installation

You can install the development version of `metafrontieR` from GitHub. It automatically installs `sfaR` as its main dependency:

```r
# Install devtools if not already installed
if (!require("devtools")) install.packages("devtools")

# Install metafrontieR
devtools::install_github("SulmanOlieko/metafrontieR")
```

---

## Usage Examples

The following examples demonstrate how to use `metafrontieR` with the `ricephil` dataset from the `sfaR` package.

### 1. Data Preparation

First, load the required data and establish technology groups based on farm area quartiles.

```r
library(metafrontieR)
library(sfaR)

data("ricephil", package = "sfaR")

# Create technology groups by farm area quartiles
ricephil$group <- cut(
  ricephil$AREA,
  breaks = quantile(ricephil$AREA, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
  labels = c("small", "medium", "large"),
  include.lowest = TRUE
)

# Check group sizes
table(ricephil$group)
```

### 2. Standard Metafrontier Approaches

#### Linear Programming (LP) Metafrontier
Estimating the deterministic LP envelope based on Battese, Rao & O'Donnell (2004). This method finds the max group beta across all observations.

```r
meta_lp <- sfametafrontier(
  formula = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data = ricephil,
  group = "group",
  S = 1,
  udist = "hnormal",
  metaMethod = "lp" 
)

summary(meta_lp)
eff_lp <- efficiencies(meta_lp)
head(eff_lp)
```

#### Quadratic Programming (QP) Metafrontier
Estimating a smooth parametric envelope via OLS based on O'Donnell et al. (2008).

```r
meta_qp <- sfametafrontier(
  formula = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data = ricephil,
  group = "group",
  S = 1,
  udist = "hnormal",
  metaMethod = "qp" 
)

summary(meta_qp)
```

#### O'Donnell (2008) Two-Stage SFA Metafrontier
Second-stage SFA on the LP envelope where the dependent variable is `max(X*beta_g)` across groups.

```r
meta_sfa_ord <- sfametafrontier(
  formula = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data = ricephil,
  group = "group",
  S = 1,
  udist = "hnormal",
  metaMethod = "sfa",
  sfaApproach = "ordonnell" 
)

summary(meta_sfa_ord)
```

#### Huang (2014) Two-Stage SFA Metafrontier
Second-stage SFA on own-group fitted values. The dependent variable is `X*beta_{g(i)}` for each observation's own group.

```r
meta_huang <- sfametafrontier(
  formula = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data = ricephil,
  group = "group",
  S = 1,
  udist = "hnormal",
  metaMethod = "sfa",
  sfaApproach = "huang" 
)

summary(meta_huang)
```

---

### 3. Latent Class Metafrontier (LCM) Models

You can combine latent class stochastic frontier analysis per group with various metafrontier approaches.

#### LCM Group Frontiers + LP Metafrontier
```r
meta_lcm_lp <- sfametafrontier(
  formula = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data = ricephil,
  S = 1,
  groupType = "sfalcmcross",
  lcmClasses = 2,     # Number of latent production classes to be estimated
  whichStart = 2,     # Random half-normal starting values
  initAlg = "nm",
  metaMethod = "lp"
)

summary(meta_lcm_lp)
```

#### LCM Group Frontiers + Huang (2014) Metafrontier
```r
meta_lcm_huang <- sfametafrontier(
  formula = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data = ricephil,
  S = 1,
  groupType = "sfalcmcross",
  lcmClasses = 2,
  whichStart = 2,
  metaMethod = "sfa",
  sfaApproach = "huang" 
)

summary(meta_lcm_huang)
```

---

### 4. Sample Selection Correction Metafrontier Models

Address sample selection biases within groups before estimating the metafrontier.

```r
# Create a BALANCED selection indicator — later survey rounds (YEARDUM > 3)
ricephil$laterSurvey <- as.integer(ricephil$YEARDUM > 3)

## KEY API RULES for groupType = "sfaselectioncross":
## - formula = frontier ONLY
## - selectionF = TWO-SIDED: selection_var ~ instruments
## - method = "nr" for numerical stability

meta_sel_ordonnell <- sfametafrontier(
  formula = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data = ricephil,
  group = "group",
  S = 1,
  groupType = "sfaselectioncross",
  selectionF = laterSurvey ~ EDYRS + AGE,
  method = "nr",
  lType = "ghermite",
  Nsub = 20,
  metaMethod = "sfa",
  sfaApproach = "ordonnell"
)

# Various S3 methods available for interpreting the model:
print(meta_sel_ordonnell)      # Quick model summary
summary(meta_sel_ordonnell)    # Detailed group + meta parameter tables
coef(meta_sel_ordonnell)       # Meta-frontier coefficients
vcov(meta_sel_ordonnell)       # VCV of meta-frontier coefficients
logLik(meta_sel_ordonnell)     # Total log-likelihood
nobs(meta_sel_ordonnell)       # Total and meta-stage observations
fitted(meta_sel_ordonnell)     # Meta-frontier fitted values
residuals(meta_sel_ordonnell)  # Meta-stage residuals
efficiencies(meta_sel_ordonnell) # TE_group, TE_meta, MTR for all observations
```

---

## Authors
- **Sulman Olieko Owili** - Author and Creator 

This package is licensed under GPL (>= 3).
