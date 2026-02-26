rm(list = ls())
remove.packages(c("sfaR", "metafrontieR"))
.rs.restartR()

rm(list = ls())
setwd("~/sfaR")

# install.packages("/Users/macbookpro/sfaR", repos=NULL, type="source")
# library(metafrontieR)

# Option B — load source directly without installing (faster for development):
pkgload::load_all("/Users/macbookpro/sfaR")

data("ricephil", package = "sfaR")

# Create technology groups by farm area quartiles
ricephil$group <- cut(
  ricephil$AREA,
  breaks = quantile(ricephil$AREA, probs = c(0, 1 / 3, 2 / 3, 1), na.rm = TRUE),
  labels = c("small", "medium", "large"),
  include.lowest = TRUE
)

# Check group sizes
table(ricephil$group)

# detach(ricephil)
# attach(ricephil)

# STANDARD LP METAFRONTIER

## Battese, Rao & O'Donnell (2004) — deterministic LP envelope
meta_lp <- sfametafrontier(
  formula = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data = ricephil,
  group = "group",
  S = 1,
  udist = "hnormal",
  metaMethod = "lp" # LP envelope: max group beta across all obs
)
summary(meta_lp)
print(meta_lp)
eff_lp <- efficiencies(meta_lp)
head(eff_lp)
#>   u_g  teGroup_JLMS  teGroup_BC  teMeta_BC  mtr

# STANDARD QP METAFRONTIER

## O'Donnell et al. (2008) — OLS on LP envelope
meta_qp <- sfametafrontier(
  formula = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data = ricephil,
  group = "group",
  S = 1,
  udist = "hnormal",
  metaMethod = "qp" # Smooth parametric envelope via OLS
)

summary(meta_qp)
coef(meta_qp) # Meta-frontier coefficients with SEs


# SFA O'Donnell Metafrontier
## O'Donnell et al. (2008) — second-stage SFA on LP envelope
meta_sfa_ord <- sfametafrontier(
  formula = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data = ricephil,
  group = "group",
  S = 1,
  udist = "hnormal",
  metaMethod = "sfa",
  sfaApproach = "ordonnell" # DV = max(X*beta_g) across groups
)

summary(meta_sfa_ord)
eff_ord <- efficiencies(meta_sfa_ord)
head(eff_ord)

# HUANG 2014

## Huang, Huang & Liu (2014) — second-stage SFA on own-group fitted values
meta_huang <- sfametafrontier(
  formula = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data = ricephil,
  group = "group",
  S = 1,
  udist = "hnormal",
  metaMethod = "sfa",
  sfaApproach = "huang" # DV = X*beta_{g(i)} for each obs's own group
)

summary(meta_huang)
eff_huang <- efficiencies(meta_huang)
head(eff_huang)

# Compare group-average MTR across groups
aggregate(
  mtr ~ group,
  data = cbind(
    ricephil[!is.na(ricephil$group), ],
    eff_huang
  ),
  FUN = mean,
  na.rm = TRUE
)

# Latent Class Group Frontiers + LP Metafrontier

## 2-class LCM per group, LP meta-stage
meta_lcm_lp <- sfametafrontier(
  formula = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data = ricephil,
  # group      = "group",
  S = 1,
  groupType = "sfalcmcross",
  lcmClasses = 2, # # of latent production classes within each group if group is specified
  whichStart = 2, # Random half-normal starting values
  initAlg = "nm",
  metaMethod = "lp"
)
summary(meta_lcm_lp)
print(meta_lcm_lp)
eff_lcm_lp <- efficiencies(meta_lcm_lp)


## 2-class LCM per group, QP meta-stage
meta_lcm_qp <- sfametafrontier(
  formula = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data = ricephil,
  # group      = "group",
  S = 1,
  groupType = "sfalcmcross",
  lcmClasses = 2, # # of latent production classes within each group if group is specified
  whichStart = 2, # Random half-normal starting values
  initAlg = "nm",
  metaMethod = "qp"
)
summary(meta_lcm_qp)
print(meta_lcm_qp)
eff_lcm_qp <- efficiencies(meta_lcm_qp)


# Latent Class Group Frontiers + Huang SFA Metafrontier
## 2-class LCM per group, Huang two-stage SFA meta-stage
meta_lcm_huang <- sfametafrontier(
  formula = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data = ricephil,
  # group       = "group",
  S = 1,
  groupType = "sfalcmcross",
  lcmClasses = 2,
  whichStart = 2,
  metaMethod = "sfa",
  sfaApproach = "huang" # Huang approach also works with LCM group frontiers
)

summary(meta_lcm_huang)

# Latent Class Group Frontiers + O'donnell (2008) SFA Metafrontier
## 2-class LCM per group, O'donnell (2008) two-stage SFA meta-stage
meta_lcm_ordonnell <- sfametafrontier(
  formula = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data = ricephil,
  # group       = "group",
  S = 1,
  groupType = "sfalcmcross",
  lcmClasses = 2,
  whichStart = 2,
  metaMethod = "sfa",
  sfaApproach = "ordonnell" # Huang approach also works with LCM group frontiers
)

summary(meta_lcm_ordonnell)


# Sample Selection Group Frontiers + LP Metafrontier

## Create a BALANCED selection indicator — later survey rounds (YEARDUM > 3)
## gives: small 81/125 | medium 65/104 | large 69/115 selected
ricephil$laterSurvey <- as.integer(ricephil$YEARDUM > 3)

## KEY API RULES for groupType = "sfaselectioncross":
## - formula = frontier ONLY (no | selected in LHS, no | 1 | 1 on RHS)
## - selectionF = TWO-SIDED: selection_var ~ instruments
## - method = "nr" for numerical stability

meta_sel_lp <- sfametafrontier(
  formula = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data = ricephil,
  group = "group",
  S = 1,
  groupType = "sfaselectioncross",
  selectionF = laterSurvey ~ EDYRS + AGE, # TWO-SIDED: LHS = selection dummy
  method = "nr",
  lType = "ghermite",
  Nsub = 20,
  metaMethod = "lp"
)
summary(meta_sel_lp)

eff_sel <- efficiencies(meta_sel_lp)
na.omit(eff_sel)
sum(!is.na(eff_sel$mtr)) # 215 obs with valid MTR

meta_sel_qp <- sfametafrontier(
  formula = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data = ricephil,
  group = "group",
  S = 1,
  groupType = "sfaselectioncross",
  selectionF = laterSurvey ~ EDYRS + AGE, # TWO-SIDED: LHS = selection dummy
  method = "nr",
  lType = "ghermite",
  Nsub = 20,
  metaMethod = "qp"
)
summary(meta_sel_qp)

eff_sel <- efficiencies(meta_sel_qp)
na.omit(eff_sel)
sum(!is.na(eff_sel$mtr)) # 215 obs with valid MTR


meta_sel_huang <- sfametafrontier(
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
  sfaApproach = "huang"
)
summary(meta_sel_huang)

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
summary(meta_sel_ordonnell)

# S3 Methods Available

print(meta_sel_ordonnell) # Quick model summary
summary(meta_sel_ordonnell) # Detailed group + meta parameter tables
coef(meta_sel_ordonnell) # Meta-frontier coefficients
vcov(meta_sel_ordonnell) # VCV of meta-frontier coefficients
logLik(meta_sel_ordonnell) # Total log-likelihood
nobs(meta_sel_ordonnell) # Total and meta-stage observations
fitted(meta_sel_ordonnell) # Meta-frontier fitted values
residuals(meta_sel_ordonnell) # Meta-stage residuals
ic(meta_sel_ordonnell) # AIC / BIC / HQIC
efficiencies(meta_sel_ordonnell) # TE_group, TE_meta, MTR for all observations
