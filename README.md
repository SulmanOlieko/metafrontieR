# metafrontieR

> **Metafrontier Analysis Routines**

An R package for implementing various metafrontier analyses for productivity/performance benchmarking, assessing efficiencies, and measuring technology gaps for firms operating under different technologies.

It contains routines for implementing:
1. The **deterministic envelope** proposed by [Battese et al. (2004)](https://doi.org/10.1023/B:PROD.0000012454.06094.29) and [O'Donnell et al. (2008)](https://doi.org/10.1007/s00181-007-0119-4) via linear and quadratic programming, as well as second-stage SFA.
2. The **stochastic metafrontier** proposed by [Huang et al. (2014)](https://doi.org/10.1007/s11123-014-0402-2).

The package also has functionalities for implementing **latent class stochastic metafrontier analysis** and **sample selection correction stochastic metafrontier models**.

> **Note:** The package depends on the `sfaR` package by [Dakpo et al. (2023)](https://github.com/hdakpo/sfaR).

---

## Installation

You can install the development version of `metafrontieR` from GitHub. The package automatically calls `sfaR` as its main dependency. `sfaR` contains the necessary routines for stochastic frontier analysis via maximum likelihood and maximum simulated likelihood for various specifications of the frontier and error distributions.

```r
# Install devtools if not already installed
if (!require("devtools")) install.packages("devtools")

# Install metafrontieR
devtools::install_github("SulmanOlieko/metafrontieR")
```

---

## Usage Examples

The following examples demonstrate how to use `metafrontieR`.

### 1. Data Preparation

First, lets use the `ricephil` dataset from the `sfaR` package, and then load the required data and create technology groups based on farm area quartiles.

```r
library(metafrontieR)

#* Please cite the 'metafrontieR' package as:
#Owili SO. (2026). metafrontieR: Metafrontier Analysis in R. R package version 1.0.0.

#See also: citation("metafrontieR")

#* For any questions, suggestions, or comments on the 'metafrontieR' package, you can contact the authors directly or visit:
  https://github.com/SulmanOlieko/metafrontieR/issues

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
We have artificially created these three groups. In practice, there should be at least some logical justification for formation of these groups. However, to empirically justify the use of a metafrontier, one you must determine whether the technologies used by the groups differ systemmatically. The usual way of performing testing this in stochastic frontier analysis is through a generalised likelihood ratio test. However, this can only be done after estimating the metafrontier.

### 2. Standard Metafrontier Approaches

#### Linear Programming (LP) Metafrontier (Non-parametric metafrontier)
Let's begin by estimating the deterministic LP envelope based on Battese, Rao & O'Donnell (2004). This method finds the max group beta across all observations.

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
```
This is the output we obtain:

<details>
  <summary>Toggle to see the output</summary>

  ============================================================ 
Stochastic Metafrontier Analysis
Metafrontier method: Linear Programming (LP) Metafrontier 
Stochastic Production/Profit Frontier, e = v - u 
Group estimator    : sfacross 
Groups ( 3 ): small, medium, large 
Total observations : 344 
Distribution       : hnormal 
============================================================ 

------------------------------------------------------------ 
Group: small (N = 125)  Log-likelihood: -50.98578
------------------------------------------------------------ 
               Coefficient Std. Error z value  Pr(>|z|)    
(Intercept)      -1.587445   0.512745 -3.0960  0.001962 ** 
log(AREA)         0.240139   0.118343  2.0292  0.042441 *  
log(LABOR)        0.434645   0.122915  3.5361  0.000406 ***
log(NPK)          0.305164   0.057015  5.3523 8.682e-08 ***
Zu_(Intercept)   -1.450932   0.298670 -4.8580 1.186e-06 ***
Zv_(Intercept)   -2.934055   0.354013 -8.2880 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Variance & Efficiency Statistics (delta-method SEs):
           Estimate Std. Error z value  Pr(>|z|)    
sigma_u    0.484099   0.072293  6.6963 2.137e-11 ***
sigma_v    0.230610   0.040819  5.6495 1.609e-08 ***
sigma      0.536221   0.054820  9.7814 < 2.2e-16 ***
gamma      0.815044   0.090288  9.0271 < 2.2e-16 ***
lambda     2.099212   0.628650  3.3392 0.0008401 ***
E[u]       0.386255          —       —         —    
E[exp(-u)] 0.706426          —       —         —    

------------------------------------------------------------ 
Group: medium (N = 104)  Log-likelihood: -15.28164
------------------------------------------------------------ 
               Coefficient Std. Error z value  Pr(>|z|)    
(Intercept)      -0.081817   0.506685 -0.1615 0.8717187    
log(AREA)         0.474101   0.139839  3.3903 0.0006981 ***
log(LABOR)        0.179351   0.102014  1.7581 0.0787310 .  
log(NPK)          0.202545   0.081302  2.4913 0.0127289 *  
Zu_(Intercept)   -1.513671   0.235495 -6.4276 1.296e-10 ***
Zv_(Intercept)   -4.548464   0.764291 -5.9512 2.661e-09 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Variance & Efficiency Statistics (delta-method SEs):
           Estimate Std. Error z value  Pr(>|z|)    
sigma_u    0.469149   0.055241  8.4928 < 2.2e-16 ***
sigma_v    0.102876   0.039314  2.6168  0.008876 ** 
sigma      0.480296   0.048721  9.8580 < 2.2e-16 ***
gamma      0.954121   0.041080 23.2259 < 2.2e-16 ***
lambda     4.560336   2.139857  2.1311  0.033078 *  
E[u]       0.374326          —       —         —    
E[exp(-u)] 0.713297          —       —         —    

------------------------------------------------------------ 
Group: large (N = 115)  Log-likelihood: -8.02197
------------------------------------------------------------ 
               Coefficient Std. Error  z value  Pr(>|z|)    
(Intercept)      -1.311937   0.418592  -3.1342 0.0017234 ** 
log(AREA)         0.382776   0.142975   2.6772 0.0074236 ** 
log(LABOR)        0.421047   0.109924   3.8303 0.0001280 ***
log(NPK)          0.231427   0.060646   3.8160 0.0001356 ***
Zu_(Intercept)   -1.786729   0.201765  -8.8555 < 2.2e-16 ***
Zv_(Intercept)   -4.269633   0.405838 -10.5205 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Variance & Efficiency Statistics (delta-method SEs):
           Estimate Std. Error z value  Pr(>|z|)    
sigma_u    0.409276   0.041289  9.9125 < 2.2e-16 ***
sigma_v    0.118266   0.023998  4.9281 8.304e-07 ***
sigma      0.426021   0.036947 11.5306 < 2.2e-16 ***
gamma      0.922935   0.037885 24.3615 < 2.2e-16 ***
lambda     3.460635   0.921642  3.7549 0.0001734 ***
E[u]       0.326555          —       —         —    
E[exp(-u)] 0.741946          —       —         —    

------------------------------------------------------------ 
Metafrontier Coefficients (lp):
  (LP: deterministic envelope — no estimated parameters)

------------------------------------------------------------ 
Efficiency Statistics (group means):
------------------------------------------------------------ 
       N_obs N_valid TE_group_BC TE_meta_BC MTR_mean
small    125     125     0.71065    0.64126  0.89981
medium   104     104     0.71253    0.68204  0.95597
large    115     115     0.74772    0.72186  0.96521

Overall: TE_group=0.7236  TE_meta=0.6817  MTR=0.9403
------------------------------------------------------------ 
Total Log-likelihood: -74.28939 
AIC: 184.5788   BIC: 253.7103   HQIC: 212.113 
------------------------------------------------------------ 
Model was estimated on : Feb Fri 27, 2026 at 10:24
</details>









============================================================ 
Stochastic Metafrontier Analysis
Metafrontier method: Linear Programming (LP) Metafrontier 
Stochastic Production/Profit Frontier, e = v - u 
Group estimator    : sfacross 
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
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Variance & Efficiency Statistics (delta-method SEs):
           Estimate Std. Error z value  Pr(>|z|)    
sigma_u    0.484099   0.072293  6.6963 2.137e-11 ***
sigma_v    0.230610   0.040819  5.6495 1.609e-08 ***
sigma      0.536221   0.054820  9.7814 < 2.2e-16 ***
gamma      0.815044   0.090288  9.0271 < 2.2e-16 ***
lambda     2.099212   0.628650  3.3392 0.0008401 ***
E[u]       0.386255          —       —         —    
E[exp(-u)] 0.706426          —       —         —    

------------------------------------------------------------ 
Group: medium (N = 104)  Log-likelihood: -15.28164
------------------------------------------------------------ 
               Coefficient Std. Error z value  Pr(>|z|)    
(Intercept)      -0.081817   0.506685 -0.1615 0.8717187    
log(AREA)         0.474101   0.139839  3.3903 0.0006981 ***
log(LABOR)        0.179351   0.102014  1.7581 0.0787310 .  
log(NPK)          0.202545   0.081302  2.4913 0.0127289 *  
Zu_(Intercept)   -1.513671   0.235495 -6.4276 1.296e-10 ***
Zv_(Intercept)   -4.548464   0.764291 -5.9512 2.661e-09 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Variance & Efficiency Statistics (delta-method SEs):
           Estimate Std. Error z value  Pr(>|z|)    
sigma_u    0.469149   0.055241  8.4928 < 2.2e-16 ***
sigma_v    0.102876   0.039314  2.6168  0.008876 ** 
sigma      0.480296   0.048721  9.8580 < 2.2e-16 ***
gamma      0.954121   0.041080 23.2259 < 2.2e-16 ***
lambda     4.560336   2.139857  2.1311  0.033078 *  
E[u]       0.374326          —       —         —    
E[exp(-u)] 0.713297          —       —         —    

------------------------------------------------------------ 
Group: large (N = 115)  Log-likelihood: -8.02197
------------------------------------------------------------ 
               Coefficient Std. Error  z value  Pr(>|z|)    
(Intercept)      -1.311937   0.418592  -3.1342 0.0017234 ** 
log(AREA)         0.382776   0.142975   2.6772 0.0074236 ** 
log(LABOR)        0.421047   0.109924   3.8303 0.0001280 ***
log(NPK)          0.231427   0.060646   3.8160 0.0001356 ***
Zu_(Intercept)   -1.786729   0.201765  -8.8555 < 2.2e-16 ***
Zv_(Intercept)   -4.269633   0.405838 -10.5205 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Variance & Efficiency Statistics (delta-method SEs):
           Estimate Std. Error z value  Pr(>|z|)    
sigma_u    0.409276   0.041289  9.9125 < 2.2e-16 ***
sigma_v    0.118266   0.023998  4.9281 8.304e-07 ***
sigma      0.426021   0.036947 11.5306 < 2.2e-16 ***
gamma      0.922935   0.037885 24.3615 < 2.2e-16 ***
lambda     3.460635   0.921642  3.7549 0.0001734 ***
E[u]       0.326555          —       —         —    
E[exp(-u)] 0.741946          —       —         —    

------------------------------------------------------------ 
Metafrontier Coefficients (lp):
  (LP: deterministic envelope — no estimated parameters)

------------------------------------------------------------ 
Efficiency Statistics (group means):
------------------------------------------------------------ 
       N_obs N_valid TE_group_BC TE_meta_BC MTR_mean
small    125     125     0.71065    0.64126  0.89981
medium   104     104     0.71253    0.68204  0.95597
large    115     115     0.74772    0.72186  0.96521

Overall: TE_group=0.7236  TE_meta=0.6817  MTR=0.9403
------------------------------------------------------------ 
Total Log-likelihood: -74.28939 
AIC: 184.5788   BIC: 253.7103   HQIC: 212.113 
------------------------------------------------------------ 
Model was estimated on : Feb Fri 27, 2026 at 10:24 


You can call efficiencies as:

```r
eff_lp <- efficiencies(meta_lp)
head(eff_lp)
```
        u_g teGroup_JLMS teGroup_BC teMeta_BC       mtr
1 0.2697165    0.7635959  0.7673345 0.6773549 0.8827375
2 0.3515642    0.7035867  0.7080897 0.6896274 0.9739266
3 0.2774565    0.7577085  0.7623358 0.7416598 0.9728780
4 0.1710417    0.8427864  0.8461331 0.8461331 1.0000000
5 0.2119629    0.8089947  0.8133556 0.7925093 0.9743700
6 0.1987499    0.8197549  0.8275685 0.7261201 0.8774139


#### Quadratic Programming (QP) Metafrontier
Now let's estimate a smooth parametric envelope via OLS based on O'Donnell et al. (2008).

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
We obtain the following output:

============================================================ 
Stochastic Metafrontier Analysis
Metafrontier method: Quadratic Programming (QP) Metafrontier 
Stochastic Production/Profit Frontier, e = v - u 
Group estimator    : sfacross 
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
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Variance & Efficiency Statistics (delta-method SEs):
           Estimate Std. Error z value  Pr(>|z|)    
sigma_u    0.484099   0.072293  6.6963 2.137e-11 ***
sigma_v    0.230610   0.040819  5.6495 1.609e-08 ***
sigma      0.536221   0.054820  9.7814 < 2.2e-16 ***
gamma      0.815044   0.090288  9.0271 < 2.2e-16 ***
lambda     2.099212   0.628650  3.3392 0.0008401 ***
E[u]       0.386255          —       —         —    
E[exp(-u)] 0.706426          —       —         —    

------------------------------------------------------------ 
Group: medium (N = 104)  Log-likelihood: -15.28164
------------------------------------------------------------ 
               Coefficient Std. Error z value  Pr(>|z|)    
(Intercept)      -0.081817   0.506685 -0.1615 0.8717187    
log(AREA)         0.474101   0.139839  3.3903 0.0006981 ***
log(LABOR)        0.179351   0.102014  1.7581 0.0787310 .  
log(NPK)          0.202545   0.081302  2.4913 0.0127289 *  
Zu_(Intercept)   -1.513671   0.235495 -6.4276 1.296e-10 ***
Zv_(Intercept)   -4.548464   0.764291 -5.9512 2.661e-09 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Variance & Efficiency Statistics (delta-method SEs):
           Estimate Std. Error z value  Pr(>|z|)    
sigma_u    0.469149   0.055241  8.4928 < 2.2e-16 ***
sigma_v    0.102876   0.039314  2.6168  0.008876 ** 
sigma      0.480296   0.048721  9.8580 < 2.2e-16 ***
gamma      0.954121   0.041080 23.2259 < 2.2e-16 ***
lambda     4.560336   2.139857  2.1311  0.033078 *  
E[u]       0.374326          —       —         —    
E[exp(-u)] 0.713297          —       —         —    

------------------------------------------------------------ 
Group: large (N = 115)  Log-likelihood: -8.02197
------------------------------------------------------------ 
               Coefficient Std. Error  z value  Pr(>|z|)    
(Intercept)      -1.311937   0.418592  -3.1342 0.0017234 ** 
log(AREA)         0.382776   0.142975   2.6772 0.0074236 ** 
log(LABOR)        0.421047   0.109924   3.8303 0.0001280 ***
log(NPK)          0.231427   0.060646   3.8160 0.0001356 ***
Zu_(Intercept)   -1.786729   0.201765  -8.8555 < 2.2e-16 ***
Zv_(Intercept)   -4.269633   0.405838 -10.5205 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Variance & Efficiency Statistics (delta-method SEs):
           Estimate Std. Error z value  Pr(>|z|)    
sigma_u    0.409276   0.041289  9.9125 < 2.2e-16 ***
sigma_v    0.118266   0.023998  4.9281 8.304e-07 ***
sigma      0.426021   0.036947 11.5306 < 2.2e-16 ***
gamma      0.922935   0.037885 24.3615 < 2.2e-16 ***
lambda     3.460635   0.921642  3.7549 0.0001734 ***
E[u]       0.326555          —       —         —    
E[exp(-u)] 0.741946          —       —         —    

------------------------------------------------------------ 
Metafrontier Coefficients (qp):
              Estimate Std. Error z value  Pr(>|z|)    
(Intercept) -0.6117795  0.0291793 -20.966 < 2.2e-16 ***
log(AREA)    0.3937843  0.0073209  53.789 < 2.2e-16 ***
log(LABOR)   0.2791273  0.0077215  36.150 < 2.2e-16 ***
log(NPK)     0.2409454  0.0046846  51.434 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

------------------------------------------------------------ 
Efficiency Statistics (group means):
------------------------------------------------------------ 
       N_obs N_valid TE_group_BC TE_meta_BC MTR_mean
small    125     125     0.71065    0.64037  0.89972
medium   104     104     0.71253    0.66998  0.94053
large    115     115     0.74772    0.72290  0.96676

Overall: TE_group=0.7236  TE_meta=0.6777  MTR=0.9357
------------------------------------------------------------ 
Total Log-likelihood: -74.28939 
AIC: 192.5788   BIC: 277.0729   HQIC: 226.2318 
------------------------------------------------------------ 
Model was estimated on : Feb Fri 27, 2026 at 10:34 

We can harvest the efficiencies as:
```r
eff_qp <- efficiencies(meta_lp)
head(eff_qp)
```
        u_g teGroup_JLMS teGroup_BC teMeta_BC       mtr
1 0.2697165    0.7635959  0.7673345 0.6773549 0.8827375
2 0.3515642    0.7035867  0.7080897 0.6896274 0.9739266
3 0.2774565    0.7577085  0.7623358 0.7416598 0.9728780
4 0.1710417    0.8427864  0.8461331 0.8461331 1.0000000
5 0.2119629    0.8089947  0.8133556 0.7925093 0.9743700
6 0.1987499    0.8197549  0.8275685 0.7261201 0.8774139

> **Note:** It is clearly evident that LP and QP produce quite comparable and consistent results.

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
