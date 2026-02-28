# metafrontieR: An R package for metafrontier analysis. 
<img src="man/figures/logo.png" align="right" height="139" alt="metafrontieR logo" />

[![CodeFactor](https://www.codefactor.io/repository/github/SulmanOlieko/metafrontieR/badge)](https://www.codefactor.io/repository/github/SulmanOlieko/metafrontieR)
[![R-CMD-check](https://github.com/SulmanOlieko/metafrontieR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SulmanOlieko/metafrontieR/actions/workflows/R-CMD-check.yaml)
[![](https://img.shields.io/badge/devel%20version-1.0.0-darkred.svg)](https://github.com/SulmanOlieko/metafrontieR)
[![](https://img.shields.io/badge/license-GPL-blue)](https://github.com/SulmanOlieko/metafrontieR)
[![](https://img.shields.io/github/languages/code-size/SulmanOlieko/metafrontieR.svg)](https://github.com/SulmanOlieko/metafrontieR)

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

First, we use the `ricephil` dataset from the `sfaR` package. We create technology groups based on farm area quartiles.

```r
data("ricephil", package = "sfaR")

# Create technology groups by farm area quartiles
ricephil$group <- cut(ricephil$AREA,
                      breaks = quantile(ricephil$AREA, probs = c(0, 1 / 3, 2 / 3, 1), na.rm = TRUE),
                      labels = c("small", "medium", "large"),
                      include.lowest = TRUE
)

# Check group sizes
table(ricephil$group)

```
<details>
  <summary>Toggle to see the output</summary>

```plaintext

 small medium  large 
   125    104    115 
```
</details>

We have artificially created these three groups for the sake of demonstration. In practice, there should be empirical justification for the formation of these groups.

---

### 2. Standard Metafrontier Approaches

#### Linear Programming (LP) Metafrontier

Let's estimate the deterministic LP envelope based on Battese, Rao & O'Donnell (2004). This method finds the max group beta across all observations.

```r
attach(ricephil)
meta_lp <- sfametafrontier(
  formula    = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data       = ricephil,
  group      = "group",
  S          = 1,
  udist      = "hnormal",
  metaMethod = "lp" # LP envelope: max group beta across all obs
)
summary(meta_lp)

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
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Variance & Efficiency Statistics (delta-method SEs):
                              Estimate Std. Error z value  Pr(>|z|)    
Sigma-squared(u)              0.234352   0.069994  3.3482 0.0008135 ***
Sigma(u)                      0.484099   0.072293  6.6963 2.137e-11 ***
Sigma-squared(v)              0.053181   0.018827  2.8248 0.0047317 ** 
Sigma(v)                      0.230610   0.040819  5.6495 1.609e-08 ***
Sigma = Sqrt[(s^2(u)+s^2(v))] 0.536221   0.054820  9.7814 < 2.2e-16 ***
Gamma = sigma(u)^2/sigma^2    0.815044   0.090288  9.0271 < 2.2e-16 ***
Lambda = sigma(u)/sigma(v)    2.099212   0.628650  3.3392 0.0008401 ***
E[u]                          0.386255          -       -         -    
E[exp(-u)]                    0.706426          -       -         -    
-----[ Tests vs. No Inefficiency ]-----
Likelihood Ratio Test of Inefficiency
Deg. freedom for inefficiency model                        1 
Log Likelihood for OLS Log(H0) =                   -54.80277 
LR statistic: 
Chisq = 2*[LogL(H0)-LogL(H1)]  =                     7.63398 
Kodde-Palm C*:       95%: 2.70554               99%: 5.41189 
Coelli (1995) skewness test on OLS residuals
M3T: z                         =                    -3.57676 
M3T: p.value                   =                     0.00035 
Log likelihood status: successful convergence  

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
Sigma-squared(u)              0.2201004  0.0518325  4.2464 2.173e-05 ***
Sigma(u)                      0.4691486  0.0552410  8.4928 < 2.2e-16 ***
Sigma-squared(v)              0.0105834  0.0080888  1.3084  0.190737    
Sigma(v)                      0.1028759  0.0393135  2.6168  0.008876 ** 
Sigma = Sqrt[(s^2(u)+s^2(v))] 0.4802956  0.0487215  9.8580 < 2.2e-16 ***
Gamma = sigma(u)^2/sigma^2    0.9541214  0.0410801 23.2259 < 2.2e-16 ***
Lambda = sigma(u)/sigma(v)    4.5603356  2.1398573  2.1311  0.033078 *  
E[u]                          0.3743264          -       -         -    
E[exp(-u)]                    0.7132967          -       -         -    
-----[ Tests vs. No Inefficiency ]-----
Likelihood Ratio Test of Inefficiency
Deg. freedom for inefficiency model                        1 
Log Likelihood for OLS Log(H0) =                   -21.11323 
LR statistic: 
Chisq = 2*[LogL(H0)-LogL(H1)]  =                    11.66318 
Kodde-Palm C*:       95%: 2.70554               99%: 5.41189 
Coelli (1995) skewness test on OLS residuals
M3T: z                         =                    -2.91021 
M3T: p.value                   =                     0.00361 
Log likelihood status: successful convergence  

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
Sigma-squared(u)              0.1675072  0.0337970  4.9563 7.186e-07 ***
Sigma(u)                      0.4092764  0.0412888  9.9125 < 2.2e-16 ***
Sigma-squared(v)              0.0139869  0.0056764  2.4640 0.0137381 *  
Sigma(v)                      0.1182663  0.0239985  4.9281 8.304e-07 ***
Sigma = Sqrt[(s^2(u)+s^2(v))] 0.4260212  0.0369471 11.5306 < 2.2e-16 ***
Gamma = sigma(u)^2/sigma^2    0.9229346  0.0378850 24.3615 < 2.2e-16 ***
Lambda = sigma(u)/sigma(v)    3.4606346  0.9216422  3.7549 0.0001734 ***
E[u]                          0.3265553          -       -         -    
E[exp(-u)]                    0.7419464          -       -         -    
-----[ Tests vs. No Inefficiency ]-----
Likelihood Ratio Test of Inefficiency
Deg. freedom for inefficiency model                        1 
Log Likelihood for OLS Log(H0) =                   -16.96836 
LR statistic: 
Chisq = 2*[LogL(H0)-LogL(H1)]  =                    17.89279 
Kodde-Palm C*:       95%: 2.70554               99%: 5.41189 
Coelli (1995) skewness test on OLS residuals
M3T: z                         =                    -4.12175 
M3T: p.value                   =                     0.00004 
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Metafrontier Coefficients (lp):
  (LP: deterministic envelope - no estimated parameters)

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
Model was estimated on : Feb Sat 28, 2026 at 12:16 
```
</details>

#### Quadratic Programming (QP) Metafrontier

We can also estimate a smooth parametric envelope via Quadratic Programming (QP).

```r
meta_qp <- sfametafrontier(
  formula    = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data       = ricephil,
  group      = "group",
  S          = 1,
  udist      = "hnormal",
  metaMethod = "qp"
)
summary(meta_qp)

```
<details>
  <summary>Toggle to see the output</summary>

```plaintext
============================================================ 
Stochastic Metafrontier Analysis
Metafrontier method: Quadratic Programming (QP) Metafrontier 
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
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Variance & Efficiency Statistics (delta-method SEs):
                              Estimate Std. Error z value  Pr(>|z|)    
Sigma-squared(u)              0.234352   0.069994  3.3482 0.0008135 ***
Sigma(u)                      0.484099   0.072293  6.6963 2.137e-11 ***
Sigma-squared(v)              0.053181   0.018827  2.8248 0.0047317 ** 
Sigma(v)                      0.230610   0.040819  5.6495 1.609e-08 ***
Sigma = Sqrt[(s^2(u)+s^2(v))] 0.536221   0.054820  9.7814 < 2.2e-16 ***
Gamma = sigma(u)^2/sigma^2    0.815044   0.090288  9.0271 < 2.2e-16 ***
Lambda = sigma(u)/sigma(v)    2.099212   0.628650  3.3392 0.0008401 ***
E[u]                          0.386255          -       -         -    
E[exp(-u)]                    0.706426          -       -         -    
-----[ Tests vs. No Inefficiency ]-----
Likelihood Ratio Test of Inefficiency
Deg. freedom for inefficiency model                        1 
Log Likelihood for OLS Log(H0) =                   -54.80277 
LR statistic: 
Chisq = 2*[LogL(H0)-LogL(H1)]  =                     7.63398 
Kodde-Palm C*:       95%: 2.70554               99%: 5.41189 
Coelli (1995) skewness test on OLS residuals
M3T: z                         =                    -3.57676 
M3T: p.value                   =                     0.00035 
Log likelihood status: successful convergence  

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
Sigma-squared(u)              0.2201004  0.0518325  4.2464 2.173e-05 ***
Sigma(u)                      0.4691486  0.0552410  8.4928 < 2.2e-16 ***
Sigma-squared(v)              0.0105834  0.0080888  1.3084  0.190737    
Sigma(v)                      0.1028759  0.0393135  2.6168  0.008876 ** 
Sigma = Sqrt[(s^2(u)+s^2(v))] 0.4802956  0.0487215  9.8580 < 2.2e-16 ***
Gamma = sigma(u)^2/sigma^2    0.9541214  0.0410801 23.2259 < 2.2e-16 ***
Lambda = sigma(u)/sigma(v)    4.5603356  2.1398573  2.1311  0.033078 *  
E[u]                          0.3743264          -       -         -    
E[exp(-u)]                    0.7132967          -       -         -    
-----[ Tests vs. No Inefficiency ]-----
Likelihood Ratio Test of Inefficiency
Deg. freedom for inefficiency model                        1 
Log Likelihood for OLS Log(H0) =                   -21.11323 
LR statistic: 
Chisq = 2*[LogL(H0)-LogL(H1)]  =                    11.66318 
Kodde-Palm C*:       95%: 2.70554               99%: 5.41189 
Coelli (1995) skewness test on OLS residuals
M3T: z                         =                    -2.91021 
M3T: p.value                   =                     0.00361 
Log likelihood status: successful convergence  

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
Sigma-squared(u)              0.1675072  0.0337970  4.9563 7.186e-07 ***
Sigma(u)                      0.4092764  0.0412888  9.9125 < 2.2e-16 ***
Sigma-squared(v)              0.0139869  0.0056764  2.4640 0.0137381 *  
Sigma(v)                      0.1182663  0.0239985  4.9281 8.304e-07 ***
Sigma = Sqrt[(s^2(u)+s^2(v))] 0.4260212  0.0369471 11.5306 < 2.2e-16 ***
Gamma = sigma(u)^2/sigma^2    0.9229346  0.0378850 24.3615 < 2.2e-16 ***
Lambda = sigma(u)/sigma(v)    3.4606346  0.9216422  3.7549 0.0001734 ***
E[u]                          0.3265553          -       -         -    
E[exp(-u)]                    0.7419464          -       -         -    
-----[ Tests vs. No Inefficiency ]-----
Likelihood Ratio Test of Inefficiency
Deg. freedom for inefficiency model                        1 
Log Likelihood for OLS Log(H0) =                   -16.96836 
LR statistic: 
Chisq = 2*[LogL(H0)-LogL(H1)]  =                    17.89279 
Kodde-Palm C*:       95%: 2.70554               99%: 5.41189 
Coelli (1995) skewness test on OLS residuals
M3T: z                         =                    -4.12175 
M3T: p.value                   =                     0.00004 
Log likelihood status: successful convergence  

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
Model was estimated on : Feb Sat 28, 2026 at 12:16 
```
</details>

#### O'Donnell (2008) Two-Stage SFA Metafrontier

We now demonstrate the two-stage stochastic metafrontier using O'Donnell's approach. We'll use the `utility` dataset since it represents a cost frontier.

```r
# Utility data
data("utility", package="sfaR")
attach(utility)

# STANDARD O'DONNELL METAFRONTIER (O'DONNELL IN FIRST STAGE, O'DONNELL IN SECOND STAGE)
meta_sfa_ord <- sfametafrontier(
  formula = log(tc/wf) ~ log(y) + I(1/2 * (log(y))^2) +
    log(wl/wf) + log(wk/wf) + I(1/2 * (log(wl/wf))^2) + I(1/2 * (log(wk/wf))^2) +
    I(log(wl/wf) * log(wk/wf)) + I(log(y) * log(wl/wf)) + I(log(y) * log(wk/wf)),
  data        = utility,
  group       = "regu",
  S           = -1,
  udist       = "hnormal",
  method      = "bfgs",
  metaMethod  = "sfa",
  sfaApproach = "ordonnell"
)
summary(meta_sfa_ord)

```
<details>
  <summary>Toggle to see the output</summary>

```plaintext
============================================================ 
Stochastic Metafrontier Analysis
Metafrontier method: SFA Metafrontier [O'Donnell et al. (2008), envelope] 
Stochastic Cost Frontier, e = v + u 
SFA approach       : ordonnell 
Group approach     : Stochastic Frontier Analysis 
Group estimator    : sfacross 
Group optim solver : BFGS maximization 
Groups ( 2 ): 0, 1 
Total observations : 791 
Distribution       : hnormal 
============================================================ 

------------------------------------------------------------ 
Group: 0 (N = 297)  Log-likelihood: 93.51700
------------------------------------------------------------ 
                           Coefficient Std. Error  z value  Pr(>|z|)    
(Intercept)                   8.240163   5.312510   1.5511  0.120881    
log(y)                        0.096599   0.474845   0.2034  0.838797    
I(1/2 * (log(y))^2)           0.027257   0.029156   0.9349  0.349863    
log(wl/wf)                    0.145188   1.276851   0.1137  0.909469    
log(wk/wf)                    3.718734   1.875075   1.9832  0.047340 *  
I(1/2 * (log(wl/wf))^2)      -0.204980   0.272812  -0.7514  0.452435    
I(1/2 * (log(wk/wf))^2)       1.357539   0.419869   3.2332  0.001224 ** 
I(log(wl/wf) * log(wk/wf))    0.682293   0.258505   2.6394  0.008306 ** 
I(log(y) * log(wl/wf))        0.132804   0.057974   2.2908  0.021977 *  
I(log(y) * log(wk/wf))       -0.067516   0.069041  -0.9779  0.328118    
Zu_(Intercept)               -2.492469   0.127191 -19.5962 < 2.2e-16 ***
Zv_(Intercept)               -5.237420   0.314682 -16.6435 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Variance & Efficiency Statistics (delta-method SEs):
                               Estimate Std. Error z value  Pr(>|z|)    
Sigma-squared(u)              0.0827055  0.0105194  7.8622 3.776e-15 ***
Sigma(u)                      0.2875856  0.0182892 15.7243 < 2.2e-16 ***
Sigma-squared(v)              0.0053140  0.0016722  3.1778  0.001484 ** 
Sigma(v)                      0.0728968  0.0114697  6.3556 2.076e-10 ***
Sigma = Sqrt[(s^2(u)+s^2(v))] 0.2966807  0.0163612 18.1331 < 2.2e-16 ***
Gamma = sigma(u)^2/sigma^2    0.9396275  0.0226133 41.5521 < 2.2e-16 ***
Lambda = sigma(u)/sigma(v)    3.9451034  0.7863156  5.0172 5.243e-07 ***
E[u]                          0.2294601          -       -         -    
E[exp(-u)]                    0.8063278          -       -         -    
-----[ Tests vs. No Inefficiency ]-----
Likelihood Ratio Test of Inefficiency
Deg. freedom for inefficiency model                        1 
Log Likelihood for OLS Log(H0) =                    75.20899 
LR statistic: 
Chisq = 2*[LogL(H0)-LogL(H1)]  =                    36.61600 
Kodde-Palm C*:       95%: 2.70554               99%: 5.41189 
Coelli (1995) skewness test on OLS residuals
M3T: z                         =                     4.46212 
M3T: p.value                   =                     0.00001 
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Group: 1 (N = 494)  Log-likelihood: 32.75385
------------------------------------------------------------ 
                           Coefficient  Std. Error  z value  Pr(>|z|)    
(Intercept)                -4.15081752  5.18815873  -0.8001  0.423678    
log(y)                      0.65632462  0.25886437   2.5354  0.011232 *  
I(1/2 * (log(y))^2)         0.03751748  0.01336134   2.8079  0.004986 ** 
log(wl/wf)                  0.56672802  1.11790010   0.5070  0.612185    
log(wk/wf)                 -2.34603069  2.08163884  -1.1270  0.259738    
I(1/2 * (log(wl/wf))^2)     0.18641248  0.21526874   0.8660  0.386516    
I(1/2 * (log(wk/wf))^2)    -0.10257659  0.47894977  -0.2142  0.830415    
I(log(wl/wf) * log(wk/wf))  0.37772717  0.22224791   1.6996  0.089211 .  
I(log(y) * log(wl/wf))      0.00051386  0.03683784   0.0139  0.988870    
I(log(y) * log(wk/wf))      0.11485910  0.05676252   2.0235  0.043021 *  
Zu_(Intercept)             -1.70362647  0.08005716 -21.2801 < 2.2e-16 ***
Zv_(Intercept)             -6.75857426  0.68196866  -9.9104 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Variance & Efficiency Statistics (delta-method SEs):
                                Estimate Std. Error  z value  Pr(>|z|)    
Sigma-squared(u)              1.8202e-01 1.4572e-02  12.4911 < 2.2e-16 ***
Sigma(u)                      4.2664e-01 1.7078e-02  24.9821 < 2.2e-16 ***
Sigma-squared(v)              1.1609e-03 7.9169e-04   1.4663  0.142555    
Sigma(v)                      3.4072e-02 1.1618e-02   2.9327  0.003360 ** 
Sigma = Sqrt[(s^2(u)+s^2(v))] 4.2800e-01 1.6568e-02  25.8325 < 2.2e-16 ***
Gamma = sigma(u)^2/sigma^2    9.9366e-01 4.5736e-03 217.2625 < 2.2e-16 ***
Lambda = sigma(u)/sigma(v)    1.2522e+01 4.5473e+00   2.7537  0.005892 ** 
E[u]                          3.4041e-01          -        -         -    
E[exp(-u)]                    7.3345e-01          -        -         -    
-----[ Tests vs. No Inefficiency ]-----
Likelihood Ratio Test of Inefficiency
Deg. freedom for inefficiency model                        1 
Log Likelihood for OLS Log(H0) =                   -27.98178 
LR statistic: 
Chisq = 2*[LogL(H0)-LogL(H1)]  =                   121.47128 
Kodde-Palm C*:       95%: 2.70554               99%: 5.41189 
Coelli (1995) skewness test on OLS residuals
M3T: z                         =                     8.26338 
M3T: p.value                   =                     0.00000 
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Metafrontier Coefficients (sfa):
Meta-optim solver  : BFGS maximization 
                              Estimate  Std. Error   z value  Pr(>|z|)    
(Intercept)                -1.1207e+00  3.1852e-06 -351855.5 < 2.2e-16 ***
log(y)                      4.1348e-01  2.7660e-06  149490.6 < 2.2e-16 ***
I(1/2 * (log(y))^2)         4.5944e-02  1.8445e-06   24907.8 < 2.2e-16 ***
log(wl/wf)                  5.3514e-01  7.5543e-06   70839.4 < 2.2e-16 ***
log(wk/wf)                 -1.4732e+00  1.9577e-06 -752515.4 < 2.2e-16 ***
I(1/2 * (log(wl/wf))^2)     1.1181e-01  1.4181e-05    7884.1 < 2.2e-16 ***
I(1/2 * (log(wk/wf))^2)     1.0645e-01  2.7855e-05    3821.6 < 2.2e-16 ***
I(log(wl/wf) * log(wk/wf))  4.1672e-01  5.6902e-05    7323.5 < 2.2e-16 ***
I(log(y) * log(wl/wf))      1.9915e-02  1.0502e-05    1896.3 < 2.2e-16 ***
I(log(y) * log(wk/wf))      8.8138e-02  3.2855e-06   26826.4 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Meta-frontier Variance & Efficiency Statistics:
                                Estimate Std. Error    z value  Pr(>|z|)    
Sigma-squared(u)              1.6070e-03 3.7705e-16 4.2621e+12 < 2.2e-16 ***
Sigma(u)                      4.0088e-02 4.7028e-15 8.5242e+12 < 2.2e-16 ***
Sigma-squared(v)              9.1647e-15 1.4446e-26 6.3440e+11 < 2.2e-16 ***
Sigma(v)                      9.5733e-08 7.5451e-20 1.2688e+12 < 2.2e-16 ***
Sigma = Sqrt[(s^2(u)+s^2(v))] 4.0088e-02 4.7028e-15 8.5242e+12 < 2.2e-16 ***
Gamma = sigma(u)^2/sigma^2    1.0000e+00 7.6607e-24 1.3054e+23 < 2.2e-16 ***
Lambda = sigma(u)/sigma(v)    4.1875e+05 2.8125e-07 1.4889e+12 < 2.2e-16 ***
E[u]                          3.1985e-02          -          -         -    
E[exp(-u)]                    9.6880e-01          -          -         -    
-----[ Tests vs. No Inefficiency ]-----
Likelihood Ratio Test of Inefficiency
Deg. freedom for inefficiency model                        1 
Log Likelihood for OLS Log(H0) =                  1852.52952 
LR statistic: 
Chisq = 2*[LogL(H0)-LogL(H1)]  =                   235.52006 
Kodde-Palm C*:       95%: 2.70554               99%: 5.41189 
Coelli (1995) skewness test on OLS residuals
M3T: z                         =                    18.52160 
M3T: p.value                   =                     0.00000 
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Efficiency Statistics (group means):
------------------------------------------------------------ 
  N_obs N_valid TE_group_BC TE_meta_BC MTR_mean
0   297     297     0.80987    0.98117  1.24626
1   494     494     0.73677    0.97689  1.41747

Overall: TE_group=0.7733  TE_meta=0.9790  MTR=1.3319
------------------------------------------------------------ 
Total Log-likelihood: 2096.56 
AIC: -4121.121   BIC: -3952.882   HQIC: -4056.457 
------------------------------------------------------------ 
Model was estimated on : Feb Sat 28, 2026 at 12:17 
```
</details>

#### Huang (2014) Two-Stage SFA Metafrontier

Next, we apply Huang (2014)'s stochastic metafrontier framework, where the second stage relies on own-group fitted values rather than the enveloping boundaries.

```r
# STANDARD STOCHASTIC METAFRONTIER (HUANG IN FIRST STAGE, HUANG IN SECOND STAGE)
meta_huang <- sfametafrontier(
  formula = log(tc/wf) ~ log(y) + I(1/2 * (log(y))^2) +
    log(wl/wf) + log(wk/wf) + I(1/2 * (log(wl/wf))^2) + I(1/2 * (log(wk/wf))^2) +
    I(log(wl/wf) * log(wk/wf)) + I(log(y) * log(wl/wf)) + I(log(y) * log(wk/wf)),
  data        = utility,
  group       = "regu",
  S           = -1,
  udist       = "hnormal",
  method      = "bfgs",
  metaMethod  = "sfa",
  sfaApproach = "huang"
)
summary(meta_huang)

```
<details>
  <summary>Toggle to see the output</summary>

```plaintext
============================================================ 
Stochastic Metafrontier Analysis
Metafrontier method: SFA Metafrontier [Huang et al. (2014), two-stage] 
Stochastic Cost Frontier, e = v + u 
SFA approach       : huang 
Group approach     : Stochastic Frontier Analysis 
Group estimator    : sfacross 
Group optim solver : BFGS maximization 
Groups ( 2 ): 0, 1 
Total observations : 791 
Distribution       : hnormal 
============================================================ 

------------------------------------------------------------ 
Group: 0 (N = 297)  Log-likelihood: 93.51700
------------------------------------------------------------ 
                           Coefficient Std. Error  z value  Pr(>|z|)    
(Intercept)                   8.240163   5.312510   1.5511  0.120881    
log(y)                        0.096599   0.474845   0.2034  0.838797    
I(1/2 * (log(y))^2)           0.027257   0.029156   0.9349  0.349863    
log(wl/wf)                    0.145188   1.276851   0.1137  0.909469    
log(wk/wf)                    3.718734   1.875075   1.9832  0.047340 *  
I(1/2 * (log(wl/wf))^2)      -0.204980   0.272812  -0.7514  0.452435    
I(1/2 * (log(wk/wf))^2)       1.357539   0.419869   3.2332  0.001224 ** 
I(log(wl/wf) * log(wk/wf))    0.682293   0.258505   2.6394  0.008306 ** 
I(log(y) * log(wl/wf))        0.132804   0.057974   2.2908  0.021977 *  
I(log(y) * log(wk/wf))       -0.067516   0.069041  -0.9779  0.328118    
Zu_(Intercept)               -2.492469   0.127191 -19.5962 < 2.2e-16 ***
Zv_(Intercept)               -5.237420   0.314682 -16.6435 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Variance & Efficiency Statistics (delta-method SEs):
                               Estimate Std. Error z value  Pr(>|z|)    
Sigma-squared(u)              0.0827055  0.0105194  7.8622 3.776e-15 ***
Sigma(u)                      0.2875856  0.0182892 15.7243 < 2.2e-16 ***
Sigma-squared(v)              0.0053140  0.0016722  3.1778  0.001484 ** 
Sigma(v)                      0.0728968  0.0114697  6.3556 2.076e-10 ***
Sigma = Sqrt[(s^2(u)+s^2(v))] 0.2966807  0.0163612 18.1331 < 2.2e-16 ***
Gamma = sigma(u)^2/sigma^2    0.9396275  0.0226133 41.5521 < 2.2e-16 ***
Lambda = sigma(u)/sigma(v)    3.9451034  0.7863156  5.0172 5.243e-07 ***
E[u]                          0.2294601          -       -         -    
E[exp(-u)]                    0.8063278          -       -         -    
-----[ Tests vs. No Inefficiency ]-----
Likelihood Ratio Test of Inefficiency
Deg. freedom for inefficiency model                        1 
Log Likelihood for OLS Log(H0) =                    75.20899 
LR statistic: 
Chisq = 2*[LogL(H0)-LogL(H1)]  =                    36.61600 
Kodde-Palm C*:       95%: 2.70554               99%: 5.41189 
Coelli (1995) skewness test on OLS residuals
M3T: z                         =                     4.46212 
M3T: p.value                   =                     0.00001 
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Group: 1 (N = 494)  Log-likelihood: 32.75385
------------------------------------------------------------ 
                           Coefficient  Std. Error  z value  Pr(>|z|)    
(Intercept)                -4.15081752  5.18815873  -0.8001  0.423678    
log(y)                      0.65632462  0.25886437   2.5354  0.011232 *  
I(1/2 * (log(y))^2)         0.03751748  0.01336134   2.8079  0.004986 ** 
log(wl/wf)                  0.56672802  1.11790010   0.5070  0.612185    
log(wk/wf)                 -2.34603069  2.08163884  -1.1270  0.259738    
I(1/2 * (log(wl/wf))^2)     0.18641248  0.21526874   0.8660  0.386516    
I(1/2 * (log(wk/wf))^2)    -0.10257659  0.47894977  -0.2142  0.830415    
I(log(wl/wf) * log(wk/wf))  0.37772717  0.22224791   1.6996  0.089211 .  
I(log(y) * log(wl/wf))      0.00051386  0.03683784   0.0139  0.988870    
I(log(y) * log(wk/wf))      0.11485910  0.05676252   2.0235  0.043021 *  
Zu_(Intercept)             -1.70362647  0.08005716 -21.2801 < 2.2e-16 ***
Zv_(Intercept)             -6.75857426  0.68196866  -9.9104 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Variance & Efficiency Statistics (delta-method SEs):
                                Estimate Std. Error  z value  Pr(>|z|)    
Sigma-squared(u)              1.8202e-01 1.4572e-02  12.4911 < 2.2e-16 ***
Sigma(u)                      4.2664e-01 1.7078e-02  24.9821 < 2.2e-16 ***
Sigma-squared(v)              1.1609e-03 7.9169e-04   1.4663  0.142555    
Sigma(v)                      3.4072e-02 1.1618e-02   2.9327  0.003360 ** 
Sigma = Sqrt[(s^2(u)+s^2(v))] 4.2800e-01 1.6568e-02  25.8325 < 2.2e-16 ***
Gamma = sigma(u)^2/sigma^2    9.9366e-01 4.5736e-03 217.2625 < 2.2e-16 ***
Lambda = sigma(u)/sigma(v)    1.2522e+01 4.5473e+00   2.7537  0.005892 ** 
E[u]                          3.4041e-01          -        -         -    
E[exp(-u)]                    7.3345e-01          -        -         -    
-----[ Tests vs. No Inefficiency ]-----
Likelihood Ratio Test of Inefficiency
Deg. freedom for inefficiency model                        1 
Log Likelihood for OLS Log(H0) =                   -27.98178 
LR statistic: 
Chisq = 2*[LogL(H0)-LogL(H1)]  =                   121.47128 
Kodde-Palm C*:       95%: 2.70554               99%: 5.41189 
Coelli (1995) skewness test on OLS residuals
M3T: z                         =                     8.26338 
M3T: p.value                   =                     0.00000 
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Metafrontier Coefficients (sfa):
Meta-optim solver  : BFGS maximization 
                             Estimate Std. Error   z value  Pr(>|z|)    
(Intercept)                -1.7897199  0.0019706 -908.2068 < 2.2e-16 ***
log(y)                      0.5161981  0.0048397  106.6591 < 2.2e-16 ***
I(1/2 * (log(y))^2)         0.0341051  0.0025706   13.2672 < 2.2e-16 ***
log(wl/wf)                  1.1824021  0.0072591  162.8866 < 2.2e-16 ***
log(wk/wf)                 -0.6577364  0.0139389  -47.1872 < 2.2e-16 ***
I(1/2 * (log(wl/wf))^2)     0.0510267  0.0289357    1.7635  0.077824 .  
I(1/2 * (log(wk/wf))^2)     0.0791190  0.0530981    1.4901  0.136210    
I(log(wl/wf) * log(wk/wf))  0.4541855  0.0295361   15.3773 < 2.2e-16 ***
I(log(y) * log(wl/wf))     -0.0035069  0.0067084   -0.5228  0.601139    
I(log(y) * log(wk/wf))      0.0280731  0.0106059    2.6469  0.008123 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Meta-frontier Variance & Efficiency Statistics:
                                Estimate Std. Error    z value  Pr(>|z|)    
Sigma-squared(u)              2.0127e-07 2.5048e-13 8.0357e+05 < 2.2e-16 ***
Sigma(u)                      4.4864e-04 2.7915e-10 1.6071e+06 < 2.2e-16 ***
Sigma-squared(v)              3.0558e-03 1.5366e-04 1.9887e+01 < 2.2e-16 ***
Sigma(v)                      5.5280e-02 1.3899e-03 3.9773e+01 < 2.2e-16 ***
Sigma = Sqrt[(s^2(u)+s^2(v))] 5.5281e-02 1.3898e-03 3.9776e+01 < 2.2e-16 ***
Gamma = sigma(u)^2/sigma^2    6.5861e-05 3.3115e-06 1.9889e+01 < 2.2e-16 ***
Lambda = sigma(u)/sigma(v)    8.1158e-03 2.0404e-04 3.9774e+01 < 2.2e-16 ***
E[u]                          3.5796e-04          -          -         -    
E[exp(-u)]                    9.9964e-01          -          -         -    
-----[ Tests vs. No Inefficiency ]-----
Likelihood Ratio Test of Inefficiency
Deg. freedom for inefficiency model                        1 
Log Likelihood for OLS Log(H0) =                  1167.83124 
LR statistic: 
Chisq = 2*[LogL(H0)-LogL(H1)]  =                    -0.00004 
Kodde-Palm C*:       95%: 2.70554               99%: 5.41189 
Coelli (1995) skewness test on OLS residuals
M3T: z                         =                   -14.50576 
M3T: p.value                   =                     0.00000 
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Efficiency Statistics (group means):
------------------------------------------------------------ 
  N_obs N_valid TE_group_BC TE_meta_BC MTR_mean
0   297     297     0.80987    0.80958  0.99964
1   494     494     0.73677    0.73651  0.99964

Overall: TE_group=0.7733  TE_meta=0.7730  MTR=0.9996
------------------------------------------------------------ 
Total Log-likelihood: 1294.102 
AIC: -2516.204   BIC: -2347.965   HQIC: -2451.54 
------------------------------------------------------------ 
Model was estimated on : Feb Sat 28, 2026 at 12:17 
```
</details>

---

### 3. Latent Class Metafrontier (LCM) Models

Latent Class stochastic metafrontier models relax the assumption that firms are fully segmented into observed explicit groups. Here we estimate models assuming 2 latent classes.

#### LCM Group Frontiers + LP Metafrontier

```r
## LCM, LP METAFRONTIER (PARAMETRIC FIRST STAGE (DEFAULTS TO HUANG), LP (NON-PARAMETRIC) IN SECOND STAGE)
meta_lcm_lp <- sfametafrontier(
  formula = log(tc/wf) ~ log(y) + I(1/2 * (log(y))^2) +
    log(wl/wf) + log(wk/wf) + I(1/2 * (log(wl/wf))^2) + I(1/2 * (log(wk/wf))^2) +
    I(log(wl/wf) * log(wk/wf)) + I(log(y) * log(wl/wf)) + I(log(y) * log(wk/wf)),
  data        = utility,
  S           = -1,
  groupType   = "sfalcmcross",
  lcmClasses  = 2,
  whichStart  = 2,
  initAlg     = "nm",
  metaMethod  = "lp"
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
Group optim solver : BFGS maximization 
  (Pooled LCM - latent classes used as groups)
Groups ( 2 ): Class_1, Class_2 
Total observations : 791 
Distribution       : hnormal 
============================================================ 

------------------------------------------------------------ 
Pooled LCM (2 classes) on all data (N = 791)  Log-likelihood: 101.32815
------------------------------------------------------------ 

  -- Latent Class 1 --
  Frontier:
                           Coefficient  Std. Error  z value  Pr(>|z|)    
(Intercept)                 2.0372e+01  3.6085e-07 56456646 < 2.2e-16 ***
log(y)                     -1.6227e+00  1.4373e-06 -1128996 < 2.2e-16 ***
I(1/2 * (log(y))^2)         9.8412e-02  1.0163e-06    96833 < 2.2e-16 ***
log(wl/wf)                  5.2738e-01  7.2197e-06    73047 < 2.2e-16 ***
log(wk/wf)                  2.4214e+00  4.5346e-06   533978 < 2.2e-16 ***
I(1/2 * (log(wl/wf))^2)    -6.5635e-01  2.1916e-05   -29948 < 2.2e-16 ***
I(1/2 * (log(wk/wf))^2)     1.1625e+00  2.0697e-05    56167 < 2.2e-16 ***
I(log(wl/wf) * log(wk/wf))  1.0952e+00  1.2333e-05    88801 < 2.2e-16 ***
I(log(y) * log(wl/wf))      2.7348e-01  4.7381e-06    57719 < 2.2e-16 ***
I(log(y) * log(wk/wf))     -9.9557e-02  3.1705e-06   -31401 < 2.2e-16 ***
  Var(u):
               Coefficient  Std. Error    z value  Pr(>|z|)    
Zu_(Intercept) -2.1602e+00  2.3759e-14 -9.092e+13 < 2.2e-16 ***
  Var(v):
               Coefficient  Std. Error     z value  Pr(>|z|)    
Zv_(Intercept) -4.0858e+01  4.4841e-14 -9.1116e+14 < 2.2e-16 ***
  Sigma_u=0.3396  Sigma_v=0.0000  Sigma=0.3396  Gamma=1.0000  Lambda=252951557.4060

  -- Latent Class 2 --
  Frontier:
                           Coefficient  Std. Error     z value  Pr(>|z|)    
(Intercept)                 2.5036e+00  9.7926e-13  2.5566e+12 < 2.2e-16 ***
log(y)                      3.6180e-01  1.5692e-11  2.3057e+10 < 2.2e-16 ***
I(1/2 * (log(y))^2)         2.4432e-02  1.2604e-10  1.9383e+08 < 2.2e-16 ***
log(wl/wf)                  7.7960e-01  2.0394e-12  3.8228e+11 < 2.2e-16 ***
log(wk/wf)                  1.1749e+00  2.7966e-12  4.2013e+11 < 2.2e-16 ***
I(1/2 * (log(wl/wf))^2)     3.8058e-03  2.1381e-12  1.7800e+09 < 2.2e-16 ***
I(1/2 * (log(wk/wf))^2)     1.6375e-01  3.9907e-12  4.1033e+10 < 2.2e-16 ***
I(log(wl/wf) * log(wk/wf))  4.7765e-01  5.8288e-12  8.1947e+10 < 2.2e-16 ***
I(log(y) * log(wl/wf))      2.6844e-02  3.2521e-11  8.2542e+08 < 2.2e-16 ***
I(log(y) * log(wk/wf))     -6.9407e-02  4.4913e-11 -1.5454e+09 < 2.2e-16 ***
  Var(u):
               Coefficient  Std. Error     z value  Pr(>|z|)    
Zu_(Intercept) -4.4416e+00  2.6532e-14 -1.6741e+14 < 2.2e-16 ***
  Var(v):
               Coefficient  Std. Error     z value  Pr(>|z|)    
Zv_(Intercept) -4.6164e+00  2.6643e-14 -1.7327e+14 < 2.2e-16 ***
  Sigma_u=0.1085  Sigma_v=0.0994  Sigma=0.1472  Gamma=0.5436  Lambda=1.0913

  -- Class Membership (logit) --
                Coefficient  Std. Error     z value  Pr(>|z|)    
Cl1_(Intercept) -4.2121e-01  6.0669e-14 -6.9427e+12 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Metafrontier Coefficients (lp):
  (LP: deterministic envelope - no estimated parameters)

------------------------------------------------------------ 
Efficiency Statistics (group means):
------------------------------------------------------------ 
        N_obs N_valid TE_group_BC TE_meta_BC MTR_mean
Class_1   303     303     0.77828    0.77828  1.00000
Class_2   488     488     0.92338    0.92338  1.00000

Overall: TE_group=0.8508  TE_meta=0.8508  MTR=1.0000

------------------------------------------------------------ 
Posterior Class Membership (pooled LCM):
------------------------------------------------------------ 
        % assigned Mean post. prob.
Class 1       38.3            0.396
Class 2       61.7            0.604
------------------------------------------------------------ 
Total Log-likelihood: 101.3281 
AIC: -152.6563   BIC: -35.82386   HQIC: -107.7506 
------------------------------------------------------------ 
Model was estimated on : Feb Sat 28, 2026 at 12:17 
```
</details>

#### LCM Group Frontiers + QP Metafrontier

```r
## LCM, QP METAFRONTIER (PARAMETRIC FIRST STAGE (DEFAULTS TO HUANG), QP IN SECOND STAGE)
meta_lcm_qp <- sfametafrontier(
  formula = log(tc/wf) ~ log(y) + I(1/2 * (log(y))^2) +
    log(wl/wf) + log(wk/wf) + I(1/2 * (log(wl/wf))^2) + I(1/2 * (log(wk/wf))^2) +
    I(log(wl/wf) * log(wk/wf)) + I(log(y) * log(wl/wf)) + I(log(y) * log(wk/wf)),
  data        = utility,
  S           = -1,
  groupType   = "sfalcmcross",
  lcmClasses  = 2,
  whichStart  = 2,
  initAlg     = "nm",
  metaMethod  = "qp"
)
summary(meta_lcm_qp)
```
<details>
  <summary>Toggle to see the output</summary>

```plaintext
============================================================ 
Stochastic Metafrontier Analysis
Metafrontier method: Quadratic Programming (QP) Metafrontier 
Stochastic Cost Frontier, e = v + u 
Group approach     : Latent Class Stochastic Frontier Analysis 
Group estimator    : sfalcmcross 
Group optim solver : BFGS maximization 
  (Pooled LCM - latent classes used as groups)
Groups ( 2 ): Class_1, Class_2 
Total observations : 791 
Distribution       : hnormal 
============================================================ 

------------------------------------------------------------ 
Pooled LCM (2 classes) on all data (N = 791)  Log-likelihood: 101.32815
------------------------------------------------------------ 

  -- Latent Class 1 --
  Frontier:
                           Coefficient  Std. Error  z value  Pr(>|z|)    
(Intercept)                 2.0372e+01  3.6085e-07 56456646 < 2.2e-16 ***
log(y)                     -1.6227e+00  1.4373e-06 -1128996 < 2.2e-16 ***
I(1/2 * (log(y))^2)         9.8412e-02  1.0163e-06    96833 < 2.2e-16 ***
log(wl/wf)                  5.2738e-01  7.2197e-06    73047 < 2.2e-16 ***
log(wk/wf)                  2.4214e+00  4.5346e-06   533978 < 2.2e-16 ***
I(1/2 * (log(wl/wf))^2)    -6.5635e-01  2.1916e-05   -29948 < 2.2e-16 ***
I(1/2 * (log(wk/wf))^2)     1.1625e+00  2.0697e-05    56167 < 2.2e-16 ***
I(log(wl/wf) * log(wk/wf))  1.0952e+00  1.2333e-05    88801 < 2.2e-16 ***
I(log(y) * log(wl/wf))      2.7348e-01  4.7381e-06    57719 < 2.2e-16 ***
I(log(y) * log(wk/wf))     -9.9557e-02  3.1705e-06   -31401 < 2.2e-16 ***
  Var(u):
               Coefficient  Std. Error    z value  Pr(>|z|)    
Zu_(Intercept) -2.1602e+00  2.3759e-14 -9.092e+13 < 2.2e-16 ***
  Var(v):
               Coefficient  Std. Error     z value  Pr(>|z|)    
Zv_(Intercept) -4.0858e+01  4.4841e-14 -9.1116e+14 < 2.2e-16 ***
  Sigma_u=0.3396  Sigma_v=0.0000  Sigma=0.3396  Gamma=1.0000  Lambda=252951557.4060

  -- Latent Class 2 --
  Frontier:
                           Coefficient  Std. Error     z value  Pr(>|z|)    
(Intercept)                 2.5036e+00  9.7926e-13  2.5566e+12 < 2.2e-16 ***
log(y)                      3.6180e-01  1.5692e-11  2.3057e+10 < 2.2e-16 ***
I(1/2 * (log(y))^2)         2.4432e-02  1.2604e-10  1.9383e+08 < 2.2e-16 ***
log(wl/wf)                  7.7960e-01  2.0394e-12  3.8228e+11 < 2.2e-16 ***
log(wk/wf)                  1.1749e+00  2.7966e-12  4.2013e+11 < 2.2e-16 ***
I(1/2 * (log(wl/wf))^2)     3.8058e-03  2.1381e-12  1.7800e+09 < 2.2e-16 ***
I(1/2 * (log(wk/wf))^2)     1.6375e-01  3.9907e-12  4.1033e+10 < 2.2e-16 ***
I(log(wl/wf) * log(wk/wf))  4.7765e-01  5.8288e-12  8.1947e+10 < 2.2e-16 ***
I(log(y) * log(wl/wf))      2.6844e-02  3.2521e-11  8.2542e+08 < 2.2e-16 ***
I(log(y) * log(wk/wf))     -6.9407e-02  4.4913e-11 -1.5454e+09 < 2.2e-16 ***
  Var(u):
               Coefficient  Std. Error     z value  Pr(>|z|)    
Zu_(Intercept) -4.4416e+00  2.6532e-14 -1.6741e+14 < 2.2e-16 ***
  Var(v):
               Coefficient  Std. Error     z value  Pr(>|z|)    
Zv_(Intercept) -4.6164e+00  2.6643e-14 -1.7327e+14 < 2.2e-16 ***
  Sigma_u=0.1085  Sigma_v=0.0994  Sigma=0.1472  Gamma=0.5436  Lambda=1.0913

  -- Class Membership (logit) --
                Coefficient  Std. Error     z value  Pr(>|z|)    
Cl1_(Intercept) -4.2121e-01  6.0669e-14 -6.9427e+12 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Metafrontier Coefficients (qp):
                             Estimate Std. Error  z value  Pr(>|z|)    
(Intercept)                21.8693132  0.5283648  41.3906 < 2.2e-16 ***
log(y)                     -1.4146087  0.0341953 -41.3686 < 2.2e-16 ***
I(1/2 * (log(y))^2)         0.1078725  0.0017677  61.0227 < 2.2e-16 ***
log(wl/wf)                 -0.8044534  0.1348633  -5.9650 2.447e-09 ***
log(wk/wf)                  3.5025110  0.1944788  18.0097 < 2.2e-16 ***
I(1/2 * (log(wl/wf))^2)    -0.1028056  0.0253184  -4.0605 4.896e-05 ***
I(1/2 * (log(wk/wf))^2)     1.4492152  0.0472234  30.6885 < 2.2e-16 ***
I(log(wl/wf) * log(wk/wf))  0.6553995  0.0288038  22.7539 < 2.2e-16 ***
I(log(y) * log(wl/wf))      0.1917004  0.0042808  44.7809 < 2.2e-16 ***
I(log(y) * log(wk/wf))     -0.0470162  0.0062185  -7.5607 4.010e-14 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

------------------------------------------------------------ 
Efficiency Statistics (group means):
------------------------------------------------------------ 
        N_obs N_valid TE_group_BC TE_meta_BC MTR_mean
Class_1   303     303     0.77828    0.77364  0.99420
Class_2   488     488     0.92338    0.91975  0.99605

Overall: TE_group=0.8508  TE_meta=0.8467  MTR=0.9951

------------------------------------------------------------ 
Posterior Class Membership (pooled LCM):
------------------------------------------------------------ 
        % assigned Mean post. prob.
Class 1       38.3            0.396
Class 2       61.7            0.604
------------------------------------------------------------ 
Total Log-likelihood: 101.3281 
AIC: -132.6563   BIC: 30.90912   HQIC: -69.78831 
------------------------------------------------------------ 
Model was estimated on : Feb Sat 28, 2026 at 12:17 
```
</details>

#### LCM Group Frontiers + Huang Stochastic Metafrontier

```r
## LATENT CLASS STOCHASTIC METAFRONTIER (HUANG IN FIRST STAGE, HUANG IN SECOND STAGE)
meta_lcm_huang <- sfametafrontier(
  formula = log(tc/wf) ~ log(y) + I(1/2 * (log(y))^2) +
    log(wl/wf) + log(wk/wf) + I(1/2 * (log(wl/wf))^2) + I(1/2 * (log(wk/wf))^2) +
    I(log(wl/wf) * log(wk/wf)) + I(log(y) * log(wl/wf)) + I(log(y) * log(wk/wf)),
  data        = utility,
  S           = -1,
  groupType   = "sfalcmcross",
  lcmClasses  = 2,
  whichStart  = 2,
  method      = "bfgs",
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
Stochastic Cost Frontier, e = v + u 
SFA approach       : huang 
Group approach     : Latent Class Stochastic Frontier Analysis 
Group estimator    : sfalcmcross 
Group optim solver : BFGS maximization 
  (Pooled LCM - latent classes used as groups)
Groups ( 2 ): Class_1, Class_2 
Total observations : 791 
Distribution       : hnormal 
============================================================ 

------------------------------------------------------------ 
Pooled LCM (2 classes) on all data (N = 791)  Log-likelihood: 101.32815
------------------------------------------------------------ 

  -- Latent Class 1 --
  Frontier:
                           Coefficient  Std. Error  z value  Pr(>|z|)    
(Intercept)                 2.0372e+01  3.6085e-07 56456646 < 2.2e-16 ***
log(y)                     -1.6227e+00  1.4373e-06 -1128996 < 2.2e-16 ***
I(1/2 * (log(y))^2)         9.8412e-02  1.0163e-06    96833 < 2.2e-16 ***
log(wl/wf)                  5.2738e-01  7.2197e-06    73047 < 2.2e-16 ***
log(wk/wf)                  2.4214e+00  4.5346e-06   533978 < 2.2e-16 ***
I(1/2 * (log(wl/wf))^2)    -6.5635e-01  2.1916e-05   -29948 < 2.2e-16 ***
I(1/2 * (log(wk/wf))^2)     1.1625e+00  2.0697e-05    56167 < 2.2e-16 ***
I(log(wl/wf) * log(wk/wf))  1.0952e+00  1.2333e-05    88801 < 2.2e-16 ***
I(log(y) * log(wl/wf))      2.7348e-01  4.7381e-06    57719 < 2.2e-16 ***
I(log(y) * log(wk/wf))     -9.9557e-02  3.1705e-06   -31401 < 2.2e-16 ***
  Var(u):
               Coefficient  Std. Error    z value  Pr(>|z|)    
Zu_(Intercept) -2.1602e+00  2.3759e-14 -9.092e+13 < 2.2e-16 ***
  Var(v):
               Coefficient  Std. Error     z value  Pr(>|z|)    
Zv_(Intercept) -4.0858e+01  4.4841e-14 -9.1116e+14 < 2.2e-16 ***
  Sigma_u=0.3396  Sigma_v=0.0000  Sigma=0.3396  Gamma=1.0000  Lambda=252951557.4060

  -- Latent Class 2 --
  Frontier:
                           Coefficient  Std. Error     z value  Pr(>|z|)    
(Intercept)                 2.5036e+00  9.7926e-13  2.5566e+12 < 2.2e-16 ***
log(y)                      3.6180e-01  1.5692e-11  2.3057e+10 < 2.2e-16 ***
I(1/2 * (log(y))^2)         2.4432e-02  1.2604e-10  1.9383e+08 < 2.2e-16 ***
log(wl/wf)                  7.7960e-01  2.0394e-12  3.8228e+11 < 2.2e-16 ***
log(wk/wf)                  1.1749e+00  2.7966e-12  4.2013e+11 < 2.2e-16 ***
I(1/2 * (log(wl/wf))^2)     3.8058e-03  2.1381e-12  1.7800e+09 < 2.2e-16 ***
I(1/2 * (log(wk/wf))^2)     1.6375e-01  3.9907e-12  4.1033e+10 < 2.2e-16 ***
I(log(wl/wf) * log(wk/wf))  4.7765e-01  5.8288e-12  8.1947e+10 < 2.2e-16 ***
I(log(y) * log(wl/wf))      2.6844e-02  3.2521e-11  8.2542e+08 < 2.2e-16 ***
I(log(y) * log(wk/wf))     -6.9407e-02  4.4913e-11 -1.5454e+09 < 2.2e-16 ***
  Var(u):
               Coefficient  Std. Error     z value  Pr(>|z|)    
Zu_(Intercept) -4.4416e+00  2.6532e-14 -1.6741e+14 < 2.2e-16 ***
  Var(v):
               Coefficient  Std. Error     z value  Pr(>|z|)    
Zv_(Intercept) -4.6164e+00  2.6643e-14 -1.7327e+14 < 2.2e-16 ***
  Sigma_u=0.1085  Sigma_v=0.0994  Sigma=0.1472  Gamma=0.5436  Lambda=1.0913

  -- Class Membership (logit) --
                Coefficient  Std. Error     z value  Pr(>|z|)    
Cl1_(Intercept) -4.2121e-01  6.0669e-14 -6.9427e+12 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Metafrontier Coefficients (sfa):
Meta-optim solver  : BFGS maximization 
                              Estimate  Std. Error  z value  Pr(>|z|)    
(Intercept)                 9.9044e-01  3.4527e-06 286864.5 < 2.2e-16 ***
log(y)                      1.3705e-01  4.9599e-06  27632.0 < 2.2e-16 ***
I(1/2 * (log(y))^2)         8.8688e-03  1.3197e-06   6720.1 < 2.2e-16 ***
log(wl/wf)                  2.0283e+00  1.0538e-05 192468.9 < 2.2e-16 ***
log(wk/wf)                 -2.4311e-02  3.0197e-06  -8050.9 < 2.2e-16 ***
I(1/2 * (log(wl/wf))^2)    -4.9562e-01  1.7825e-05 -27805.0 < 2.2e-16 ***
I(1/2 * (log(wk/wf))^2)    -3.0495e-01  3.2359e-05  -9424.0 < 2.2e-16 ***
I(log(wl/wf) * log(wk/wf))  9.6412e-01  3.3913e-05  28429.4 < 2.2e-16 ***
I(log(y) * log(wl/wf))      1.1548e-01  5.3111e-06  21743.0 < 2.2e-16 ***
I(log(y) * log(wk/wf))     -1.5593e-01  5.2468e-06 -29718.5 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Meta-frontier Variance & Efficiency Statistics:
                                Estimate Std. Error    z value  Pr(>|z|)    
Sigma-squared(u)              3.2723e-02 1.8260e-14 1.7921e+12 < 2.2e-16 ***
Sigma(u)                      1.8090e-01 5.0470e-14 3.5842e+12 < 2.2e-16 ***
Sigma-squared(v)              4.4624e-16 5.2939e-28 8.4294e+11 < 2.2e-16 ***
Sigma(v)                      2.1124e-08 1.2530e-20 1.6859e+12 < 2.2e-16 ***
Sigma = Sqrt[(s^2(u)+s^2(v))] 1.8090e-01 5.0470e-14 3.5842e+12 < 2.2e-16 ***
Gamma = sigma(u)^2/sigma^2    1.0000e+00 2.3497e-26 4.2558e+25 < 2.2e-16 ***
Lambda = sigma(u)/sigma(v)    8.5633e+06 7.4278e-06 1.1529e+12 < 2.2e-16 ***
E[u]                          1.4433e-01          -          -         -    
E[exp(-u)]                    8.7058e-01          -          -         -    
-----[ Tests vs. No Inefficiency ]-----
Likelihood Ratio Test of Inefficiency
Deg. freedom for inefficiency model                        1 
Log Likelihood for OLS Log(H0) =                   533.70137 
LR statistic: 
Chisq = 2*[LogL(H0)-LogL(H1)]  =                   489.35221 
Kodde-Palm C*:       95%: 2.70554               99%: 5.41189 
Coelli (1995) skewness test on OLS residuals
M3T: z                         =                     3.81630 
M3T: p.value                   =                     0.00014 
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Efficiency Statistics (group means):
------------------------------------------------------------ 
        N_obs N_valid TE_group_BC TE_meta_BC MTR_mean
Class_1   303     303     0.77828    0.60776  0.78267
Class_2   488     488     0.92338    0.88849  0.96217

Overall: TE_group=0.8508  TE_meta=0.7481  MTR=0.8724

------------------------------------------------------------ 
Posterior Class Membership (pooled LCM):
------------------------------------------------------------ 
        % assigned Mean post. prob.
Class 1       38.3            0.396
Class 2       61.7            0.604
------------------------------------------------------------ 
Total Log-likelihood: 879.7056 
AIC: -1685.411   BIC: -1512.499   HQIC: -1618.951 
------------------------------------------------------------ 
Model was estimated on : Feb Sat 28, 2026 at 12:17 
```
</details>

#### LCM Group Frontiers + O'Donnell Stochastic Metafrontier

```r
## LATENT CLASS STOCHASTIC METAFRONTIER (O'DONNELL IN FIRST STAGE, O'DONNELL IN SECOND STAGE)
meta_lcm_ordonnell <- sfametafrontier(
  formula = log(tc/wf) ~ log(y) + I(1/2 * (log(y))^2) +
    log(wl/wf) + log(wk/wf) + I(1/2 * (log(wl/wf))^2) + I(1/2 * (log(wk/wf))^2) +
    I(log(wl/wf) * log(wk/wf)) + I(log(y) * log(wl/wf)) + I(log(y) * log(wk/wf)),
  data        = utility,
  S           = -1,
  groupType   = "sfalcmcross",
  lcmClasses  = 2,
  whichStart  = 2,
  method      = "bfgs",
  metaMethod  = "sfa",
  sfaApproach = "ordonnell"
)
summary(meta_lcm_ordonnell)
```
<details>
  <summary>Toggle to see the output</summary>

```plaintext
============================================================ 
Stochastic Metafrontier Analysis
Metafrontier method: SFA Metafrontier [O'Donnell et al. (2008), envelope] 
Stochastic Cost Frontier, e = v + u 
SFA approach       : ordonnell 
Group approach     : Latent Class Stochastic Frontier Analysis 
Group estimator    : sfalcmcross 
Group optim solver : BFGS maximization 
  (Pooled LCM - latent classes used as groups)
Groups ( 2 ): Class_1, Class_2 
Total observations : 791 
Distribution       : hnormal 
============================================================ 

------------------------------------------------------------ 
Pooled LCM (2 classes) on all data (N = 791)  Log-likelihood: 101.32815
------------------------------------------------------------ 

  -- Latent Class 1 --
  Frontier:
                           Coefficient  Std. Error  z value  Pr(>|z|)    
(Intercept)                 2.0372e+01  3.6085e-07 56456646 < 2.2e-16 ***
log(y)                     -1.6227e+00  1.4373e-06 -1128996 < 2.2e-16 ***
I(1/2 * (log(y))^2)         9.8412e-02  1.0163e-06    96833 < 2.2e-16 ***
log(wl/wf)                  5.2738e-01  7.2197e-06    73047 < 2.2e-16 ***
log(wk/wf)                  2.4214e+00  4.5346e-06   533978 < 2.2e-16 ***
I(1/2 * (log(wl/wf))^2)    -6.5635e-01  2.1916e-05   -29948 < 2.2e-16 ***
I(1/2 * (log(wk/wf))^2)     1.1625e+00  2.0697e-05    56167 < 2.2e-16 ***
I(log(wl/wf) * log(wk/wf))  1.0952e+00  1.2333e-05    88801 < 2.2e-16 ***
I(log(y) * log(wl/wf))      2.7348e-01  4.7381e-06    57719 < 2.2e-16 ***
I(log(y) * log(wk/wf))     -9.9557e-02  3.1705e-06   -31401 < 2.2e-16 ***
  Var(u):
               Coefficient  Std. Error    z value  Pr(>|z|)    
Zu_(Intercept) -2.1602e+00  2.3759e-14 -9.092e+13 < 2.2e-16 ***
  Var(v):
               Coefficient  Std. Error     z value  Pr(>|z|)    
Zv_(Intercept) -4.0858e+01  4.4841e-14 -9.1116e+14 < 2.2e-16 ***
  Sigma_u=0.3396  Sigma_v=0.0000  Sigma=0.3396  Gamma=1.0000  Lambda=252951557.4060

  -- Latent Class 2 --
  Frontier:
                           Coefficient  Std. Error     z value  Pr(>|z|)    
(Intercept)                 2.5036e+00  9.7926e-13  2.5566e+12 < 2.2e-16 ***
log(y)                      3.6180e-01  1.5692e-11  2.3057e+10 < 2.2e-16 ***
I(1/2 * (log(y))^2)         2.4432e-02  1.2604e-10  1.9383e+08 < 2.2e-16 ***
log(wl/wf)                  7.7960e-01  2.0394e-12  3.8228e+11 < 2.2e-16 ***
log(wk/wf)                  1.1749e+00  2.7966e-12  4.2013e+11 < 2.2e-16 ***
I(1/2 * (log(wl/wf))^2)     3.8058e-03  2.1381e-12  1.7800e+09 < 2.2e-16 ***
I(1/2 * (log(wk/wf))^2)     1.6375e-01  3.9907e-12  4.1033e+10 < 2.2e-16 ***
I(log(wl/wf) * log(wk/wf))  4.7765e-01  5.8288e-12  8.1947e+10 < 2.2e-16 ***
I(log(y) * log(wl/wf))      2.6844e-02  3.2521e-11  8.2542e+08 < 2.2e-16 ***
I(log(y) * log(wk/wf))     -6.9407e-02  4.4913e-11 -1.5454e+09 < 2.2e-16 ***
  Var(u):
               Coefficient  Std. Error     z value  Pr(>|z|)    
Zu_(Intercept) -4.4416e+00  2.6532e-14 -1.6741e+14 < 2.2e-16 ***
  Var(v):
               Coefficient  Std. Error     z value  Pr(>|z|)    
Zv_(Intercept) -4.6164e+00  2.6643e-14 -1.7327e+14 < 2.2e-16 ***
  Sigma_u=0.1085  Sigma_v=0.0994  Sigma=0.1472  Gamma=0.5436  Lambda=1.0913

  -- Class Membership (logit) --
                Coefficient  Std. Error     z value  Pr(>|z|)    
Cl1_(Intercept) -4.2121e-01  6.0669e-14 -6.9427e+12 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Metafrontier Coefficients (sfa):
Meta-optim solver  : BFGS maximization 
                              Estimate  Std. Error z value  Pr(>|z|)    
(Intercept)                 1.9614e+01  3.1346e-06 6257440 < 2.2e-16 ***
log(y)                     -1.5220e+00  1.7233e-06 -883190 < 2.2e-16 ***
I(1/2 * (log(y))^2)         9.5988e-02  2.1873e-06   43884 < 2.2e-16 ***
log(wl/wf)                  5.1411e-01  4.9434e-06  103999 < 2.2e-16 ***
log(wk/wf)                  2.4423e+00  7.1562e-06  341288 < 2.2e-16 ***
I(1/2 * (log(wl/wf))^2)    -6.0468e-01  3.0442e-05  -19864 < 2.2e-16 ***
I(1/2 * (log(wk/wf))^2)     1.1430e+00  1.7774e-05   64306 < 2.2e-16 ***
I(log(wl/wf) * log(wk/wf))  1.0391e+00  3.3805e-05   30738 < 2.2e-16 ***
I(log(y) * log(wl/wf))      2.5577e-01  8.2292e-06   31081 < 2.2e-16 ***
I(log(y) * log(wk/wf))     -9.4399e-02  4.9573e-06  -19042 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Meta-frontier Variance & Efficiency Statistics:
                                Estimate Std. Error    z value  Pr(>|z|)    
Sigma-squared(u)              1.9881e-03 1.9651e-15 1.0117e+12 < 2.2e-16 ***
Sigma(u)                      4.4588e-02 2.2036e-14 2.0234e+12 < 2.2e-16 ***
Sigma-squared(v)              1.7884e-15 3.3991e-27 5.2614e+11 < 2.2e-16 ***
Sigma(v)                      4.2289e-08 4.0188e-20 1.0523e+12 < 2.2e-16 ***
Sigma = Sqrt[(s^2(u)+s^2(v))] 4.4588e-02 2.2036e-14 2.0234e+12 < 2.2e-16 ***
Gamma = sigma(u)^2/sigma^2    1.0000e+00 2.5815e-24 3.8736e+23 < 2.2e-16 ***
Lambda = sigma(u)/sigma(v)    1.0544e+06 1.5128e-06 6.9696e+11 < 2.2e-16 ***
E[u]                          3.5576e-02          -          -         -    
E[exp(-u)]                    9.6539e-01          -          -         -    
-----[ Tests vs. No Inefficiency ]-----
Likelihood Ratio Test of Inefficiency
Deg. freedom for inefficiency model                        1 
Log Likelihood for OLS Log(H0) =                  1764.29432 
LR statistic: 
Chisq = 2*[LogL(H0)-LogL(H1)]  =                   243.65844 
Kodde-Palm C*:       95%: 2.70554               99%: 5.41189 
Coelli (1995) skewness test on OLS residuals
M3T: z                         =                    13.59090 
M3T: p.value                   =                     0.00000 
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Efficiency Statistics (group means):
------------------------------------------------------------ 
        N_obs N_valid TE_group_BC TE_meta_BC MTR_mean
Class_1   303     303     0.77828    0.98452  1.32452
Class_2   488     488     0.92338    0.97762  1.05950

Overall: TE_group=0.8508  TE_meta=0.9811  MTR=1.1920

------------------------------------------------------------ 
Posterior Class Membership (pooled LCM):
------------------------------------------------------------ 
        % assigned Mean post. prob.
Class 1       38.3            0.396
Class 2       61.7            0.604
------------------------------------------------------------ 
Total Log-likelihood: 1987.452 
AIC: -3900.903   BIC: -3727.991   HQIC: -3834.443 
------------------------------------------------------------ 
Model was estimated on : Feb Sat 28, 2026 at 12:17 
```
</details>

---

### 4. Sample Selection Correction Metafrontier Models

Sample selection correction models account for sample selection biases within groups before estimating the global metafrontier. Here we demonstrate using a balanced selection indicator.

```r
## Create a BALANCED selection indicator — later survey rounds (YEARDUM > 3)
ricephil$laterSurvey <- as.integer(ricephil$YEARDUM > 3)

## SAMPLE SELECTION METAFRONTIER (PARAMETRIC FIRST STAGE (DEFAULTS TO HUANG), LP IN SECOND STAGE)
meta_sel_lp <- sfametafrontier(
  formula    = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data       = ricephil,
  group      = "group",
  S          = 1,
  groupType  = "sfaselectioncross",
  selectionF = laterSurvey ~ EDYRS + AGE, # TWO-SIDED: LHS = selection dummy
  method     = "bfgs",
  lType      = "ghermite",
  Nsub       = 20,
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
Groups ( 3 ): small, medium, large 
Total observations : 344 
Distribution       : hnormal 
============================================================ 

------------------------------------------------------------ 
Group: small (N = 125)  Log-likelihood: -60.78829
------------------------------------------------------------ 
  Frontier equation:
            Coefficient  Std. Error     z value  Pr(>|z|)    
(Intercept) -1.0411e+00  9.5368e-18 -1.0916e+17 < 2.2e-16 ***
log(AREA)    3.7310e-01  0.0000e+00         Inf < 2.2e-16 ***
log(LABOR)   3.8093e-01  3.1779e-17  1.1987e+16 < 2.2e-16 ***
log(NPK)     2.0580e-01  4.8996e-17  4.2004e+15 < 2.2e-16 ***
  Var(u) parameters:
               Coefficient  Std. Error     z value  Pr(>|z|)    
Zu_(Intercept) -1.2373e+00  1.9030e-18 -6.5017e+17 < 2.2e-16 ***
  Var(v) parameters:
               Coefficient  Std. Error     z value  Pr(>|z|)    
Zv_(Intercept) -2.3846e+00  1.0919e-18 -2.1839e+18 < 2.2e-16 ***
  Selection bias parameter (rho):
    Coefficient Std. Error   z value  Pr(>|z|)    
rho  1.0000e+00 1.6096e-09 621259584 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Variance & Efficiency Statistics (delta-method SEs):
                                Estimate Std. Error    z value  Pr(>|z|)    
Sigma-squared(u)              2.9017e-01 5.5220e-19 5.2548e+17 < 2.2e-16 ***
Sigma(u)                      5.3868e-01 5.1255e-19 1.0510e+18 < 2.2e-16 ***
Sigma-squared(v)              9.2122e-02 1.0059e-19 9.1581e+17 < 2.2e-16 ***
Sigma(v)                      3.0352e-01 1.6571e-19 1.8316e+18 < 2.2e-16 ***
Sigma = Sqrt[(s^2(u)+s^2(v))] 6.1830e-01 5.2789e-19 1.1713e+18 < 2.2e-16 ***
Gamma = sigma(u)^2/sigma^2    7.5903e-01 1.4835e-19 5.1164e+18 < 2.2e-16 ***
Lambda = sigma(u)/sigma(v)    1.7748e+00 7.1975e-19 2.4658e+18 < 2.2e-16 ***
E[u]                          4.2980e-01          -          -         -    
E[exp(-u)]                    6.8225e-01          -          -         -    
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Group: medium (N = 104)  Log-likelihood: -35.08943
------------------------------------------------------------ 
  Frontier equation:
            Coefficient Std. Error z value Pr(>|z|)   
(Intercept)   -0.404972   0.628876 -0.6440 0.519600   
log(AREA)      0.580751   0.204902  2.8343 0.004593 **
log(LABOR)     0.186337   0.136722  1.3629 0.172918   
log(NPK)       0.237259   0.089289  2.6572 0.007879 **
  Var(u) parameters:
               Coefficient Std. Error z value  Pr(>|z|)    
Zu_(Intercept)    -1.81401    0.36213 -5.0093 5.464e-07 ***
  Var(v) parameters:
               Coefficient Std. Error z value  Pr(>|z|)    
Zv_(Intercept)    -4.01638    0.69542 -5.7755 7.674e-09 ***
  Selection bias parameter (rho):
    Coefficient Std. Error z value Pr(>|z|)
rho    -0.06544    1.67044 -0.0392   0.9688

  Variance & Efficiency Statistics (delta-method SEs):
                              Estimate Std. Error z value  Pr(>|z|)    
Sigma-squared(u)              0.162999   0.059027  2.7614  0.005755 ** 
Sigma(u)                      0.403731   0.073102  5.5228 3.336e-08 ***
Sigma-squared(v)              0.018018   0.012530  1.4380  0.150440    
Sigma(v)                      0.134231   0.046674  2.8760  0.004028 ** 
Sigma = Sqrt[(s^2(u)+s^2(v))] 0.425461   0.060124  7.0764 1.480e-12 ***
Gamma = sigma(u)^2/sigma^2    0.900462   0.087972 10.2358 < 2.2e-16 ***
Lambda = sigma(u)/sigma(v)    3.007723   1.476033  2.0377  0.041579 *  
E[u]                          0.322131          -       -         -    
E[exp(-u)]                    0.744695          -       -         -    
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Group: large (N = 115)  Log-likelihood: -38.90214
------------------------------------------------------------ 
  Frontier equation:
            Coefficient  Std. Error     z value  Pr(>|z|)    
(Intercept) -1.7866e+00  2.9487e-16 -6.0590e+15 < 2.2e-16 ***
log(AREA)    4.3860e-01  3.6941e-16  1.1873e+15 < 2.2e-16 ***
log(LABOR)   4.4722e-01  1.5795e-15  2.8314e+14 < 2.2e-16 ***
log(NPK)     2.7744e-01  1.7113e-15  1.6212e+14 < 2.2e-16 ***
  Var(u) parameters:
               Coefficient  Std. Error     z value  Pr(>|z|)    
Zu_(Intercept) -1.2509e+00  1.9470e-17 -6.4247e+16 < 2.2e-16 ***
  Var(v) parameters:
               Coefficient  Std. Error     z value  Pr(>|z|)    
Zv_(Intercept) -3.7721e+00  1.1346e-17 -3.3247e+17 < 2.2e-16 ***
  Selection bias parameter (rho):
    Coefficient Std. Error  z value  Pr(>|z|)    
rho  1.0000e+00 2.7044e-08 36976802 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Variance & Efficiency Statistics (delta-method SEs):
                                Estimate Std. Error    z value  Pr(>|z|)    
Sigma-squared(u)              2.8624e-01 5.5733e-18 5.1360e+16 < 2.2e-16 ***
Sigma(u)                      5.3502e-01 5.2085e-18 1.0272e+17 < 2.2e-16 ***
Sigma-squared(v)              2.3004e-02 2.6099e-19 8.8140e+16 < 2.2e-16 ***
Sigma(v)                      1.5167e-01 8.6039e-19 1.7628e+17 < 2.2e-16 ***
Sigma = Sqrt[(s^2(u)+s^2(v))] 5.5610e-01 5.2457e-18 1.0601e+17 < 2.2e-16 ***
Gamma = sigma(u)^2/sigma^2    9.2561e-01 5.5942e-19 1.6546e+18 < 2.2e-16 ***
Lambda = sigma(u)/sigma(v)    3.5275e+00 1.4330e-17 2.4616e+17 < 2.2e-16 ***
E[u]                          4.2688e-01          -          -         -    
E[exp(-u)]                    6.8383e-01          -          -         -    
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Metafrontier Coefficients (lp):
  (LP: deterministic envelope - no estimated parameters)

------------------------------------------------------------ 
Total Log-likelihood: -134.7799 
AIC: 311.5597   BIC: 392.2132   HQIC: 343.683 
------------------------------------------------------------ 
Model was estimated on : Feb Sat 28, 2026 at 12:17 
```
</details>

#### Sample Selection + QP Metafrontier

```r
meta_sel_qp <- sfametafrontier(
  formula    = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data       = ricephil,
  group      = "group",
  S          = 1,
  groupType  = "sfaselectioncross",
  selectionF = laterSurvey ~ EDYRS + AGE,
  method     = "bfgs",
  lType      = "ghermite",
  Nsub       = 20,
  metaMethod = "qp"
)
summary(meta_sel_qp)

```
<details>
  <summary>Toggle to see the output</summary>

```plaintext
============================================================ 
Stochastic Metafrontier Analysis
Metafrontier method: Quadratic Programming (QP) Metafrontier 
Stochastic Production/Profit Frontier, e = v - u 
Group approach     : Sample Selection Stochastic Frontier Analysis 
Group estimator    : sfaselectioncross 
Group optim solver : BFGS maximization 
Groups ( 3 ): small, medium, large 
Total observations : 344 
Distribution       : hnormal 
============================================================ 

------------------------------------------------------------ 
Group: small (N = 125)  Log-likelihood: -60.78829
------------------------------------------------------------ 
  Frontier equation:
            Coefficient  Std. Error     z value  Pr(>|z|)    
(Intercept) -1.0411e+00  9.5368e-18 -1.0916e+17 < 2.2e-16 ***
log(AREA)    3.7310e-01  0.0000e+00         Inf < 2.2e-16 ***
log(LABOR)   3.8093e-01  3.1779e-17  1.1987e+16 < 2.2e-16 ***
log(NPK)     2.0580e-01  4.8996e-17  4.2004e+15 < 2.2e-16 ***
  Var(u) parameters:
               Coefficient  Std. Error     z value  Pr(>|z|)    
Zu_(Intercept) -1.2373e+00  1.9030e-18 -6.5017e+17 < 2.2e-16 ***
  Var(v) parameters:
               Coefficient  Std. Error     z value  Pr(>|z|)    
Zv_(Intercept) -2.3846e+00  1.0919e-18 -2.1839e+18 < 2.2e-16 ***
  Selection bias parameter (rho):
    Coefficient Std. Error   z value  Pr(>|z|)    
rho  1.0000e+00 1.6096e-09 621259584 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Variance & Efficiency Statistics (delta-method SEs):
                                Estimate Std. Error    z value  Pr(>|z|)    
Sigma-squared(u)              2.9017e-01 5.5220e-19 5.2548e+17 < 2.2e-16 ***
Sigma(u)                      5.3868e-01 5.1255e-19 1.0510e+18 < 2.2e-16 ***
Sigma-squared(v)              9.2122e-02 1.0059e-19 9.1581e+17 < 2.2e-16 ***
Sigma(v)                      3.0352e-01 1.6571e-19 1.8316e+18 < 2.2e-16 ***
Sigma = Sqrt[(s^2(u)+s^2(v))] 6.1830e-01 5.2789e-19 1.1713e+18 < 2.2e-16 ***
Gamma = sigma(u)^2/sigma^2    7.5903e-01 1.4835e-19 5.1164e+18 < 2.2e-16 ***
Lambda = sigma(u)/sigma(v)    1.7748e+00 7.1975e-19 2.4658e+18 < 2.2e-16 ***
E[u]                          4.2980e-01          -          -         -    
E[exp(-u)]                    6.8225e-01          -          -         -    
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Group: medium (N = 104)  Log-likelihood: -35.08943
------------------------------------------------------------ 
  Frontier equation:
            Coefficient Std. Error z value Pr(>|z|)   
(Intercept)   -0.404972   0.628876 -0.6440 0.519600   
log(AREA)      0.580751   0.204902  2.8343 0.004593 **
log(LABOR)     0.186337   0.136722  1.3629 0.172918   
log(NPK)       0.237259   0.089289  2.6572 0.007879 **
  Var(u) parameters:
               Coefficient Std. Error z value  Pr(>|z|)    
Zu_(Intercept)    -1.81401    0.36213 -5.0093 5.464e-07 ***
  Var(v) parameters:
               Coefficient Std. Error z value  Pr(>|z|)    
Zv_(Intercept)    -4.01638    0.69542 -5.7755 7.674e-09 ***
  Selection bias parameter (rho):
    Coefficient Std. Error z value Pr(>|z|)
rho    -0.06544    1.67044 -0.0392   0.9688

  Variance & Efficiency Statistics (delta-method SEs):
                              Estimate Std. Error z value  Pr(>|z|)    
Sigma-squared(u)              0.162999   0.059027  2.7614  0.005755 ** 
Sigma(u)                      0.403731   0.073102  5.5228 3.336e-08 ***
Sigma-squared(v)              0.018018   0.012530  1.4380  0.150440    
Sigma(v)                      0.134231   0.046674  2.8760  0.004028 ** 
Sigma = Sqrt[(s^2(u)+s^2(v))] 0.425461   0.060124  7.0764 1.480e-12 ***
Gamma = sigma(u)^2/sigma^2    0.900462   0.087972 10.2358 < 2.2e-16 ***
Lambda = sigma(u)/sigma(v)    3.007723   1.476033  2.0377  0.041579 *  
E[u]                          0.322131          -       -         -    
E[exp(-u)]                    0.744695          -       -         -    
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Group: large (N = 115)  Log-likelihood: -38.90214
------------------------------------------------------------ 
  Frontier equation:
            Coefficient  Std. Error     z value  Pr(>|z|)    
(Intercept) -1.7866e+00  2.9487e-16 -6.0590e+15 < 2.2e-16 ***
log(AREA)    4.3860e-01  3.6941e-16  1.1873e+15 < 2.2e-16 ***
log(LABOR)   4.4722e-01  1.5795e-15  2.8314e+14 < 2.2e-16 ***
log(NPK)     2.7744e-01  1.7113e-15  1.6212e+14 < 2.2e-16 ***
  Var(u) parameters:
               Coefficient  Std. Error     z value  Pr(>|z|)    
Zu_(Intercept) -1.2509e+00  1.9470e-17 -6.4247e+16 < 2.2e-16 ***
  Var(v) parameters:
               Coefficient  Std. Error     z value  Pr(>|z|)    
Zv_(Intercept) -3.7721e+00  1.1346e-17 -3.3247e+17 < 2.2e-16 ***
  Selection bias parameter (rho):
    Coefficient Std. Error  z value  Pr(>|z|)    
rho  1.0000e+00 2.7044e-08 36976802 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Variance & Efficiency Statistics (delta-method SEs):
                                Estimate Std. Error    z value  Pr(>|z|)    
Sigma-squared(u)              2.8624e-01 5.5733e-18 5.1360e+16 < 2.2e-16 ***
Sigma(u)                      5.3502e-01 5.2085e-18 1.0272e+17 < 2.2e-16 ***
Sigma-squared(v)              2.3004e-02 2.6099e-19 8.8140e+16 < 2.2e-16 ***
Sigma(v)                      1.5167e-01 8.6039e-19 1.7628e+17 < 2.2e-16 ***
Sigma = Sqrt[(s^2(u)+s^2(v))] 5.5610e-01 5.2457e-18 1.0601e+17 < 2.2e-16 ***
Gamma = sigma(u)^2/sigma^2    9.2561e-01 5.5942e-19 1.6546e+18 < 2.2e-16 ***
Lambda = sigma(u)/sigma(v)    3.5275e+00 1.4330e-17 2.4616e+17 < 2.2e-16 ***
E[u]                          4.2688e-01          -          -         -    
E[exp(-u)]                    6.8383e-01          -          -         -    
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Metafrontier Coefficients (qp):
              Estimate Std. Error z value  Pr(>|z|)    
(Intercept) -0.7971075  0.0342211 -23.293 < 2.2e-16 ***
log(AREA)    0.4775786  0.0087076  54.846 < 2.2e-16 ***
log(LABOR)   0.2973584  0.0088723  33.515 < 2.2e-16 ***
log(NPK)     0.2341056  0.0051737  45.249 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

------------------------------------------------------------ 
Total Log-likelihood: -134.7799 
AIC: 319.5597   BIC: 415.5758   HQIC: 357.8018 
------------------------------------------------------------ 
Model was estimated on : Feb Sat 28, 2026 at 12:17 
```
</details>

#### Sample Selection + Huang Stochastic Metafrontier

```r
meta_sel_huang <- sfametafrontier(
  formula     = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data        = ricephil,
  group       = "group",
  S           = 1,
  groupType   = "sfaselectioncross",
  selectionF  = laterSurvey ~ EDYRS + AGE,
  method      = "bfgs",
  lType       = "ghermite",
  Nsub        = 20,
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
Stochastic Production/Profit Frontier, e = v - u 
SFA approach       : huang 
Group approach     : Sample Selection Stochastic Frontier Analysis 
Group estimator    : sfaselectioncross 
Group optim solver : BFGS maximization 
Groups ( 3 ): small, medium, large 
Total observations : 344 
Distribution       : hnormal 
============================================================ 

------------------------------------------------------------ 
Group: small (N = 125)  Log-likelihood: -60.78829
------------------------------------------------------------ 
  Frontier equation:
            Coefficient  Std. Error     z value  Pr(>|z|)    
(Intercept) -1.0411e+00  9.5368e-18 -1.0916e+17 < 2.2e-16 ***
log(AREA)    3.7310e-01  0.0000e+00         Inf < 2.2e-16 ***
log(LABOR)   3.8093e-01  3.1779e-17  1.1987e+16 < 2.2e-16 ***
log(NPK)     2.0580e-01  4.8996e-17  4.2004e+15 < 2.2e-16 ***
  Var(u) parameters:
               Coefficient  Std. Error     z value  Pr(>|z|)    
Zu_(Intercept) -1.2373e+00  1.9030e-18 -6.5017e+17 < 2.2e-16 ***
  Var(v) parameters:
               Coefficient  Std. Error     z value  Pr(>|z|)    
Zv_(Intercept) -2.3846e+00  1.0919e-18 -2.1839e+18 < 2.2e-16 ***
  Selection bias parameter (rho):
    Coefficient Std. Error   z value  Pr(>|z|)    
rho  1.0000e+00 1.6096e-09 621259584 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Variance & Efficiency Statistics (delta-method SEs):
                                Estimate Std. Error    z value  Pr(>|z|)    
Sigma-squared(u)              2.9017e-01 5.5220e-19 5.2548e+17 < 2.2e-16 ***
Sigma(u)                      5.3868e-01 5.1255e-19 1.0510e+18 < 2.2e-16 ***
Sigma-squared(v)              9.2122e-02 1.0059e-19 9.1581e+17 < 2.2e-16 ***
Sigma(v)                      3.0352e-01 1.6571e-19 1.8316e+18 < 2.2e-16 ***
Sigma = Sqrt[(s^2(u)+s^2(v))] 6.1830e-01 5.2789e-19 1.1713e+18 < 2.2e-16 ***
Gamma = sigma(u)^2/sigma^2    7.5903e-01 1.4835e-19 5.1164e+18 < 2.2e-16 ***
Lambda = sigma(u)/sigma(v)    1.7748e+00 7.1975e-19 2.4658e+18 < 2.2e-16 ***
E[u]                          4.2980e-01          -          -         -    
E[exp(-u)]                    6.8225e-01          -          -         -    
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Group: medium (N = 104)  Log-likelihood: -35.08943
------------------------------------------------------------ 
  Frontier equation:
            Coefficient Std. Error z value Pr(>|z|)   
(Intercept)   -0.404972   0.628876 -0.6440 0.519600   
log(AREA)      0.580751   0.204902  2.8343 0.004593 **
log(LABOR)     0.186337   0.136722  1.3629 0.172918   
log(NPK)       0.237259   0.089289  2.6572 0.007879 **
  Var(u) parameters:
               Coefficient Std. Error z value  Pr(>|z|)    
Zu_(Intercept)    -1.81401    0.36213 -5.0093 5.464e-07 ***
  Var(v) parameters:
               Coefficient Std. Error z value  Pr(>|z|)    
Zv_(Intercept)    -4.01638    0.69542 -5.7755 7.674e-09 ***
  Selection bias parameter (rho):
    Coefficient Std. Error z value Pr(>|z|)
rho    -0.06544    1.67044 -0.0392   0.9688

  Variance & Efficiency Statistics (delta-method SEs):
                              Estimate Std. Error z value  Pr(>|z|)    
Sigma-squared(u)              0.162999   0.059027  2.7614  0.005755 ** 
Sigma(u)                      0.403731   0.073102  5.5228 3.336e-08 ***
Sigma-squared(v)              0.018018   0.012530  1.4380  0.150440    
Sigma(v)                      0.134231   0.046674  2.8760  0.004028 ** 
Sigma = Sqrt[(s^2(u)+s^2(v))] 0.425461   0.060124  7.0764 1.480e-12 ***
Gamma = sigma(u)^2/sigma^2    0.900462   0.087972 10.2358 < 2.2e-16 ***
Lambda = sigma(u)/sigma(v)    3.007723   1.476033  2.0377  0.041579 *  
E[u]                          0.322131          -       -         -    
E[exp(-u)]                    0.744695          -       -         -    
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Group: large (N = 115)  Log-likelihood: -38.90214
------------------------------------------------------------ 
  Frontier equation:
            Coefficient  Std. Error     z value  Pr(>|z|)    
(Intercept) -1.7866e+00  2.9487e-16 -6.0590e+15 < 2.2e-16 ***
log(AREA)    4.3860e-01  3.6941e-16  1.1873e+15 < 2.2e-16 ***
log(LABOR)   4.4722e-01  1.5795e-15  2.8314e+14 < 2.2e-16 ***
log(NPK)     2.7744e-01  1.7113e-15  1.6212e+14 < 2.2e-16 ***
  Var(u) parameters:
               Coefficient  Std. Error     z value  Pr(>|z|)    
Zu_(Intercept) -1.2509e+00  1.9470e-17 -6.4247e+16 < 2.2e-16 ***
  Var(v) parameters:
               Coefficient  Std. Error     z value  Pr(>|z|)    
Zv_(Intercept) -3.7721e+00  1.1346e-17 -3.3247e+17 < 2.2e-16 ***
  Selection bias parameter (rho):
    Coefficient Std. Error  z value  Pr(>|z|)    
rho  1.0000e+00 2.7044e-08 36976802 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Variance & Efficiency Statistics (delta-method SEs):
                                Estimate Std. Error    z value  Pr(>|z|)    
Sigma-squared(u)              2.8624e-01 5.5733e-18 5.1360e+16 < 2.2e-16 ***
Sigma(u)                      5.3502e-01 5.2085e-18 1.0272e+17 < 2.2e-16 ***
Sigma-squared(v)              2.3004e-02 2.6099e-19 8.8140e+16 < 2.2e-16 ***
Sigma(v)                      1.5167e-01 8.6039e-19 1.7628e+17 < 2.2e-16 ***
Sigma = Sqrt[(s^2(u)+s^2(v))] 5.5610e-01 5.2457e-18 1.0601e+17 < 2.2e-16 ***
Gamma = sigma(u)^2/sigma^2    9.2561e-01 5.5942e-19 1.6546e+18 < 2.2e-16 ***
Lambda = sigma(u)/sigma(v)    3.5275e+00 1.4330e-17 2.4616e+17 < 2.2e-16 ***
E[u]                          4.2688e-01          -          -         -    
E[exp(-u)]                    6.8383e-01          -          -         -    
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Metafrontier Coefficients (sfa):
Meta-optim solver  : BFGS maximization 
              Estimate Std. Error z value  Pr(>|z|)    
(Intercept) -0.9614117  0.0824958 -11.654 < 2.2e-16 ***
log(AREA)    0.4414238  0.0112675  39.177 < 2.2e-16 ***
log(LABOR)   0.3321428  0.0114806  28.931 < 2.2e-16 ***
log(NPK)     0.2366345  0.0066946  35.347 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Meta-frontier Variance & Efficiency Statistics:
                                Estimate Std. Error z value Pr(>|z|)    
Sigma-squared(u)              5.6825e-07 1.3160e-04  0.0043   0.9966    
Sigma(u)                      7.5382e-04 8.7290e-02  0.0086   0.9931    
Sigma-squared(v)              2.0494e-03 2.0334e-04 10.0784   <2e-16 ***
Sigma(v)                      4.5270e-02 2.2459e-03 20.1569   <2e-16 ***
Sigma = Sqrt[(s^2(u)+s^2(v))] 4.5276e-02 2.3717e-03 19.0901   <2e-16 ***
Gamma = sigma(u)^2/sigma^2    2.7720e-04 6.4187e-02  0.0043   0.9966    
Lambda = sigma(u)/sigma(v)    1.6652e-02 1.9284e+00  0.0086   0.9931    
E[u]                          6.0146e-04          -       -        -    
E[exp(-u)]                    9.9940e-01          -       -        -    
-----[ Tests vs. No Inefficiency ]-----
Likelihood Ratio Test of Inefficiency
Deg. freedom for inefficiency model                        1 
Log Likelihood for OLS Log(H0) =                   360.36601 
LR statistic: 
Chisq = 2*[LogL(H0)-LogL(H1)]  =                    -0.00002 
Kodde-Palm C*:       95%: 2.70554               99%: 5.41189 
Coelli (1995) skewness test on OLS residuals
M3T: z                         =                     1.39308 
M3T: p.value                   =                     0.16359 
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Total Log-likelihood: 225.5862 
AIC: -397.1723   BIC: -293.475   HQIC: -355.8709 
------------------------------------------------------------ 
Model was estimated on : Feb Sat 28, 2026 at 12:17 
```
</details>

#### Sample Selection + O'Donnell Stochastic Metafrontier

```r
meta_sel_ordonnell <- sfametafrontier(
  formula     = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data        = ricephil,
  group       = "group",
  S           = 1,
  groupType   = "sfaselectioncross",
  selectionF  = laterSurvey ~ EDYRS + AGE,
  method      = "bfgs",
  lType       = "ghermite",
  Nsub        = 20,
  metaMethod  = "sfa",
  sfaApproach = "ordonnell"
)
summary(meta_sel_ordonnell)

```
<details>
  <summary>Toggle to see the output</summary>

```plaintext
============================================================ 
Stochastic Metafrontier Analysis
Metafrontier method: SFA Metafrontier [O'Donnell et al. (2008), envelope] 
Stochastic Production/Profit Frontier, e = v - u 
SFA approach       : ordonnell 
Group approach     : Sample Selection Stochastic Frontier Analysis 
Group estimator    : sfaselectioncross 
Group optim solver : BFGS maximization 
Groups ( 3 ): small, medium, large 
Total observations : 344 
Distribution       : hnormal 
============================================================ 

------------------------------------------------------------ 
Group: small (N = 125)  Log-likelihood: -60.78829
------------------------------------------------------------ 
  Frontier equation:
            Coefficient  Std. Error     z value  Pr(>|z|)    
(Intercept) -1.0411e+00  9.5368e-18 -1.0916e+17 < 2.2e-16 ***
log(AREA)    3.7310e-01  0.0000e+00         Inf < 2.2e-16 ***
log(LABOR)   3.8093e-01  3.1779e-17  1.1987e+16 < 2.2e-16 ***
log(NPK)     2.0580e-01  4.8996e-17  4.2004e+15 < 2.2e-16 ***
  Var(u) parameters:
               Coefficient  Std. Error     z value  Pr(>|z|)    
Zu_(Intercept) -1.2373e+00  1.9030e-18 -6.5017e+17 < 2.2e-16 ***
  Var(v) parameters:
               Coefficient  Std. Error     z value  Pr(>|z|)    
Zv_(Intercept) -2.3846e+00  1.0919e-18 -2.1839e+18 < 2.2e-16 ***
  Selection bias parameter (rho):
    Coefficient Std. Error   z value  Pr(>|z|)    
rho  1.0000e+00 1.6096e-09 621259584 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Variance & Efficiency Statistics (delta-method SEs):
                                Estimate Std. Error    z value  Pr(>|z|)    
Sigma-squared(u)              2.9017e-01 5.5220e-19 5.2548e+17 < 2.2e-16 ***
Sigma(u)                      5.3868e-01 5.1255e-19 1.0510e+18 < 2.2e-16 ***
Sigma-squared(v)              9.2122e-02 1.0059e-19 9.1581e+17 < 2.2e-16 ***
Sigma(v)                      3.0352e-01 1.6571e-19 1.8316e+18 < 2.2e-16 ***
Sigma = Sqrt[(s^2(u)+s^2(v))] 6.1830e-01 5.2789e-19 1.1713e+18 < 2.2e-16 ***
Gamma = sigma(u)^2/sigma^2    7.5903e-01 1.4835e-19 5.1164e+18 < 2.2e-16 ***
Lambda = sigma(u)/sigma(v)    1.7748e+00 7.1975e-19 2.4658e+18 < 2.2e-16 ***
E[u]                          4.2980e-01          -          -         -    
E[exp(-u)]                    6.8225e-01          -          -         -    
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Group: medium (N = 104)  Log-likelihood: -35.08943
------------------------------------------------------------ 
  Frontier equation:
            Coefficient Std. Error z value Pr(>|z|)   
(Intercept)   -0.404972   0.628876 -0.6440 0.519600   
log(AREA)      0.580751   0.204902  2.8343 0.004593 **
log(LABOR)     0.186337   0.136722  1.3629 0.172918   
log(NPK)       0.237259   0.089289  2.6572 0.007879 **
  Var(u) parameters:
               Coefficient Std. Error z value  Pr(>|z|)    
Zu_(Intercept)    -1.81401    0.36213 -5.0093 5.464e-07 ***
  Var(v) parameters:
               Coefficient Std. Error z value  Pr(>|z|)    
Zv_(Intercept)    -4.01638    0.69542 -5.7755 7.674e-09 ***
  Selection bias parameter (rho):
    Coefficient Std. Error z value Pr(>|z|)
rho    -0.06544    1.67044 -0.0392   0.9688

  Variance & Efficiency Statistics (delta-method SEs):
                              Estimate Std. Error z value  Pr(>|z|)    
Sigma-squared(u)              0.162999   0.059027  2.7614  0.005755 ** 
Sigma(u)                      0.403731   0.073102  5.5228 3.336e-08 ***
Sigma-squared(v)              0.018018   0.012530  1.4380  0.150440    
Sigma(v)                      0.134231   0.046674  2.8760  0.004028 ** 
Sigma = Sqrt[(s^2(u)+s^2(v))] 0.425461   0.060124  7.0764 1.480e-12 ***
Gamma = sigma(u)^2/sigma^2    0.900462   0.087972 10.2358 < 2.2e-16 ***
Lambda = sigma(u)/sigma(v)    3.007723   1.476033  2.0377  0.041579 *  
E[u]                          0.322131          -       -         -    
E[exp(-u)]                    0.744695          -       -         -    
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Group: large (N = 115)  Log-likelihood: -38.90214
------------------------------------------------------------ 
  Frontier equation:
            Coefficient  Std. Error     z value  Pr(>|z|)    
(Intercept) -1.7866e+00  2.9487e-16 -6.0590e+15 < 2.2e-16 ***
log(AREA)    4.3860e-01  3.6941e-16  1.1873e+15 < 2.2e-16 ***
log(LABOR)   4.4722e-01  1.5795e-15  2.8314e+14 < 2.2e-16 ***
log(NPK)     2.7744e-01  1.7113e-15  1.6212e+14 < 2.2e-16 ***
  Var(u) parameters:
               Coefficient  Std. Error     z value  Pr(>|z|)    
Zu_(Intercept) -1.2509e+00  1.9470e-17 -6.4247e+16 < 2.2e-16 ***
  Var(v) parameters:
               Coefficient  Std. Error     z value  Pr(>|z|)    
Zv_(Intercept) -3.7721e+00  1.1346e-17 -3.3247e+17 < 2.2e-16 ***
  Selection bias parameter (rho):
    Coefficient Std. Error  z value  Pr(>|z|)    
rho  1.0000e+00 2.7044e-08 36976802 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Variance & Efficiency Statistics (delta-method SEs):
                                Estimate Std. Error    z value  Pr(>|z|)    
Sigma-squared(u)              2.8624e-01 5.5733e-18 5.1360e+16 < 2.2e-16 ***
Sigma(u)                      5.3502e-01 5.2085e-18 1.0272e+17 < 2.2e-16 ***
Sigma-squared(v)              2.3004e-02 2.6099e-19 8.8140e+16 < 2.2e-16 ***
Sigma(v)                      1.5167e-01 8.6039e-19 1.7628e+17 < 2.2e-16 ***
Sigma = Sqrt[(s^2(u)+s^2(v))] 5.5610e-01 5.2457e-18 1.0601e+17 < 2.2e-16 ***
Gamma = sigma(u)^2/sigma^2    9.2561e-01 5.5942e-19 1.6546e+18 < 2.2e-16 ***
Lambda = sigma(u)/sigma(v)    3.5275e+00 1.4330e-17 2.4616e+17 < 2.2e-16 ***
E[u]                          4.2688e-01          -          -         -    
E[exp(-u)]                    6.8383e-01          -          -         -    
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Metafrontier Coefficients (sfa):
Meta-optim solver  : BFGS maximization 
              Estimate Std. Error z value  Pr(>|z|)    
(Intercept) -0.7968929  0.0485409 -16.417 < 2.2e-16 ***
log(AREA)    0.4775786  0.0086262  55.364 < 2.2e-16 ***
log(LABOR)   0.2973584  0.0087894  33.831 < 2.2e-16 ***
log(NPK)     0.2341056  0.0051253  45.676 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  Meta-frontier Variance & Efficiency Statistics:
                                Estimate Std. Error z value Pr(>|z|)    
Sigma-squared(u)              7.2371e-08 2.3431e-05  0.0031   0.9975    
Sigma(u)                      2.6902e-04 4.3548e-02  0.0062   0.9951    
Sigma-squared(v)              1.2013e-03 1.1617e-04 10.3406   <2e-16 ***
Sigma(v)                      3.4660e-02 1.6759e-03 20.6811   <2e-16 ***
Sigma = Sqrt[(s^2(u)+s^2(v))] 3.4661e-02 1.6853e-03 20.5659   <2e-16 ***
Gamma = sigma(u)^2/sigma^2    6.0241e-05 1.9503e-02  0.0031   0.9975    
Lambda = sigma(u)/sigma(v)    7.7617e-03 1.2565e+00  0.0062   0.9951    
E[u]                          2.1465e-04          -       -        -    
E[exp(-u)]                    9.9979e-01          -       -        -    
-----[ Tests vs. No Inefficiency ]-----
Likelihood Ratio Test of Inefficiency
Deg. freedom for inefficiency model                        1 
Log Likelihood for OLS Log(H0) =                   417.79514 
LR statistic: 
Chisq = 2*[LogL(H0)-LogL(H1)]  =                    -0.00001 
Kodde-Palm C*:       95%: 2.70554               99%: 5.41189 
Coelli (1995) skewness test on OLS residuals
M3T: z                         =                     6.96573 
M3T: p.value                   =                     0.00000 
Log likelihood status: successful convergence  

------------------------------------------------------------ 
Total Log-likelihood: 283.0153 
AIC: -512.0306   BIC: -408.3332   HQIC: -470.7292 
------------------------------------------------------------ 
Model was estimated on : Feb Sat 28, 2026 at 12:17 
```
</details>

## Authors
- **Sulman Olieko Owili** - Author and Creator 

This package is licensed under GPL (>= 3).
