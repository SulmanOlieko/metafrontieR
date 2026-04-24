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

library(smfa)
#> Loading required package: sfaR
#>            ****           *******  
#>           /**/           /**////** 
#>   ****** ******  ******  /**   /** 
#>  **//// ///**/  //////** /*******  
#> //*****   /**    ******* /**///**  
#>  /////**  /**   **////** /**  //** 
#>  ******   /**  //********/**   //**
#> //////    //    //////// //     //    version 1.0.1
#> 
#> * Please cite the 'sfaR' package as:
#>   Dakpo KH., Desjeux Y., Henningsen A., and Latruffe L. (2024). sfaR: Stochastic Frontier Analysis Using R. R package version 1.0.1.
#> 
#> See also: citation("sfaR")
#> 
#> * For any questions, suggestions, or comments on the 'sfaR' package, you can contact directly the authors or visit:  https://github.com/hdakpo/sfaR/issues
#>                         .d888         
#>                        d88P"          
#>                        888            
#> .d8888b  88888b.d88b.  888888 8888b.  
#> 88K      888 "888 "88b 888       "88b 
#>  Y8888b. 888  888  888 888   .d888888  
#>      X88 888  888  888 888   888  888 
#>  88888P' 888  888  888 888   "Y888888 
#>                           version 1.0.0
#> 
#> * Please cite the 'smfa' package as:
#> Owili, S. O. (2026). smfa: Stochastic Metafrontier Analysis. R package version 1.0.0.
#> 
#> See also: citation("smfa")
#> 
#> * For any questions, suggestions, or comments on the 'smfa' package, you can contact the authors directly or visit:
#>   https://github.com/SulmanOlieko/smfa/issues

N <- 500; set.seed(12345)
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
#> 
#>   0   1 
#> 237 263
#>    0    1
#> 1013  987
```

## Method 1: sfaselectioncross + LP Metafrontier

``` r

meta_sel_lp <- smfa(
  formula    = log(y) ~ log(x1) + log(x2),
  selectionF = d ~ z1 + z2,      # selection equation: d is the binary indicator
  data       = dat,
  group      = "group",
  S          = 1L,
  udist      = "hnormal",
  groupType  = "sfaselectioncross",
  modelType  = "greene10",        # Greene (2010) two-step ML correction
  lType      = "kronrod",         # integration method for the selection likelihood
  Nsub       = 20,               # number of sub-intervals for numerical integration
  uBound     = Inf,
  method     = "bfgs",
  itermax    = 2000,
  metaMethod = "lp"
)
#> First step probit model...
#> Second step Frontier model...
#> First step probit model...
#> Second step Frontier model...
summary(meta_sel_lp)
#> ============================================================ 
#> Stochastic Metafrontier Analysis
#> Metafrontier method: Linear Programming (LP) Metafrontier 
#> Stochastic Production/Profit Frontier, e = v - u 
#> Group approach     : Sample Selection Stochastic Frontier Analysis 
#> Group estimator    : sfaselectioncross 
#> Group optim solver : BFGS maximization 
#> Groups ( 2 ): 0, 1 
#> Total observations : 500 
#> Distribution       : hnormal 
#> ============================================================ 
#> 
#> ------------------------------------------------------------ 
#> Group: 0 (N = 252)  Log-likelihood: -225.33119
#> ------------------------------------------------------------ 
#> -------------------------------------------------------------------------------- 
#> Sample Selection Correction Stochastic Frontier Model 
#> Dependent Variable:                                                       log(y) 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                          67 
#> Log likelihood value:                                                 -225.33119 
#> Log likelihood gradient norm:                                        5.77769e-07 
#> Estimation based on:                             N =  131 of 252 obs. and K =  6 
#> Inf. Cr:                                           AIC  =  462.7 AIC/N  =  3.532 
#>                                                    BIC  =  479.9 BIC/N  =  3.663 
#>                                                    HQIC =  469.7 HQIC/N =  3.585 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.01981 
#>            Sigma(v)           =                                          0.01981 
#>            Sigma-squared(u)   =                                          2.94034 
#>            Sigma(u)           =                                          2.94034 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          1.72051 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.99331 
#> Lambda = sigma(u)/sigma(v)    =                                         12.18326 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.98180 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         1.36817 
#> Average efficiency E[exp(-ui)] =                                         0.37581 
#> -------------------------------------------------------------------------------- 
#> Stochastic Production/Profit Frontier, e = v - u 
#> Estimator is 2 step Maximum Likelihood 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> (Intercept)        1.36885    0.11843 11.5584 < 2.2e-16 ***
#> log(x1)            0.17123    0.06350  2.6963  0.007011 ** 
#> log(x2)            0.07768    0.05238  1.4829  0.138102    
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zu_(Intercept)     1.07853    0.10204  10.569 < 2.2e-16 ***
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value Pr(>|z|)    
#> Zv_(Intercept)     -3.9216     1.1265 -3.4813 0.000499 ***
#> -------------------------------------------------------------------------------- 
#>                             Selection bias parameter 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value Pr(>|z|)
#> rho                0.27413    1.03629  0.2645   0.7914
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 17:44 
#> Log likelihood status: successful convergence  
#> --------------------------------------------------------------------------------  
#> 
#> ------------------------------------------------------------ 
#> Group: 1 (N = 248)  Log-likelihood: -197.67108
#> ------------------------------------------------------------ 
#> -------------------------------------------------------------------------------- 
#> Sample Selection Correction Stochastic Frontier Model 
#> Dependent Variable:                                                       log(y) 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                          67 
#> Log likelihood value:                                                 -197.67108 
#> Log likelihood gradient norm:                                        6.69210e-06 
#> Estimation based on:                             N =  132 of 248 obs. and K =  6 
#> Inf. Cr:                                           AIC  =  407.3 AIC/N  =  3.086 
#>                                                    BIC  =  424.6 BIC/N  =  3.217 
#>                                                    HQIC =  414.4 HQIC/N =  3.139 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.03365 
#>            Sigma(v)           =                                          0.03365 
#>            Sigma-squared(u)   =                                          1.84490 
#>            Sigma(u)           =                                          1.84490 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          1.37060 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.98209 
#> Lambda = sigma(u)/sigma(v)    =                                          7.40483 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.95221 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         1.08374 
#> Average efficiency E[exp(-ui)] =                                         0.43864 
#> -------------------------------------------------------------------------------- 
#> Stochastic Production/Profit Frontier, e = v - u 
#> Estimator is 2 step Maximum Likelihood 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> (Intercept)        1.22609    0.09780 12.5368 < 2.2e-16 ***
#> log(x1)            0.14445    0.03802  3.7994  0.000145 ***
#> log(x2)            0.11056    0.03775  2.9290  0.003401 ** 
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zu_(Intercept)     0.61243    0.12841  4.7694 1.847e-06 ***
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zv_(Intercept)    -3.39184    0.69675 -4.8681 1.127e-06 ***
#> -------------------------------------------------------------------------------- 
#>                             Selection bias parameter 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value Pr(>|z|)
#> rho                0.78787    0.60643  1.2992   0.1939
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 17:44 
#> Log likelihood status: successful convergence  
#> --------------------------------------------------------------------------------  
#> 
#> ------------------------------------------------------------ 
#> Metafrontier Coefficients (lp):
#>   (LP: deterministic envelope - no estimated parameters)
#> 
#> ------------------------------------------------------------ 
#> Efficiency Statistics (group means):
#> ------------------------------------------------------------ 
#>   N_obs N_valid TE_group_BC TE_group_JLMS TE_meta_BC TE_meta_JLMS  MTR_BC
#> 0   252     131     0.39953       0.39621    0.39953      0.39621 1.00000
#> 1   248     132     0.43254       0.42840    0.37567      0.37207 0.86741
#>   MTR_JLMS
#> 0  1.00000
#> 1  0.86741
#> 
#> Overall:
#> TE_group_BC=0.4160  TE_group_JLMS=0.4123
#> TE_meta_BC=0.3876   TE_meta_JLMS=0.3841
#> MTR_BC=0.9337     MTR_JLMS=0.9337
#> ------------------------------------------------------------ 
#> Total Log-likelihood: -423.0023 
#> AIC: 870.0045   BIC: 920.5798   HQIC: 889.8502 
#> ------------------------------------------------------------ 
#> Model was estimated on : Apr Fri 24, 2026 at 17:44
```

> **Note:** The `selectionF` argument is compulsory for
> `groupType = "sfaselectioncross"`. The left-hand side must be the
> binary selection variable (`d`). At least one regressor in the
> selection equation should *not* appear in the main frontier formula
> (exclusion restriction for identification).

## Method 2: sfaselectioncross + QP Metafrontier

``` r

meta_sel_qp <- smfa(
  formula    = log(y) ~ log(x1) + log(x2),
  selectionF = d ~ z1 + z2,
  data       = dat,
  group      = "group",
  S          = 1L,
  udist      = "hnormal",
  groupType  = "sfaselectioncross",
  modelType  = "greene10",
  lType      = "kronrod",
  Nsub       = 20,
  uBound     = Inf,
  method     = "bfgs",
  itermax    = 2000,
  metaMethod = "qp"
)
#> First step probit model...
#> Second step Frontier model...
#> First step probit model...
#> Second step Frontier model...
summary(meta_sel_qp)
#> ============================================================ 
#> Stochastic Metafrontier Analysis
#> Metafrontier method: Quadratic Programming (QP) Metafrontier 
#> Stochastic Production/Profit Frontier, e = v - u 
#> Group approach     : Sample Selection Stochastic Frontier Analysis 
#> Group estimator    : sfaselectioncross 
#> Group optim solver : BFGS maximization 
#> Groups ( 2 ): 0, 1 
#> Total observations : 500 
#> Distribution       : hnormal 
#> ============================================================ 
#> 
#> ------------------------------------------------------------ 
#> Group: 0 (N = 252)  Log-likelihood: -225.33119
#> ------------------------------------------------------------ 
#> -------------------------------------------------------------------------------- 
#> Sample Selection Correction Stochastic Frontier Model 
#> Dependent Variable:                                                       log(y) 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                          67 
#> Log likelihood value:                                                 -225.33119 
#> Log likelihood gradient norm:                                        5.77769e-07 
#> Estimation based on:                             N =  131 of 252 obs. and K =  6 
#> Inf. Cr:                                           AIC  =  462.7 AIC/N  =  3.532 
#>                                                    BIC  =  479.9 BIC/N  =  3.663 
#>                                                    HQIC =  469.7 HQIC/N =  3.585 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.01981 
#>            Sigma(v)           =                                          0.01981 
#>            Sigma-squared(u)   =                                          2.94034 
#>            Sigma(u)           =                                          2.94034 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          1.72051 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.99331 
#> Lambda = sigma(u)/sigma(v)    =                                         12.18326 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.98180 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         1.36817 
#> Average efficiency E[exp(-ui)] =                                         0.37581 
#> -------------------------------------------------------------------------------- 
#> Stochastic Production/Profit Frontier, e = v - u 
#> Estimator is 2 step Maximum Likelihood 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> (Intercept)        1.36885    0.11843 11.5584 < 2.2e-16 ***
#> log(x1)            0.17123    0.06350  2.6963  0.007011 ** 
#> log(x2)            0.07768    0.05238  1.4829  0.138102    
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zu_(Intercept)     1.07853    0.10204  10.569 < 2.2e-16 ***
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value Pr(>|z|)    
#> Zv_(Intercept)     -3.9216     1.1265 -3.4813 0.000499 ***
#> -------------------------------------------------------------------------------- 
#>                             Selection bias parameter 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value Pr(>|z|)
#> rho                0.27413    1.03629  0.2645   0.7914
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 17:44 
#> Log likelihood status: successful convergence  
#> --------------------------------------------------------------------------------  
#> 
#> ------------------------------------------------------------ 
#> Group: 1 (N = 248)  Log-likelihood: -197.67108
#> ------------------------------------------------------------ 
#> -------------------------------------------------------------------------------- 
#> Sample Selection Correction Stochastic Frontier Model 
#> Dependent Variable:                                                       log(y) 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                          67 
#> Log likelihood value:                                                 -197.67108 
#> Log likelihood gradient norm:                                        6.69210e-06 
#> Estimation based on:                             N =  132 of 248 obs. and K =  6 
#> Inf. Cr:                                           AIC  =  407.3 AIC/N  =  3.086 
#>                                                    BIC  =  424.6 BIC/N  =  3.217 
#>                                                    HQIC =  414.4 HQIC/N =  3.139 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.03365 
#>            Sigma(v)           =                                          0.03365 
#>            Sigma-squared(u)   =                                          1.84490 
#>            Sigma(u)           =                                          1.84490 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          1.37060 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.98209 
#> Lambda = sigma(u)/sigma(v)    =                                          7.40483 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.95221 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         1.08374 
#> Average efficiency E[exp(-ui)] =                                         0.43864 
#> -------------------------------------------------------------------------------- 
#> Stochastic Production/Profit Frontier, e = v - u 
#> Estimator is 2 step Maximum Likelihood 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> (Intercept)        1.22609    0.09780 12.5368 < 2.2e-16 ***
#> log(x1)            0.14445    0.03802  3.7994  0.000145 ***
#> log(x2)            0.11056    0.03775  2.9290  0.003401 ** 
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zu_(Intercept)     0.61243    0.12841  4.7694 1.847e-06 ***
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zv_(Intercept)    -3.39184    0.69675 -4.8681 1.127e-06 ***
#> -------------------------------------------------------------------------------- 
#>                             Selection bias parameter 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value Pr(>|z|)
#> rho                0.78787    0.60643  1.2992   0.1939
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 17:44 
#> Log likelihood status: successful convergence  
#> --------------------------------------------------------------------------------  
#> 
#> ------------------------------------------------------------ 
#> Metafrontier Coefficients (qp):
#>               Estimate Std. Error z value  Pr(>|z|)    
#> (Intercept) 1.36736720 0.00042531 3215.00 < 2.2e-16 ***
#> log(x1)     0.16870720 0.00027176  620.79 < 2.2e-16 ***
#> log(x2)     0.07759335 0.00030299  256.09 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> ------------------------------------------------------------ 
#> Efficiency Statistics (group means):
#> ------------------------------------------------------------ 
#>   N_obs N_valid TE_group_BC TE_group_JLMS TE_meta_BC TE_meta_JLMS  MTR_BC
#> 0   252     131     0.39953       0.39621    0.39914      0.39582 0.99892
#> 1   248     132     0.43254       0.42840    0.37556      0.37196 0.86709
#>   MTR_JLMS
#> 0  0.99892
#> 1  0.86709
#> 
#> Overall:
#> TE_group_BC=0.4160  TE_group_JLMS=0.4123
#> TE_meta_BC=0.3874   TE_meta_JLMS=0.3839
#> MTR_BC=0.9330     MTR_JLMS=0.9330
#> ------------------------------------------------------------ 
#> Total Log-likelihood: -423.0023 
#> AIC: 876.0045   BIC: 939.2237   HQIC: 900.8116 
#> ------------------------------------------------------------ 
#> Model was estimated on : Apr Fri 24, 2026 at 17:44
```

## Method 3: sfaselectioncross + SFA (Huang)

``` r

meta_sel_huang <- smfa(
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
#> First step probit model...
#> Second step Frontier model...
#> First step probit model...
#> Second step Frontier model...
summary(meta_sel_huang)
#> ============================================================ 
#> Stochastic Metafrontier Analysis
#> Metafrontier method: SFA Metafrontier [Huang et al. (2014), two-stage] 
#> Stochastic Production/Profit Frontier, e = v - u 
#> SFA approach       : huang 
#> Group approach     : Sample Selection Stochastic Frontier Analysis 
#> Group estimator    : sfaselectioncross 
#> Group optim solver : BFGS maximization 
#> Groups ( 2 ): 0, 1 
#> Total observations : 500 
#> Distribution       : hnormal 
#> ============================================================ 
#> 
#> ------------------------------------------------------------ 
#> Group: 0 (N = 252)  Log-likelihood: -225.33119
#> ------------------------------------------------------------ 
#> -------------------------------------------------------------------------------- 
#> Sample Selection Correction Stochastic Frontier Model 
#> Dependent Variable:                                                       log(y) 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                          67 
#> Log likelihood value:                                                 -225.33119 
#> Log likelihood gradient norm:                                        5.77769e-07 
#> Estimation based on:                             N =  131 of 252 obs. and K =  6 
#> Inf. Cr:                                           AIC  =  462.7 AIC/N  =  3.532 
#>                                                    BIC  =  479.9 BIC/N  =  3.663 
#>                                                    HQIC =  469.7 HQIC/N =  3.585 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.01981 
#>            Sigma(v)           =                                          0.01981 
#>            Sigma-squared(u)   =                                          2.94034 
#>            Sigma(u)           =                                          2.94034 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          1.72051 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.99331 
#> Lambda = sigma(u)/sigma(v)    =                                         12.18326 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.98180 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         1.36817 
#> Average efficiency E[exp(-ui)] =                                         0.37581 
#> -------------------------------------------------------------------------------- 
#> Stochastic Production/Profit Frontier, e = v - u 
#> Estimator is 2 step Maximum Likelihood 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> (Intercept)        1.36885    0.11843 11.5584 < 2.2e-16 ***
#> log(x1)            0.17123    0.06350  2.6963  0.007011 ** 
#> log(x2)            0.07768    0.05238  1.4829  0.138102    
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zu_(Intercept)     1.07853    0.10204  10.569 < 2.2e-16 ***
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value Pr(>|z|)    
#> Zv_(Intercept)     -3.9216     1.1265 -3.4813 0.000499 ***
#> -------------------------------------------------------------------------------- 
#>                             Selection bias parameter 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value Pr(>|z|)
#> rho                0.27413    1.03629  0.2645   0.7914
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 17:44 
#> Log likelihood status: successful convergence  
#> --------------------------------------------------------------------------------  
#> 
#> ------------------------------------------------------------ 
#> Group: 1 (N = 248)  Log-likelihood: -197.67108
#> ------------------------------------------------------------ 
#> -------------------------------------------------------------------------------- 
#> Sample Selection Correction Stochastic Frontier Model 
#> Dependent Variable:                                                       log(y) 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                          67 
#> Log likelihood value:                                                 -197.67108 
#> Log likelihood gradient norm:                                        6.69210e-06 
#> Estimation based on:                             N =  132 of 248 obs. and K =  6 
#> Inf. Cr:                                           AIC  =  407.3 AIC/N  =  3.086 
#>                                                    BIC  =  424.6 BIC/N  =  3.217 
#>                                                    HQIC =  414.4 HQIC/N =  3.139 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.03365 
#>            Sigma(v)           =                                          0.03365 
#>            Sigma-squared(u)   =                                          1.84490 
#>            Sigma(u)           =                                          1.84490 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          1.37060 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.98209 
#> Lambda = sigma(u)/sigma(v)    =                                          7.40483 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.95221 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         1.08374 
#> Average efficiency E[exp(-ui)] =                                         0.43864 
#> -------------------------------------------------------------------------------- 
#> Stochastic Production/Profit Frontier, e = v - u 
#> Estimator is 2 step Maximum Likelihood 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> (Intercept)        1.22609    0.09780 12.5368 < 2.2e-16 ***
#> log(x1)            0.14445    0.03802  3.7994  0.000145 ***
#> log(x2)            0.11056    0.03775  2.9290  0.003401 ** 
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zu_(Intercept)     0.61243    0.12841  4.7694 1.847e-06 ***
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zv_(Intercept)    -3.39184    0.69675 -4.8681 1.127e-06 ***
#> -------------------------------------------------------------------------------- 
#>                             Selection bias parameter 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value Pr(>|z|)
#> rho                0.78787    0.60643  1.2992   0.1939
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 17:44 
#> Log likelihood status: successful convergence  
#> --------------------------------------------------------------------------------  
#> 
#> ------------------------------------------------------------ 
#> Metafrontier Coefficients (sfa):
#> Meta-optim solver  : BFGS maximization 
#>              Estimate Std. Error z value  Pr(>|z|)    
#> (Intercept) 1.2986412  0.2511714  5.1703 2.337e-07 ***
#> log(x1)     0.1557503  0.0038319 40.6454 < 2.2e-16 ***
#> log(x2)     0.0921452  0.0042742 21.5584 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#>   Meta-frontier model details:
#> -------------------------------------------------------------------------------- 
#> Normal-Half Normal SF Model 
#> Dependent Variable:                                          group_fitted_values 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                         477 
#> Log likelihood value:                                                  305.40441 
#> Log likelihood gradient norm:                                        1.80864e-03 
#> Estimation based on:                                         N =  263 and K =  5 
#> Inf. Cr:                                         AIC  =  -600.8 AIC/N  =  -2.284 
#>                                                  BIC  =  -582.9 BIC/N  =  -2.217 
#>                                                  HQIC =  -593.6 HQIC/N =  -2.257 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.00573 
#>            Sigma(v)           =                                          0.00573 
#>            Sigma-squared(u)   =                                          0.00002 
#>            Sigma(u)           =                                          0.00002 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          0.07584 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.00328 
#> Lambda = sigma(u)/sigma(v)    =                                          0.05741 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.00120 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         0.00347 
#> Average efficiency E[exp(-ui)] =                                         0.99654 
#> -------------------------------------------------------------------------------- 
#> Stochastic Production/Profit Frontier, e = v - u 
#> -----[ Tests vs. No Inefficiency ]-----
#> Likelihood Ratio Test of Inefficiency
#> Deg. freedom for inefficiency model                                            1 
#> Log Likelihood for OLS Log(H0) =                                       305.40442 
#> LR statistic:  
#> Chisq = 2*[LogL(H0)-LogL(H1)]  =                                        -0.00001 
#> Kodde-Palm C*:       95%: 2.70554                                   99%: 5.41189 
#> Coelli (1995) skewness test on OLS residuals
#> M3T: z                         =                                        -0.06529 
#> M3T: p.value                   =                                         0.94794 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> (Intercept)        1.29864    0.25117  5.1703 2.337e-07 ***
#> .X2                0.15575    0.00383 40.6454 < 2.2e-16 ***
#> .X3                0.09215    0.00427 21.5584 < 2.2e-16 ***
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value Pr(>|z|)
#> Zu_(Intercept)     -10.877    144.833 -0.0751   0.9401
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zv_(Intercept)    -5.16154    0.19408 -26.595 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 17:44 
#> Log likelihood status: successful convergence  
#> --------------------------------------------------------------------------------  
#> Log likelihood status: successful convergence  
#> 
#> ------------------------------------------------------------ 
#> Efficiency Statistics (group means):
#> ------------------------------------------------------------ 
#>   N_obs N_valid TE_group_BC TE_group_JLMS TE_meta_BC TE_meta_JLMS  MTR_BC
#> 0   252     131     0.39953       0.39621    0.39818      0.39487 0.99663
#> 1   248     132     0.43254       0.42840    0.43101      0.42688 0.99646
#>   MTR_JLMS
#> 0  0.99662
#> 1  0.99645
#> 
#> Overall:
#> TE_group_BC=0.4160  TE_group_JLMS=0.4123
#> TE_meta_BC=0.4146   TE_meta_JLMS=0.4109
#> MTR_BC=0.9965     MTR_JLMS=0.9965
#> ------------------------------------------------------------ 
#> Total Log-likelihood: -117.5979 
#> AIC: 269.1957   BIC: 340.8441   HQIC: 297.3104 
#> ------------------------------------------------------------ 
#> Model was estimated on : Apr Fri 24, 2026 at 17:44
```

## Method 4: sfaselectioncross + SFA (O’Donnell)

``` r

meta_sel_odonnell <- smfa(
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
#> First step probit model...
#> Second step Frontier model...
#> First step probit model...
#> Second step Frontier model...
#> Warning: The residuals of the OLS are right-skewed. This may indicate the absence of inefficiency or
#>   model misspecification or sample 'bad luck'
summary(meta_sel_odonnell)
#> Warning: 263 MTR value(s) > 1 detected in O'Donnell SFA approach. This
#> typically occurs when the second-stage SFA estimates near-zero inefficiency
#> (sigma_u -> 0), causing TE_meta ~= 1 and MTR = TE_meta/TE_group > 1. Consider
#> using metaMethod='lp' or sfaApproach='huang' instead.
#> ============================================================ 
#> Stochastic Metafrontier Analysis
#> Metafrontier method: SFA Metafrontier [O'Donnell et al. (2008), envelope] 
#> Stochastic Production/Profit Frontier, e = v - u 
#> SFA approach       : ordonnell 
#> Group approach     : Sample Selection Stochastic Frontier Analysis 
#> Group estimator    : sfaselectioncross 
#> Group optim solver : BFGS maximization 
#> Groups ( 2 ): 0, 1 
#> Total observations : 500 
#> Distribution       : hnormal 
#> ============================================================ 
#> 
#> ------------------------------------------------------------ 
#> Group: 0 (N = 252)  Log-likelihood: -225.33119
#> ------------------------------------------------------------ 
#> -------------------------------------------------------------------------------- 
#> Sample Selection Correction Stochastic Frontier Model 
#> Dependent Variable:                                                       log(y) 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                          67 
#> Log likelihood value:                                                 -225.33119 
#> Log likelihood gradient norm:                                        5.77769e-07 
#> Estimation based on:                             N =  131 of 252 obs. and K =  6 
#> Inf. Cr:                                           AIC  =  462.7 AIC/N  =  3.532 
#>                                                    BIC  =  479.9 BIC/N  =  3.663 
#>                                                    HQIC =  469.7 HQIC/N =  3.585 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.01981 
#>            Sigma(v)           =                                          0.01981 
#>            Sigma-squared(u)   =                                          2.94034 
#>            Sigma(u)           =                                          2.94034 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          1.72051 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.99331 
#> Lambda = sigma(u)/sigma(v)    =                                         12.18326 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.98180 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         1.36817 
#> Average efficiency E[exp(-ui)] =                                         0.37581 
#> -------------------------------------------------------------------------------- 
#> Stochastic Production/Profit Frontier, e = v - u 
#> Estimator is 2 step Maximum Likelihood 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> (Intercept)        1.36885    0.11843 11.5584 < 2.2e-16 ***
#> log(x1)            0.17123    0.06350  2.6963  0.007011 ** 
#> log(x2)            0.07768    0.05238  1.4829  0.138102    
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zu_(Intercept)     1.07853    0.10204  10.569 < 2.2e-16 ***
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value Pr(>|z|)    
#> Zv_(Intercept)     -3.9216     1.1265 -3.4813 0.000499 ***
#> -------------------------------------------------------------------------------- 
#>                             Selection bias parameter 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value Pr(>|z|)
#> rho                0.27413    1.03629  0.2645   0.7914
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 17:44 
#> Log likelihood status: successful convergence  
#> --------------------------------------------------------------------------------  
#> 
#> ------------------------------------------------------------ 
#> Group: 1 (N = 248)  Log-likelihood: -197.67108
#> ------------------------------------------------------------ 
#> -------------------------------------------------------------------------------- 
#> Sample Selection Correction Stochastic Frontier Model 
#> Dependent Variable:                                                       log(y) 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                          67 
#> Log likelihood value:                                                 -197.67108 
#> Log likelihood gradient norm:                                        6.69210e-06 
#> Estimation based on:                             N =  132 of 248 obs. and K =  6 
#> Inf. Cr:                                           AIC  =  407.3 AIC/N  =  3.086 
#>                                                    BIC  =  424.6 BIC/N  =  3.217 
#>                                                    HQIC =  414.4 HQIC/N =  3.139 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.03365 
#>            Sigma(v)           =                                          0.03365 
#>            Sigma-squared(u)   =                                          1.84490 
#>            Sigma(u)           =                                          1.84490 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          1.37060 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.98209 
#> Lambda = sigma(u)/sigma(v)    =                                          7.40483 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.95221 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         1.08374 
#> Average efficiency E[exp(-ui)] =                                         0.43864 
#> -------------------------------------------------------------------------------- 
#> Stochastic Production/Profit Frontier, e = v - u 
#> Estimator is 2 step Maximum Likelihood 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> (Intercept)        1.22609    0.09780 12.5368 < 2.2e-16 ***
#> log(x1)            0.14445    0.03802  3.7994  0.000145 ***
#> log(x2)            0.11056    0.03775  2.9290  0.003401 ** 
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zu_(Intercept)     0.61243    0.12841  4.7694 1.847e-06 ***
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zv_(Intercept)    -3.39184    0.69675 -4.8681 1.127e-06 ***
#> -------------------------------------------------------------------------------- 
#>                             Selection bias parameter 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value Pr(>|z|)
#> rho                0.78787    0.60643  1.2992   0.1939
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 17:44 
#> Log likelihood status: successful convergence  
#> --------------------------------------------------------------------------------  
#> 
#> ------------------------------------------------------------ 
#> Metafrontier Coefficients (sfa):
#> Meta-optim solver  : BFGS maximization 
#>               Estimate Std. Error z value  Pr(>|z|)    
#> (Intercept) 1.36739085 0.00198573  688.61 < 2.2e-16 ***
#> log(x1)     0.16870720 0.00027021  624.36 < 2.2e-16 ***
#> log(x2)     0.07759335 0.00030126  257.57 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#>   Meta-frontier model details:
#> -------------------------------------------------------------------------------- 
#> Normal-Half Normal SF Model 
#> Dependent Variable:                                                  lp_envelope 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                         329 
#> Log likelihood value:                                                 1002.79276 
#> Log likelihood gradient norm:                                        3.56461e-03 
#> Estimation based on:                                         N =  263 and K =  5 
#> Inf. Cr:                                        AIC  =  -1995.6 AIC/N  =  -7.588 
#>                                                 BIC  =  -1977.7 BIC/N  =  -7.520 
#>                                                 HQIC =  -1988.4 HQIC/N =  -7.560 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.00003 
#>            Sigma(v)           =                                          0.00003 
#>            Sigma-squared(u)   =                                          0.00000 
#>            Sigma(u)           =                                          0.00000 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          0.00534 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.00003 
#> Lambda = sigma(u)/sigma(v)    =                                          0.00555 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.00001 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         0.00002 
#> Average efficiency E[exp(-ui)] =                                         0.99998 
#> -------------------------------------------------------------------------------- 
#> Stochastic Production/Profit Frontier, e = v - u 
#> -----[ Tests vs. No Inefficiency ]-----
#> Likelihood Ratio Test of Inefficiency
#> Deg. freedom for inefficiency model                                            1 
#> Log Likelihood for OLS Log(H0) =                                      1002.79278 
#> LR statistic:  
#> Chisq = 2*[LogL(H0)-LogL(H1)]  =                                        -0.00003 
#> Kodde-Palm C*:       95%: 2.70554                                   99%: 5.41189 
#> Coelli (1995) skewness test on OLS residuals
#> M3T: z                         =                                        68.20600 
#> M3T: p.value                   =                                         0.00000 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> (Intercept)        1.36739    0.00199  688.61 < 2.2e-16 ***
#> .X2                0.16871    0.00027  624.36 < 2.2e-16 ***
#> .X3                0.07759    0.00030  257.57 < 2.2e-16 ***
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value Pr(>|z|)
#> Zu_(Intercept)     -20.852    164.029 -0.1271   0.8988
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zv_(Intercept)   -10.46369    0.08722 -119.97 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 17:44 
#> Log likelihood status: successful convergence  
#> --------------------------------------------------------------------------------  
#> Log likelihood status: successful convergence  
#> 
#> ------------------------------------------------------------ 
#> Efficiency Statistics (group means):
#> ------------------------------------------------------------ 
#>   N_obs N_valid TE_group_BC TE_group_JLMS TE_meta_BC TE_meta_JLMS   MTR_BC
#> 0   252     131     0.39953       0.39621    0.99998      0.99998 16.55549
#> 1   248     132     0.43254       0.42840    0.99998      0.99998  5.29814
#>   MTR_JLMS
#> 0 16.71157
#> 1  5.35380
#> 
#> Overall:
#> TE_group_BC=0.4160  TE_group_JLMS=0.4123
#> TE_meta_BC=1.0000   TE_meta_JLMS=1.0000
#> MTR_BC=10.9268     MTR_JLMS=11.0327
#> ------------------------------------------------------------ 
#> Total Log-likelihood: 579.7905 
#> AIC: -1125.581   BIC: -1053.933   HQIC: -1097.466 
#> ------------------------------------------------------------ 
#> Model was estimated on : Apr Fri 24, 2026 at 17:44
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
#> [1] 237

# Subset for selected observations in group 1
sel_grp1 <- eff_sel[eff_sel$group == 1 & !is.na(eff_sel$TE_group_BC), ]
summary(sel_grp1[, c("TE_group_BC", "TE_meta_BC", "MTR_BC")])
#>   TE_group_BC        TE_meta_BC          MTR_BC      
#>  Min.   :0.01365   Min.   :0.01189   Min.   :0.7502  
#>  1st Qu.:0.21013   1st Qu.:0.18598   1st Qu.:0.8450  
#>  Median :0.40784   Median :0.34783   Median :0.8686  
#>  Mean   :0.43254   Mean   :0.37567   Mean   :0.8674  
#>  3rd Qu.:0.64468   3rd Qu.:0.57565   3rd Qu.:0.8873  
#>  Max.   :0.93622   Max.   :0.86071   Max.   :1.0000
```
