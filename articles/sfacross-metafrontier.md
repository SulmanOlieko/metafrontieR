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
data("ricephil", package = "sfaR")

ricephil$group <- cut(
  ricephil$AREA,
  breaks        = quantile(ricephil$AREA, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
  labels        = c("small", "medium", "large"),
  include.lowest = TRUE
)

table(ricephil$group)
#> 
#>  small medium  large 
#>    125    104    115
#>  small medium  large
#>    125    104    115
```

## Method 1: LP Metafrontier

The **linear programming (LP)** envelope minimises the sum of absolute
deviations from group frontier predictions while satisfying a convexity
constraint. No stochastic parameters are estimated for the metafrontier
itself.

``` r
meta_lp <- smfa(
  formula    = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data       = ricephil,
  group      = "group",
  S          = 1,
  udist      = "hnormal",
  groupType  = "sfacross",
  metaMethod = "lp"
)
summary(meta_lp)
#> ============================================================ 
#> Stochastic Metafrontier Analysis
#> Metafrontier method: Linear Programming (LP) Metafrontier 
#> Stochastic Production/Profit Frontier, e = v - u 
#> Group approach     : Stochastic Frontier Analysis 
#> Group estimator    : sfacross 
#> Group optim solver : BFGS maximization 
#> Groups ( 3 ): small, medium, large 
#> Total observations : 344 
#> Distribution       : hnormal 
#> ============================================================ 
#> 
#> ------------------------------------------------------------ 
#> Group: small (N = 125)  Log-likelihood: -50.98578
#> ------------------------------------------------------------ 
#> -------------------------------------------------------------------------------- 
#> Normal-Half Normal SF Model 
#> Dependent Variable:                                                    log(PROD) 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                          42 
#> Log likelihood value:                                                  -50.98578 
#> Log likelihood gradient norm:                                        9.40653e-06 
#> Estimation based on:                                         N =  125 and K =  6 
#> Inf. Cr:                                           AIC  =  114.0 AIC/N  =  0.912 
#>                                                    BIC  =  130.9 BIC/N  =  1.048 
#>                                                    HQIC =  120.9 HQIC/N =  0.967 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.05318 
#>            Sigma(v)           =                                          0.05318 
#>            Sigma-squared(u)   =                                          0.23435 
#>            Sigma(u)           =                                          0.23435 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          0.53622 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.81504 
#> Lambda = sigma(u)/sigma(v)    =                                          2.09921 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.61558 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         0.38626 
#> Average efficiency E[exp(-ui)] =                                         0.70643 
#> -------------------------------------------------------------------------------- 
#> Stochastic Production/Profit Frontier, e = v - u 
#> -----[ Tests vs. No Inefficiency ]-----
#> Likelihood Ratio Test of Inefficiency
#> Deg. freedom for inefficiency model                                            1 
#> Log Likelihood for OLS Log(H0) =                                       -54.80277 
#> LR statistic:  
#> Chisq = 2*[LogL(H0)-LogL(H1)]  =                                         7.63398 
#> Kodde-Palm C*:       95%: 2.70554                                   99%: 5.41189 
#> Coelli (1995) skewness test on OLS residuals
#> M3T: z                         =                                        -3.57676 
#> M3T: p.value                   =                                         0.00035 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> (Intercept)       -1.58745    0.51274 -3.0960  0.001962 ** 
#> log(AREA)          0.24014    0.11834  2.0292  0.042440 *  
#> log(LABOR)         0.43464    0.12292  3.5361  0.000406 ***
#> log(NPK)           0.30516    0.05701  5.3523 8.682e-08 ***
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zu_(Intercept)    -1.45093    0.29867  -4.858 1.186e-06 ***
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zv_(Intercept)    -2.93406    0.35401  -8.288 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 14:08 
#> Log likelihood status: successful convergence  
#> --------------------------------------------------------------------------------  
#> 
#> ------------------------------------------------------------ 
#> Group: medium (N = 104)  Log-likelihood: -15.28164
#> ------------------------------------------------------------ 
#> -------------------------------------------------------------------------------- 
#> Normal-Half Normal SF Model 
#> Dependent Variable:                                                    log(PROD) 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                          41 
#> Log likelihood value:                                                  -15.28164 
#> Log likelihood gradient norm:                                        3.83566e-05 
#> Estimation based on:                                         N =  104 and K =  6 
#> Inf. Cr:                                            AIC  =  42.6 AIC/N  =  0.409 
#>                                                     BIC  =  58.4 BIC/N  =  0.562 
#>                                                     HQIC =  49.0 HQIC/N =  0.471 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.01058 
#>            Sigma(v)           =                                          0.01058 
#>            Sigma-squared(u)   =                                          0.22010 
#>            Sigma(u)           =                                          0.22010 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          0.48030 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.95412 
#> Lambda = sigma(u)/sigma(v)    =                                          4.56034 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.88314 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         0.37433 
#> Average efficiency E[exp(-ui)] =                                         0.71330 
#> -------------------------------------------------------------------------------- 
#> Stochastic Production/Profit Frontier, e = v - u 
#> -----[ Tests vs. No Inefficiency ]-----
#> Likelihood Ratio Test of Inefficiency
#> Deg. freedom for inefficiency model                                            1 
#> Log Likelihood for OLS Log(H0) =                                       -21.11323 
#> LR statistic:  
#> Chisq = 2*[LogL(H0)-LogL(H1)]  =                                        11.66318 
#> Kodde-Palm C*:       95%: 2.70554                                   99%: 5.41189 
#> Coelli (1995) skewness test on OLS residuals
#> M3T: z                         =                                        -2.91021 
#> M3T: p.value                   =                                         0.00361 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> (Intercept)       -0.08182    0.50668 -0.1615 0.8717190    
#> log(AREA)          0.47410    0.13984  3.3903 0.0006981 ***
#> log(LABOR)         0.17935    0.10201  1.7581 0.0787310 .  
#> log(NPK)           0.20255    0.08130  2.4913 0.0127289 *  
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zu_(Intercept)    -1.51367    0.23549 -6.4276 1.296e-10 ***
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zv_(Intercept)    -4.54846    0.76429 -5.9512 2.661e-09 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 14:08 
#> Log likelihood status: successful convergence  
#> --------------------------------------------------------------------------------  
#> 
#> ------------------------------------------------------------ 
#> Group: large (N = 115)  Log-likelihood: -8.02197
#> ------------------------------------------------------------ 
#> -------------------------------------------------------------------------------- 
#> Normal-Half Normal SF Model 
#> Dependent Variable:                                                    log(PROD) 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                          68 
#> Log likelihood value:                                                   -8.02197 
#> Log likelihood gradient norm:                                        4.01301e-05 
#> Estimation based on:                                         N =  115 and K =  6 
#> Inf. Cr:                                            AIC  =  28.0 AIC/N  =  0.244 
#>                                                     BIC  =  44.5 BIC/N  =  0.387 
#>                                                     HQIC =  34.7 HQIC/N =  0.302 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.01399 
#>            Sigma(v)           =                                          0.01399 
#>            Sigma-squared(u)   =                                          0.16751 
#>            Sigma(u)           =                                          0.16751 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          0.42602 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.92293 
#> Lambda = sigma(u)/sigma(v)    =                                          3.46063 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.81315 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         0.32656 
#> Average efficiency E[exp(-ui)] =                                         0.74195 
#> -------------------------------------------------------------------------------- 
#> Stochastic Production/Profit Frontier, e = v - u 
#> -----[ Tests vs. No Inefficiency ]-----
#> Likelihood Ratio Test of Inefficiency
#> Deg. freedom for inefficiency model                                            1 
#> Log Likelihood for OLS Log(H0) =                                       -16.96836 
#> LR statistic:  
#> Chisq = 2*[LogL(H0)-LogL(H1)]  =                                        17.89279 
#> Kodde-Palm C*:       95%: 2.70554                                   99%: 5.41189 
#> Coelli (1995) skewness test on OLS residuals
#> M3T: z                         =                                        -4.12175 
#> M3T: p.value                   =                                         0.00004 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> (Intercept)       -1.31194    0.41859 -3.1342 0.0017234 ** 
#> log(AREA)          0.38278    0.14297  2.6772 0.0074236 ** 
#> log(LABOR)         0.42105    0.10992  3.8303 0.0001280 ***
#> log(NPK)           0.23143    0.06065  3.8160 0.0001356 ***
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zu_(Intercept)    -1.78673    0.20176 -8.8555 < 2.2e-16 ***
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zv_(Intercept)    -4.26963    0.40584 -10.521 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 14:08 
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
#>        N_obs N_valid TE_group_BC TE_group_JLMS TE_meta_BC TE_meta_JLMS  MTR_BC
#> small    125     125     0.71065       0.70090    0.64126      0.63244 0.89981
#> medium   104     104     0.71253       0.70965    0.68204      0.67929 0.95597
#> large    115     115     0.74772       0.74406    0.72186      0.71834 0.96521
#>        MTR_JLMS
#> small   0.89981
#> medium  0.95597
#> large   0.96521
#> 
#> Overall:
#> TE_group_BC=0.7236  TE_group_JLMS=0.7182
#> TE_meta_BC=0.6817   TE_meta_JLMS=0.6767
#> MTR_BC=0.9403     MTR_JLMS=0.9403
#> ------------------------------------------------------------ 
#> Total Log-likelihood: -74.28939 
#> AIC: 184.5788   BIC: 253.7103   HQIC: 212.113 
#> ------------------------------------------------------------ 
#> Model was estimated on : Apr Fri 24, 2026 at 14:08
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
meta_qp <- smfa(
  formula    = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data       = ricephil,
  group      = "group",
  S          = 1,
  udist      = "hnormal",
  groupType  = "sfacross",
  metaMethod = "qp"
)
summary(meta_qp)
#> ============================================================ 
#> Stochastic Metafrontier Analysis
#> Metafrontier method: Quadratic Programming (QP) Metafrontier 
#> Stochastic Production/Profit Frontier, e = v - u 
#> Group approach     : Stochastic Frontier Analysis 
#> Group estimator    : sfacross 
#> Group optim solver : BFGS maximization 
#> Groups ( 3 ): small, medium, large 
#> Total observations : 344 
#> Distribution       : hnormal 
#> ============================================================ 
#> 
#> ------------------------------------------------------------ 
#> Group: small (N = 125)  Log-likelihood: -50.98578
#> ------------------------------------------------------------ 
#> -------------------------------------------------------------------------------- 
#> Normal-Half Normal SF Model 
#> Dependent Variable:                                                    log(PROD) 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                          42 
#> Log likelihood value:                                                  -50.98578 
#> Log likelihood gradient norm:                                        9.40653e-06 
#> Estimation based on:                                         N =  125 and K =  6 
#> Inf. Cr:                                           AIC  =  114.0 AIC/N  =  0.912 
#>                                                    BIC  =  130.9 BIC/N  =  1.048 
#>                                                    HQIC =  120.9 HQIC/N =  0.967 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.05318 
#>            Sigma(v)           =                                          0.05318 
#>            Sigma-squared(u)   =                                          0.23435 
#>            Sigma(u)           =                                          0.23435 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          0.53622 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.81504 
#> Lambda = sigma(u)/sigma(v)    =                                          2.09921 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.61558 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         0.38626 
#> Average efficiency E[exp(-ui)] =                                         0.70643 
#> -------------------------------------------------------------------------------- 
#> Stochastic Production/Profit Frontier, e = v - u 
#> -----[ Tests vs. No Inefficiency ]-----
#> Likelihood Ratio Test of Inefficiency
#> Deg. freedom for inefficiency model                                            1 
#> Log Likelihood for OLS Log(H0) =                                       -54.80277 
#> LR statistic:  
#> Chisq = 2*[LogL(H0)-LogL(H1)]  =                                         7.63398 
#> Kodde-Palm C*:       95%: 2.70554                                   99%: 5.41189 
#> Coelli (1995) skewness test on OLS residuals
#> M3T: z                         =                                        -3.57676 
#> M3T: p.value                   =                                         0.00035 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> (Intercept)       -1.58745    0.51274 -3.0960  0.001962 ** 
#> log(AREA)          0.24014    0.11834  2.0292  0.042440 *  
#> log(LABOR)         0.43464    0.12292  3.5361  0.000406 ***
#> log(NPK)           0.30516    0.05701  5.3523 8.682e-08 ***
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zu_(Intercept)    -1.45093    0.29867  -4.858 1.186e-06 ***
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zv_(Intercept)    -2.93406    0.35401  -8.288 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 14:08 
#> Log likelihood status: successful convergence  
#> --------------------------------------------------------------------------------  
#> 
#> ------------------------------------------------------------ 
#> Group: medium (N = 104)  Log-likelihood: -15.28164
#> ------------------------------------------------------------ 
#> -------------------------------------------------------------------------------- 
#> Normal-Half Normal SF Model 
#> Dependent Variable:                                                    log(PROD) 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                          41 
#> Log likelihood value:                                                  -15.28164 
#> Log likelihood gradient norm:                                        3.83566e-05 
#> Estimation based on:                                         N =  104 and K =  6 
#> Inf. Cr:                                            AIC  =  42.6 AIC/N  =  0.409 
#>                                                     BIC  =  58.4 BIC/N  =  0.562 
#>                                                     HQIC =  49.0 HQIC/N =  0.471 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.01058 
#>            Sigma(v)           =                                          0.01058 
#>            Sigma-squared(u)   =                                          0.22010 
#>            Sigma(u)           =                                          0.22010 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          0.48030 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.95412 
#> Lambda = sigma(u)/sigma(v)    =                                          4.56034 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.88314 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         0.37433 
#> Average efficiency E[exp(-ui)] =                                         0.71330 
#> -------------------------------------------------------------------------------- 
#> Stochastic Production/Profit Frontier, e = v - u 
#> -----[ Tests vs. No Inefficiency ]-----
#> Likelihood Ratio Test of Inefficiency
#> Deg. freedom for inefficiency model                                            1 
#> Log Likelihood for OLS Log(H0) =                                       -21.11323 
#> LR statistic:  
#> Chisq = 2*[LogL(H0)-LogL(H1)]  =                                        11.66318 
#> Kodde-Palm C*:       95%: 2.70554                                   99%: 5.41189 
#> Coelli (1995) skewness test on OLS residuals
#> M3T: z                         =                                        -2.91021 
#> M3T: p.value                   =                                         0.00361 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> (Intercept)       -0.08182    0.50668 -0.1615 0.8717190    
#> log(AREA)          0.47410    0.13984  3.3903 0.0006981 ***
#> log(LABOR)         0.17935    0.10201  1.7581 0.0787310 .  
#> log(NPK)           0.20255    0.08130  2.4913 0.0127289 *  
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zu_(Intercept)    -1.51367    0.23549 -6.4276 1.296e-10 ***
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zv_(Intercept)    -4.54846    0.76429 -5.9512 2.661e-09 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 14:08 
#> Log likelihood status: successful convergence  
#> --------------------------------------------------------------------------------  
#> 
#> ------------------------------------------------------------ 
#> Group: large (N = 115)  Log-likelihood: -8.02197
#> ------------------------------------------------------------ 
#> -------------------------------------------------------------------------------- 
#> Normal-Half Normal SF Model 
#> Dependent Variable:                                                    log(PROD) 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                          68 
#> Log likelihood value:                                                   -8.02197 
#> Log likelihood gradient norm:                                        4.01301e-05 
#> Estimation based on:                                         N =  115 and K =  6 
#> Inf. Cr:                                            AIC  =  28.0 AIC/N  =  0.244 
#>                                                     BIC  =  44.5 BIC/N  =  0.387 
#>                                                     HQIC =  34.7 HQIC/N =  0.302 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.01399 
#>            Sigma(v)           =                                          0.01399 
#>            Sigma-squared(u)   =                                          0.16751 
#>            Sigma(u)           =                                          0.16751 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          0.42602 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.92293 
#> Lambda = sigma(u)/sigma(v)    =                                          3.46063 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.81315 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         0.32656 
#> Average efficiency E[exp(-ui)] =                                         0.74195 
#> -------------------------------------------------------------------------------- 
#> Stochastic Production/Profit Frontier, e = v - u 
#> -----[ Tests vs. No Inefficiency ]-----
#> Likelihood Ratio Test of Inefficiency
#> Deg. freedom for inefficiency model                                            1 
#> Log Likelihood for OLS Log(H0) =                                       -16.96836 
#> LR statistic:  
#> Chisq = 2*[LogL(H0)-LogL(H1)]  =                                        17.89279 
#> Kodde-Palm C*:       95%: 2.70554                                   99%: 5.41189 
#> Coelli (1995) skewness test on OLS residuals
#> M3T: z                         =                                        -4.12175 
#> M3T: p.value                   =                                         0.00004 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> (Intercept)       -1.31194    0.41859 -3.1342 0.0017234 ** 
#> log(AREA)          0.38278    0.14297  2.6772 0.0074236 ** 
#> log(LABOR)         0.42105    0.10992  3.8303 0.0001280 ***
#> log(NPK)           0.23143    0.06065  3.8160 0.0001356 ***
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zu_(Intercept)    -1.78673    0.20176 -8.8555 < 2.2e-16 ***
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zv_(Intercept)    -4.26963    0.40584 -10.521 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 14:08 
#> Log likelihood status: successful convergence  
#> --------------------------------------------------------------------------------  
#> 
#> ------------------------------------------------------------ 
#> Metafrontier Coefficients (qp):
#>               Estimate Std. Error z value  Pr(>|z|)    
#> (Intercept) -0.6117795  0.0291793 -20.966 < 2.2e-16 ***
#> log(AREA)    0.3937843  0.0073209  53.789 < 2.2e-16 ***
#> log(LABOR)   0.2791273  0.0077215  36.150 < 2.2e-16 ***
#> log(NPK)     0.2409454  0.0046846  51.434 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> ------------------------------------------------------------ 
#> Efficiency Statistics (group means):
#> ------------------------------------------------------------ 
#>        N_obs N_valid TE_group_BC TE_group_JLMS TE_meta_BC TE_meta_JLMS  MTR_BC
#> small    125     125     0.71065       0.70090    0.64037      0.63156 0.89972
#> medium   104     104     0.71253       0.70965    0.66998      0.66727 0.94053
#> large    115     115     0.74772       0.74406    0.72290      0.71937 0.96676
#>        MTR_JLMS
#> small   0.89972
#> medium  0.94053
#> large   0.96676
#> 
#> Overall:
#> TE_group_BC=0.7236  TE_group_JLMS=0.7182
#> TE_meta_BC=0.6777   TE_meta_JLMS=0.6727
#> MTR_BC=0.9357     MTR_JLMS=0.9357
#> ------------------------------------------------------------ 
#> Total Log-likelihood: -74.28939 
#> AIC: 192.5788   BIC: 277.0729   HQIC: 226.2318 
#> ------------------------------------------------------------ 
#> Model was estimated on : Apr Fri 24, 2026 at 14:08
```

## Method 3: Stochastic Metafrontier — Huang et al. (2014)

The **two-stage stochastic metafrontier** of Huang, Huang & Liu (2014)
uses the group-specific *fitted frontier values* as the dependent
variable in a second-stage pooled SFA. The technology gap U and noise V
are estimated stochastically, which naturally bounds the metatechnology
ratio MTR ∈ (0, 1\].

``` r
meta_huang <- smfa(
  formula     = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data        = ricephil,
  group       = "group",
  S           = 1,
  udist       = "hnormal",
  groupType   = "sfacross",
  metaMethod  = "sfa",
  sfaApproach = "huang"
)
#> Warning: The residuals of the OLS are right-skewed. This may indicate the absence of inefficiency or
#>   model misspecification or sample 'bad luck'
summary(meta_huang)
#> ============================================================ 
#> Stochastic Metafrontier Analysis
#> Metafrontier method: SFA Metafrontier [Huang et al. (2014), two-stage] 
#> Stochastic Production/Profit Frontier, e = v - u 
#> SFA approach       : huang 
#> Group approach     : Stochastic Frontier Analysis 
#> Group estimator    : sfacross 
#> Group optim solver : BFGS maximization 
#> Groups ( 3 ): small, medium, large 
#> Total observations : 344 
#> Distribution       : hnormal 
#> ============================================================ 
#> 
#> ------------------------------------------------------------ 
#> Group: small (N = 125)  Log-likelihood: -50.98578
#> ------------------------------------------------------------ 
#> -------------------------------------------------------------------------------- 
#> Normal-Half Normal SF Model 
#> Dependent Variable:                                                    log(PROD) 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                          42 
#> Log likelihood value:                                                  -50.98578 
#> Log likelihood gradient norm:                                        9.40653e-06 
#> Estimation based on:                                         N =  125 and K =  6 
#> Inf. Cr:                                           AIC  =  114.0 AIC/N  =  0.912 
#>                                                    BIC  =  130.9 BIC/N  =  1.048 
#>                                                    HQIC =  120.9 HQIC/N =  0.967 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.05318 
#>            Sigma(v)           =                                          0.05318 
#>            Sigma-squared(u)   =                                          0.23435 
#>            Sigma(u)           =                                          0.23435 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          0.53622 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.81504 
#> Lambda = sigma(u)/sigma(v)    =                                          2.09921 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.61558 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         0.38626 
#> Average efficiency E[exp(-ui)] =                                         0.70643 
#> -------------------------------------------------------------------------------- 
#> Stochastic Production/Profit Frontier, e = v - u 
#> -----[ Tests vs. No Inefficiency ]-----
#> Likelihood Ratio Test of Inefficiency
#> Deg. freedom for inefficiency model                                            1 
#> Log Likelihood for OLS Log(H0) =                                       -54.80277 
#> LR statistic:  
#> Chisq = 2*[LogL(H0)-LogL(H1)]  =                                         7.63398 
#> Kodde-Palm C*:       95%: 2.70554                                   99%: 5.41189 
#> Coelli (1995) skewness test on OLS residuals
#> M3T: z                         =                                        -3.57676 
#> M3T: p.value                   =                                         0.00035 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> (Intercept)       -1.58745    0.51274 -3.0960  0.001962 ** 
#> log(AREA)          0.24014    0.11834  2.0292  0.042440 *  
#> log(LABOR)         0.43464    0.12292  3.5361  0.000406 ***
#> log(NPK)           0.30516    0.05701  5.3523 8.682e-08 ***
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zu_(Intercept)    -1.45093    0.29867  -4.858 1.186e-06 ***
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zv_(Intercept)    -2.93406    0.35401  -8.288 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 14:08 
#> Log likelihood status: successful convergence  
#> --------------------------------------------------------------------------------  
#> 
#> ------------------------------------------------------------ 
#> Group: medium (N = 104)  Log-likelihood: -15.28164
#> ------------------------------------------------------------ 
#> -------------------------------------------------------------------------------- 
#> Normal-Half Normal SF Model 
#> Dependent Variable:                                                    log(PROD) 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                          41 
#> Log likelihood value:                                                  -15.28164 
#> Log likelihood gradient norm:                                        3.83566e-05 
#> Estimation based on:                                         N =  104 and K =  6 
#> Inf. Cr:                                            AIC  =  42.6 AIC/N  =  0.409 
#>                                                     BIC  =  58.4 BIC/N  =  0.562 
#>                                                     HQIC =  49.0 HQIC/N =  0.471 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.01058 
#>            Sigma(v)           =                                          0.01058 
#>            Sigma-squared(u)   =                                          0.22010 
#>            Sigma(u)           =                                          0.22010 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          0.48030 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.95412 
#> Lambda = sigma(u)/sigma(v)    =                                          4.56034 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.88314 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         0.37433 
#> Average efficiency E[exp(-ui)] =                                         0.71330 
#> -------------------------------------------------------------------------------- 
#> Stochastic Production/Profit Frontier, e = v - u 
#> -----[ Tests vs. No Inefficiency ]-----
#> Likelihood Ratio Test of Inefficiency
#> Deg. freedom for inefficiency model                                            1 
#> Log Likelihood for OLS Log(H0) =                                       -21.11323 
#> LR statistic:  
#> Chisq = 2*[LogL(H0)-LogL(H1)]  =                                        11.66318 
#> Kodde-Palm C*:       95%: 2.70554                                   99%: 5.41189 
#> Coelli (1995) skewness test on OLS residuals
#> M3T: z                         =                                        -2.91021 
#> M3T: p.value                   =                                         0.00361 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> (Intercept)       -0.08182    0.50668 -0.1615 0.8717190    
#> log(AREA)          0.47410    0.13984  3.3903 0.0006981 ***
#> log(LABOR)         0.17935    0.10201  1.7581 0.0787310 .  
#> log(NPK)           0.20255    0.08130  2.4913 0.0127289 *  
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zu_(Intercept)    -1.51367    0.23549 -6.4276 1.296e-10 ***
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zv_(Intercept)    -4.54846    0.76429 -5.9512 2.661e-09 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 14:08 
#> Log likelihood status: successful convergence  
#> --------------------------------------------------------------------------------  
#> 
#> ------------------------------------------------------------ 
#> Group: large (N = 115)  Log-likelihood: -8.02197
#> ------------------------------------------------------------ 
#> -------------------------------------------------------------------------------- 
#> Normal-Half Normal SF Model 
#> Dependent Variable:                                                    log(PROD) 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                          68 
#> Log likelihood value:                                                   -8.02197 
#> Log likelihood gradient norm:                                        4.01301e-05 
#> Estimation based on:                                         N =  115 and K =  6 
#> Inf. Cr:                                            AIC  =  28.0 AIC/N  =  0.244 
#>                                                     BIC  =  44.5 BIC/N  =  0.387 
#>                                                     HQIC =  34.7 HQIC/N =  0.302 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.01399 
#>            Sigma(v)           =                                          0.01399 
#>            Sigma-squared(u)   =                                          0.16751 
#>            Sigma(u)           =                                          0.16751 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          0.42602 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.92293 
#> Lambda = sigma(u)/sigma(v)    =                                          3.46063 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.81315 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         0.32656 
#> Average efficiency E[exp(-ui)] =                                         0.74195 
#> -------------------------------------------------------------------------------- 
#> Stochastic Production/Profit Frontier, e = v - u 
#> -----[ Tests vs. No Inefficiency ]-----
#> Likelihood Ratio Test of Inefficiency
#> Deg. freedom for inefficiency model                                            1 
#> Log Likelihood for OLS Log(H0) =                                       -16.96836 
#> LR statistic:  
#> Chisq = 2*[LogL(H0)-LogL(H1)]  =                                        17.89279 
#> Kodde-Palm C*:       95%: 2.70554                                   99%: 5.41189 
#> Coelli (1995) skewness test on OLS residuals
#> M3T: z                         =                                        -4.12175 
#> M3T: p.value                   =                                         0.00004 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> (Intercept)       -1.31194    0.41859 -3.1342 0.0017234 ** 
#> log(AREA)          0.38278    0.14297  2.6772 0.0074236 ** 
#> log(LABOR)         0.42105    0.10992  3.8303 0.0001280 ***
#> log(NPK)           0.23143    0.06065  3.8160 0.0001356 ***
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zu_(Intercept)    -1.78673    0.20176 -8.8555 < 2.2e-16 ***
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zv_(Intercept)    -4.26963    0.40584 -10.521 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 14:08 
#> Log likelihood status: successful convergence  
#> --------------------------------------------------------------------------------  
#> 
#> ------------------------------------------------------------ 
#> Metafrontier Coefficients (sfa):
#> Meta-optim solver  : BFGS maximization 
#>               Estimate Std. Error z value  Pr(>|z|)    
#> (Intercept) -1.0031443  0.0568887 -17.634 < 2.2e-16 ***
#> log(AREA)    0.3670206  0.0091533  40.097 < 2.2e-16 ***
#> log(LABOR)   0.3297853  0.0096542  34.160 < 2.2e-16 ***
#> log(NPK)     0.2648079  0.0058572  45.211 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#>   Meta-frontier model details:
#> -------------------------------------------------------------------------------- 
#> Normal-Half Normal SF Model 
#> Dependent Variable:                                          group_fitted_values 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                         579 
#> Log likelihood value:                                                  553.35240 
#> Log likelihood gradient norm:                                        5.37373e-04 
#> Estimation based on:                                         N =  344 and K =  6 
#> Inf. Cr:                                        AIC  =  -1094.7 AIC/N  =  -3.182 
#>                                                 BIC  =  -1071.7 BIC/N  =  -3.115 
#>                                                 HQIC =  -1085.5 HQIC/N =  -3.156 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.00235 
#>            Sigma(v)           =                                          0.00235 
#>            Sigma-squared(u)   =                                          0.00000 
#>            Sigma(u)           =                                          0.00000 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          0.04844 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.00017 
#> Lambda = sigma(u)/sigma(v)    =                                          0.01294 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.00006 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         0.00050 
#> Average efficiency E[exp(-ui)] =                                         0.99950 
#> -------------------------------------------------------------------------------- 
#> Stochastic Production/Profit Frontier, e = v - u 
#> -----[ Tests vs. No Inefficiency ]-----
#> Likelihood Ratio Test of Inefficiency
#> Deg. freedom for inefficiency model                                            1 
#> Log Likelihood for OLS Log(H0) =                                       553.35242 
#> LR statistic:  
#> Chisq = 2*[LogL(H0)-LogL(H1)]  =                                        -0.00003 
#> Kodde-Palm C*:       95%: 2.70554                                   99%: 5.41189 
#> Coelli (1995) skewness test on OLS residuals
#> M3T: z                         =                                         4.16139 
#> M3T: p.value                   =                                         0.00003 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> (Intercept)       -1.00314    0.05689 -17.633 < 2.2e-16 ***
#> .X2                0.36702    0.00915  40.097 < 2.2e-16 ***
#> .X3                0.32979    0.00965  34.160 < 2.2e-16 ***
#> .X4                0.26481    0.00586  45.211 < 2.2e-16 ***
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value Pr(>|z|)
#> Zu_(Intercept)      -14.75     174.37 -0.0846   0.9326
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zv_(Intercept)    -6.05510    0.07698 -78.658 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 14:08 
#> Log likelihood status: successful convergence  
#> --------------------------------------------------------------------------------  
#> Log likelihood status: successful convergence  
#> 
#> ------------------------------------------------------------ 
#> Efficiency Statistics (group means):
#> ------------------------------------------------------------ 
#>        N_obs N_valid TE_group_BC TE_group_JLMS TE_meta_BC TE_meta_JLMS  MTR_BC
#> small    125     125     0.71065       0.70090    0.71030      0.70055 0.99950
#> medium   104     104     0.71253       0.70965    0.71217      0.70930 0.99950
#> large    115     115     0.74772       0.74406    0.74734      0.74369 0.99950
#>        MTR_JLMS
#> small   0.99950
#> medium  0.99950
#> large   0.99950
#> 
#> Overall:
#> TE_group_BC=0.7236  TE_group_JLMS=0.7182
#> TE_meta_BC=0.7233   TE_meta_JLMS=0.7178
#> MTR_BC=0.9995     MTR_JLMS=0.9995
#> ------------------------------------------------------------ 
#> Total Log-likelihood: 479.063 
#> AIC: -910.126   BIC: -817.9506   HQIC: -873.4137 
#> ------------------------------------------------------------ 
#> Model was estimated on : Apr Fri 24, 2026 at 14:08
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
meta_ordonnell <- smfa(
  formula     = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data        = ricephil,
  group       = "group",
  S           = 1,
  udist       = "hnormal",
  groupType   = "sfacross",
  metaMethod  = "sfa",
  sfaApproach = "ordonnell"
)
#> Warning: The residuals of the OLS are right-skewed. This may indicate the absence of inefficiency or
#>   model misspecification or sample 'bad luck'
summary(meta_ordonnell)
#> Warning: 344 MTR value(s) > 1 detected in O'Donnell SFA approach. This
#> typically occurs when the second-stage SFA estimates near-zero inefficiency
#> (sigma_u -> 0), causing TE_meta ~= 1 and MTR = TE_meta/TE_group > 1. Consider
#> using metaMethod='lp' or sfaApproach='huang' instead.
#> ============================================================ 
#> Stochastic Metafrontier Analysis
#> Metafrontier method: SFA Metafrontier [O'Donnell et al. (2008), envelope] 
#> Stochastic Production/Profit Frontier, e = v - u 
#> SFA approach       : ordonnell 
#> Group approach     : Stochastic Frontier Analysis 
#> Group estimator    : sfacross 
#> Group optim solver : BFGS maximization 
#> Groups ( 3 ): small, medium, large 
#> Total observations : 344 
#> Distribution       : hnormal 
#> ============================================================ 
#> 
#> ------------------------------------------------------------ 
#> Group: small (N = 125)  Log-likelihood: -50.98578
#> ------------------------------------------------------------ 
#> -------------------------------------------------------------------------------- 
#> Normal-Half Normal SF Model 
#> Dependent Variable:                                                    log(PROD) 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                          42 
#> Log likelihood value:                                                  -50.98578 
#> Log likelihood gradient norm:                                        9.40653e-06 
#> Estimation based on:                                         N =  125 and K =  6 
#> Inf. Cr:                                           AIC  =  114.0 AIC/N  =  0.912 
#>                                                    BIC  =  130.9 BIC/N  =  1.048 
#>                                                    HQIC =  120.9 HQIC/N =  0.967 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.05318 
#>            Sigma(v)           =                                          0.05318 
#>            Sigma-squared(u)   =                                          0.23435 
#>            Sigma(u)           =                                          0.23435 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          0.53622 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.81504 
#> Lambda = sigma(u)/sigma(v)    =                                          2.09921 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.61558 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         0.38626 
#> Average efficiency E[exp(-ui)] =                                         0.70643 
#> -------------------------------------------------------------------------------- 
#> Stochastic Production/Profit Frontier, e = v - u 
#> -----[ Tests vs. No Inefficiency ]-----
#> Likelihood Ratio Test of Inefficiency
#> Deg. freedom for inefficiency model                                            1 
#> Log Likelihood for OLS Log(H0) =                                       -54.80277 
#> LR statistic:  
#> Chisq = 2*[LogL(H0)-LogL(H1)]  =                                         7.63398 
#> Kodde-Palm C*:       95%: 2.70554                                   99%: 5.41189 
#> Coelli (1995) skewness test on OLS residuals
#> M3T: z                         =                                        -3.57676 
#> M3T: p.value                   =                                         0.00035 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> (Intercept)       -1.58745    0.51274 -3.0960  0.001962 ** 
#> log(AREA)          0.24014    0.11834  2.0292  0.042440 *  
#> log(LABOR)         0.43464    0.12292  3.5361  0.000406 ***
#> log(NPK)           0.30516    0.05701  5.3523 8.682e-08 ***
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zu_(Intercept)    -1.45093    0.29867  -4.858 1.186e-06 ***
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zv_(Intercept)    -2.93406    0.35401  -8.288 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 14:08 
#> Log likelihood status: successful convergence  
#> --------------------------------------------------------------------------------  
#> 
#> ------------------------------------------------------------ 
#> Group: medium (N = 104)  Log-likelihood: -15.28164
#> ------------------------------------------------------------ 
#> -------------------------------------------------------------------------------- 
#> Normal-Half Normal SF Model 
#> Dependent Variable:                                                    log(PROD) 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                          41 
#> Log likelihood value:                                                  -15.28164 
#> Log likelihood gradient norm:                                        3.83566e-05 
#> Estimation based on:                                         N =  104 and K =  6 
#> Inf. Cr:                                            AIC  =  42.6 AIC/N  =  0.409 
#>                                                     BIC  =  58.4 BIC/N  =  0.562 
#>                                                     HQIC =  49.0 HQIC/N =  0.471 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.01058 
#>            Sigma(v)           =                                          0.01058 
#>            Sigma-squared(u)   =                                          0.22010 
#>            Sigma(u)           =                                          0.22010 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          0.48030 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.95412 
#> Lambda = sigma(u)/sigma(v)    =                                          4.56034 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.88314 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         0.37433 
#> Average efficiency E[exp(-ui)] =                                         0.71330 
#> -------------------------------------------------------------------------------- 
#> Stochastic Production/Profit Frontier, e = v - u 
#> -----[ Tests vs. No Inefficiency ]-----
#> Likelihood Ratio Test of Inefficiency
#> Deg. freedom for inefficiency model                                            1 
#> Log Likelihood for OLS Log(H0) =                                       -21.11323 
#> LR statistic:  
#> Chisq = 2*[LogL(H0)-LogL(H1)]  =                                        11.66318 
#> Kodde-Palm C*:       95%: 2.70554                                   99%: 5.41189 
#> Coelli (1995) skewness test on OLS residuals
#> M3T: z                         =                                        -2.91021 
#> M3T: p.value                   =                                         0.00361 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> (Intercept)       -0.08182    0.50668 -0.1615 0.8717190    
#> log(AREA)          0.47410    0.13984  3.3903 0.0006981 ***
#> log(LABOR)         0.17935    0.10201  1.7581 0.0787310 .  
#> log(NPK)           0.20255    0.08130  2.4913 0.0127289 *  
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zu_(Intercept)    -1.51367    0.23549 -6.4276 1.296e-10 ***
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zv_(Intercept)    -4.54846    0.76429 -5.9512 2.661e-09 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 14:08 
#> Log likelihood status: successful convergence  
#> --------------------------------------------------------------------------------  
#> 
#> ------------------------------------------------------------ 
#> Group: large (N = 115)  Log-likelihood: -8.02197
#> ------------------------------------------------------------ 
#> -------------------------------------------------------------------------------- 
#> Normal-Half Normal SF Model 
#> Dependent Variable:                                                    log(PROD) 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                          68 
#> Log likelihood value:                                                   -8.02197 
#> Log likelihood gradient norm:                                        4.01301e-05 
#> Estimation based on:                                         N =  115 and K =  6 
#> Inf. Cr:                                            AIC  =  28.0 AIC/N  =  0.244 
#>                                                     BIC  =  44.5 BIC/N  =  0.387 
#>                                                     HQIC =  34.7 HQIC/N =  0.302 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.01399 
#>            Sigma(v)           =                                          0.01399 
#>            Sigma-squared(u)   =                                          0.16751 
#>            Sigma(u)           =                                          0.16751 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          0.42602 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.92293 
#> Lambda = sigma(u)/sigma(v)    =                                          3.46063 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.81315 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         0.32656 
#> Average efficiency E[exp(-ui)] =                                         0.74195 
#> -------------------------------------------------------------------------------- 
#> Stochastic Production/Profit Frontier, e = v - u 
#> -----[ Tests vs. No Inefficiency ]-----
#> Likelihood Ratio Test of Inefficiency
#> Deg. freedom for inefficiency model                                            1 
#> Log Likelihood for OLS Log(H0) =                                       -16.96836 
#> LR statistic:  
#> Chisq = 2*[LogL(H0)-LogL(H1)]  =                                        17.89279 
#> Kodde-Palm C*:       95%: 2.70554                                   99%: 5.41189 
#> Coelli (1995) skewness test on OLS residuals
#> M3T: z                         =                                        -4.12175 
#> M3T: p.value                   =                                         0.00004 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> (Intercept)       -1.31194    0.41859 -3.1342 0.0017234 ** 
#> log(AREA)          0.38278    0.14297  2.6772 0.0074236 ** 
#> log(LABOR)         0.42105    0.10992  3.8303 0.0001280 ***
#> log(NPK)           0.23143    0.06065  3.8160 0.0001356 ***
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zu_(Intercept)    -1.78673    0.20176 -8.8555 < 2.2e-16 ***
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zv_(Intercept)    -4.26963    0.40584 -10.521 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 14:08 
#> Log likelihood status: successful convergence  
#> --------------------------------------------------------------------------------  
#> 
#> ------------------------------------------------------------ 
#> Metafrontier Coefficients (sfa):
#> Meta-optim solver  : BFGS maximization 
#>               Estimate Std. Error z value  Pr(>|z|)    
#> (Intercept) -0.6114342  0.0414990 -14.734 < 2.2e-16 ***
#> log(AREA)    0.3937848  0.0072782  54.105 < 2.2e-16 ***
#> log(LABOR)   0.2791270  0.0076764  36.361 < 2.2e-16 ***
#> log(NPK)     0.2409454  0.0046573  51.735 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#>   Meta-frontier model details:
#> -------------------------------------------------------------------------------- 
#> Normal-Half Normal SF Model 
#> Dependent Variable:                                                  lp_envelope 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                         436 
#> Log likelihood value:                                                  632.20951 
#> Log likelihood gradient norm:                                        5.34744e-02 
#> Estimation based on:                                         N =  344 and K =  6 
#> Inf. Cr:                                        AIC  =  -1252.4 AIC/N  =  -3.641 
#>                                                 BIC  =  -1229.4 BIC/N  =  -3.574 
#>                                                 HQIC =  -1243.2 HQIC/N =  -3.614 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.00148 
#>            Sigma(v)           =                                          0.00148 
#>            Sigma-squared(u)   =                                          0.00000 
#>            Sigma(u)           =                                          0.00000 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          0.03851 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.00013 
#> Lambda = sigma(u)/sigma(v)    =                                          0.01121 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.00005 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         0.00034 
#> Average efficiency E[exp(-ui)] =                                         0.99966 
#> -------------------------------------------------------------------------------- 
#> Stochastic Production/Profit Frontier, e = v - u 
#> -----[ Tests vs. No Inefficiency ]-----
#> Likelihood Ratio Test of Inefficiency
#> Deg. freedom for inefficiency model                                            1 
#> Log Likelihood for OLS Log(H0) =                                       632.20952 
#> LR statistic:  
#> Chisq = 2*[LogL(H0)-LogL(H1)]  =                                        -0.00003 
#> Kodde-Palm C*:       95%: 2.70554                                   99%: 5.41189 
#> Coelli (1995) skewness test on OLS residuals
#> M3T: z                         =                                         6.24028 
#> M3T: p.value                   =                                         0.00000 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> (Intercept)       -0.61143    0.04150 -14.734 < 2.2e-16 ***
#> .X2                0.39378    0.00728  54.105 < 2.2e-16 ***
#> .X3                0.27913    0.00768  36.361 < 2.2e-16 ***
#> .X4                0.24095    0.00466  51.735 < 2.2e-16 ***
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value Pr(>|z|)
#> Zu_(Intercept)     -15.496    172.306 -0.0899   0.9283
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zv_(Intercept)    -6.51356    0.07665 -84.978 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 14:08 
#> Log likelihood status: successful convergence  
#> --------------------------------------------------------------------------------  
#> Log likelihood status: successful convergence  
#> 
#> ------------------------------------------------------------ 
#> Efficiency Statistics (group means):
#> ------------------------------------------------------------ 
#>        N_obs N_valid TE_group_BC TE_group_JLMS TE_meta_BC TE_meta_JLMS  MTR_BC
#> small    125     125     0.71065       0.70090    0.99966      0.99966 1.49276
#> medium   104     104     0.71253       0.70965    0.99966      0.99966 1.50575
#> large    115     115     0.74772       0.74406    0.99966      0.99966 1.41180
#>        MTR_JLMS
#> small   1.51673
#> medium  1.51248
#> large   1.41943
#> 
#> Overall:
#> TE_group_BC=0.7236  TE_group_JLMS=0.7182
#> TE_meta_BC=0.9997   TE_meta_JLMS=0.9997
#> MTR_BC=1.4701     MTR_JLMS=1.4829
#> ------------------------------------------------------------ 
#> Total Log-likelihood: 557.9201 
#> AIC: -1067.84   BIC: -975.6648   HQIC: -1031.128 
#> ------------------------------------------------------------ 
#> Model was estimated on : Apr Fri 24, 2026 at 14:08
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
[`efficiencies()`](https://SulmanOlieko.github.io/smfa/reference/efficiencies.md):

``` r
eff <- efficiencies(meta_lp)
head(eff)
#>   id  group       u_g TE_group_JLMS TE_group_BC TE_group_BC_reciprocal
#> 1  1 medium 0.2697165     0.7635959   0.7673345               1.316036
#> 2  2  large 0.3515642     0.7035867   0.7080897               1.430406
#> 3  3  large 0.2774565     0.7577085   0.7623358               1.327899
#> 4  4 medium 0.1710417     0.8427864   0.8461331               1.191355
#> 5  5  large 0.2119629     0.8089947   0.8133556               1.242901
#> 6  6  small 0.1987499     0.8197549   0.8275685               1.232467
#>         uLB_g     uUB_g        m_g TE_group_mode  teBCLB_g  teBCUB_g    u_meta
#> 1 0.077581942 0.4657010 0.26858570     0.7644599 0.6276949 0.9253512 0.3944439
#> 2 0.130356248 0.5739174 0.35118207     0.7038556 0.5633144 0.8777827 0.3779836
#> 3 0.065447909 0.4980807 0.27501606     0.7595599 0.6076959 0.9366478 0.3049531
#> 4 0.018022507 0.3583190 0.15885675     0.8531186 0.6988501 0.9821389 0.1710417
#> 5 0.027125654 0.4268531 0.20231520     0.8168374 0.6525594 0.9732389 0.2379271
#> 6 0.009050601 0.5251973 0.07998025     0.9231346 0.5914386 0.9909902 0.3295263
#>   TE_meta_JLMS TE_meta_BC  MTR_JLMS    MTR_BC
#> 1    0.6740548  0.6773549 0.8827375 0.8827375
#> 2    0.6852418  0.6896274 0.9739266 0.9739266
#> 3    0.7371580  0.7416598 0.9728780 0.9728780
#> 4    0.8427864  0.8461331 1.0000000 1.0000000
#> 5    0.7882601  0.7925093 0.9743700 0.9743700
#> 6    0.7192644  0.7261201 0.8774139 0.8774139

# Subset for a specific group
eff_small <- eff[eff$group == "small", ]
summary(eff_small[, c("TE_group_BC", "TE_meta_BC", "MTR_BC")])
#>   TE_group_BC       TE_meta_BC         MTR_BC      
#>  Min.   :0.1737   Min.   :0.1179   Min.   :0.5907  
#>  1st Qu.:0.6217   1st Qu.:0.5678   1st Qu.:0.8313  
#>  Median :0.7427   Median :0.6683   Median :0.9204  
#>  Mean   :0.7107   Mean   :0.6413   Mean   :0.8998  
#>  3rd Qu.:0.8165   3rd Qu.:0.7509   3rd Qu.:0.9908  
#>  Max.   :0.9275   Max.   :0.8774   Max.   :1.0000
```

## Other Extractors

``` r
coef(meta_qp)          # metafrontier coefficients
#> (Intercept)   log(AREA)  log(LABOR)    log(NPK) 
#>  -0.6117795   0.3937843   0.2791273   0.2409454
vcov(meta_qp)          # variance-covariance matrix
#>                (Intercept)   `log(AREA)`  `log(LABOR)`    `log(NPK)`
#> (Intercept)   8.514304e-04  1.954064e-04 -1.729963e-04 -3.730091e-05
#> `log(AREA)`   1.954064e-04  5.359537e-05 -3.976453e-05 -9.599635e-06
#> `log(LABOR)` -1.729963e-04 -3.976453e-05  5.962116e-05 -1.454127e-05
#> `log(NPK)`   -3.730091e-05 -9.599635e-06 -1.454127e-05  2.194543e-05
logLik(meta_lp)        # log-likelihood
#> 'log Lik.' -74.28939 (df=18)
ic(meta_lp)            # AIC, BIC, HQIC
#>        AIC      BIC    HQIC
#> 1 184.5788 253.7103 212.113
nobs(meta_lp)          # number of observations
#> [1] 344
fitted(meta_lp)        # fitted values
#>   [1] 2.4692860 2.7439118 2.6260597 1.7413418 2.4130830 0.8386720 2.1838447
#>   [8] 2.1368249 2.5632590 2.6811266 1.0256316 0.1866309 1.6584919 2.3673143
#>  [15] 0.5318491 1.0749268 2.9226606 3.1820088 2.7526696 2.7875600 2.2775942
#>  [22] 1.8328112 2.9712349 2.3411190 2.9540079 1.5904844 2.4159250 1.8695405
#>  [29] 1.8806492 1.8081081 1.2053265 1.2472296 1.9714412 1.1387651 2.7063760
#>  [36] 1.9160249 1.6961237 2.8670880 1.2103088 2.1644129 1.7155060 2.0399982
#>  [43] 1.8390740 2.5101720 2.5445910 2.7921145 1.6829644 2.5266572 0.7143551
#>  [50] 2.1361437 2.0008358 2.4644202 2.6488824 1.2647850 0.3109477 1.6718514
#>  [57] 2.3258381 0.5434242 1.0306822 2.9736508 3.1803910 2.7121716 2.7667310
#>  [64] 2.5782398 1.8760712 3.0852801 2.2654800 2.8031443 1.5769829 2.1311538
#>  [71] 1.9129112 1.8138741 1.7296852 1.2309918 1.2858102 1.9978788 1.0892187
#>  [78] 2.0766095 1.7471685 1.8009587 2.8613923 1.1336461 2.1069233 1.6561833
#>  [85] 2.0988490 1.9150465 2.5787879 2.8104161 2.7846665 1.5953777 2.5187777
#>  [92] 0.9488430 2.2119053 2.1542782 2.5159164 2.6740634 1.1166023 0.3722135
#>  [99] 1.6928734 2.2298570 0.5843291 1.2561007 3.0662891 3.2740149 3.0130060
#> [106] 2.8966776 2.3924492 1.9195720 3.1038424 2.4698633 2.8910529 1.7264018
#> [113] 2.4273869 1.9378162 1.6742073 0.5639574 1.1764958 1.2496804 1.8637001
#> [120] 1.1691521 2.0872688 1.5744022 1.7298469 2.9304411 1.0201563 1.9811044
#> [127] 1.6681942 2.1348045 1.5313581 2.5150745 2.9973843 2.7681912 1.7154370
#> [134] 2.5772810 0.8951080 2.0540639 2.2076712 2.5922775 2.5457456 1.6160185
#> [141] 0.2355537 1.8541395 2.0494468 0.4914381 0.9626502 2.8953205 3.2430506
#> [148] 2.8505586 2.8138232 2.3029447 1.9745591 2.9895696 2.1532577 2.8004837
#> [155] 1.7566981 1.7815008 2.0389834 2.0122922 0.6748837 1.4306932 1.5142029
#> [162] 1.1333442 1.1276370 2.1006493 2.1675322 1.8864125 2.5926241 1.0516188
#> [169] 2.0762856 1.4716730 2.0082786 1.8555625 2.5722335 2.9254124 2.8315667
#> [176] 1.7095770 2.5310052 0.9156910 2.1895030 2.1133067 2.4433770 2.5284215
#> [183] 1.5917408 0.3365432 1.9200143 1.9170048 0.5100709 1.0241596 2.8920102
#> [190] 3.3345192 2.7927954 2.8126605 2.4130604 1.9143897 3.0940329 2.4190965
#> [197] 3.0585010 1.6222408 1.6625476 1.8857861 1.9242456 0.6377106 1.1021552
#> [204] 1.3132326 1.7753533 0.9128345 2.4091480 2.1650832 1.4916809 2.8787436
#> [211] 0.6548046 2.1711145 1.4429747 1.8281572 1.9867232 2.3518294 2.8197699
#> [218] 2.5347746 1.8711220 2.4972608 0.8320827 2.9844363 2.2310199 2.5788511
#> [225] 2.5037044 1.7868914 0.2655194 1.7010836 1.9617781 0.4734649 0.9704104
#> [232] 3.0409983 3.5790913 2.8954003 2.9146311 2.6674014 2.0420367 3.2798123
#> [239] 2.6144439 3.1474022 1.6457397 1.9044427 2.0077175 2.3988297 0.6098552
#> [246] 1.2576153 1.2932868 1.1942484 1.4741438 2.4985791 2.0472037 1.6638331
#> [253] 1.1832652 0.7223914 2.2743936 1.4444894 2.3832867 2.0192976 2.4856384
#> [260] 2.8492927 2.6693680 1.8159416 2.5786298 0.8569476 2.7205239 2.3290874
#> [267] 2.3625518 2.6763391 1.7500781 0.4880103 1.7336934 1.7571496 0.4816027
#> [274] 0.8310833 3.0787333 3.4816827 3.0615238 2.8771885 2.5014493 2.0924022
#> [281] 3.4050940 2.3637466 2.7953274 1.6079663 1.9153483 1.1776235 1.8736594
#> [288] 0.7990109 1.1506511 1.2063827 1.0593765 1.5096871 2.4415489 2.0734100
#> [295] 1.3592284 1.1583390 0.8507941 1.9451625 1.4728146 2.1092525 1.7678016
#> [302] 2.1872556 2.4621500 2.5665104 1.7351918 2.5951799 1.0868779 2.2386175
#> [309] 2.1667252 2.2906645 2.7338493 1.7816010 0.4911352 1.8329253 1.8120439
#> [316] 0.4770658 0.9030706 2.7378122 3.4560616 2.9363898 2.8635826 2.5768058
#> [323] 1.9027268 3.3379239 2.6178754 2.7099038 1.6341968 2.3195787 1.1575454
#> [330] 2.0290368 0.1534083 1.1206897 1.3624220 1.1015321 1.5564064 2.3988119
#> [337] 2.1502764 1.4867585 1.1304626 1.0437816 1.9765226 1.5564463 2.1815665
#> [344] 2.0019192
residuals(meta_lp)     # residuals
#>   [1]  5.40071399  7.60608821  7.35394035  3.08865822  6.32691704  1.00132804
#>   [7]  5.17615529  4.53317508  6.63674105  6.60887340 -0.19563164  0.73336915
#>  [13]  2.25150806  5.08268567  0.38815087  0.07507323  7.88733941 17.88799120
#>  [19] 10.81733041  9.45244004  2.83240578  3.59718878  6.68876505  4.87888104
#>  [25]  9.51599215  1.57951565  2.64407501  3.65045952  2.25935083  0.88189188
#>  [31]  2.61467351 -0.09722964  3.08855880 -0.44876507  9.94362401  0.15397506
#>  [37]  3.36387628 14.15291201  0.16969124  1.79558706  2.42449404  6.56000176
#>  [43] -0.04907398  4.66982802  7.66540903  7.64788553  1.44703560  6.66334280
#>  [49]  0.25564491  5.68385626  3.83916415  7.42557979  5.91111760 -0.16478498
#>  [55]  0.60905228  1.17814859  3.97416190  0.37657580  1.08931777  3.00634923
#>  [61] 14.93960900 12.60782835 10.34326896  3.26176019  2.40392875  9.79471993
#>  [67]  3.66451996 10.94685569  0.95301713  3.84884622  4.47708879  0.99612587
#>  [73]  1.44031477  0.55900818  1.01418985  3.06212118 -0.44921865  5.46339053
#>  [79]  1.33283151  3.48904129 10.84860767 -0.21364610  1.25307665  3.40381667
#>  [85]  5.54115104 -0.16504651  6.34121212 10.47958390  7.61533351  4.24462232
#>  [91]  7.32122234  0.75115697  5.37809466  2.39572179  7.37408362  9.51593665
#>  [97]  0.26339774  0.54778649  2.26712661  4.16014297  0.24567094  1.09389932
#> [103] 13.40371088 18.57598513 12.85699397 11.73332239  7.95755082  4.01042799
#> [109] 13.45615763  7.60013666 12.00894706  2.59359823  6.49261310  4.59218376
#> [115]  1.36579271  0.30604264  1.63350416  1.56031962  2.68629994 -0.01915215
#> [121]  4.86273120  1.36559777  3.79015309 13.30955889  0.35984371  2.01889564
#> [127]  3.39180576  5.40519554  1.17864190  4.79492550 15.58261574  6.15180881
#> [133]  3.16456303  8.37271904  0.34489200  5.58593613  4.14232878  6.37772255
#> [139]  8.67425440  1.42398147  1.05444631  1.59586055  3.93055318  0.51856192
#> [145]  0.87734977 15.09467954 16.99694940  3.35944136  9.60617679  2.15705528
#> [151]  4.46544092  5.29043041  4.39674232 12.05951625  1.41330189  1.94849916
#> [157]  3.62101659  4.88770778  0.52511627  1.09930676  2.01579715  1.16665577
#> [163]  0.53236303  4.38935065  2.66246782  3.86358755  9.57737588  0.78838120
#> [169]  2.38371439  2.20832700  3.10172137  1.45443752  4.96776654 14.14458758
#> [175]  4.52843330  2.75042302  5.46899481  0.64430897  4.39049702  3.72669331
#> [181]  5.97662295  3.17157851  0.38825921  0.44345679  1.11998566  3.37299521
#> [187]  0.49992913  1.18584036 17.02798983 17.73548082 10.13720459 10.11733955
#> [193]  5.45693963  1.21561029 12.08596709  1.49090349  5.54149905  2.46775920
#> [199]  1.09745241  3.40421394  1.24575439  0.42228944  1.24784484  1.06676742
#> [205]  3.37464671 -0.31283447  3.20085200  2.66491685  1.67831914  7.01125643
#> [211]  0.95519543  2.65888548  0.85702530  1.76184276  3.62327679  2.15817061
#> [217] 13.79023007  7.63522540  2.49887800  6.42273922  0.63791727 10.07556365
#> [223]  2.67898013  5.24114893  8.07629561  2.21310856  0.90448056  1.79891642
#> [229]  3.23822187  0.49653508  1.32958960 16.68900167 22.96090874 13.10459974
#> [235]  9.45536891 11.49259857  1.63796331 14.72018770  5.98555608  8.90259777
#> [241]  2.49426034  3.20555730  3.37228245  0.26117034  0.34014481  1.87238465
#> [247]  0.21671324  1.34575157  0.52585618  5.27142095  2.78279626  2.24616690
#> [253]  1.71673480  0.80760862  2.83560641  3.15551060  4.83671332  5.76070235
#> [259]  1.88436164  9.43070726  2.76063198  2.32405839  6.62137016  0.15305242
#> [265]  7.21947612  1.95091263  5.45744816  5.92366094  0.17992192  0.94198971
#> [271]  1.91630659  3.30285039 -0.11160275  0.95891668  7.04126671 19.23831733
#> [277]  3.37847620  8.34281148  8.76855073  1.81759779  7.17490605  2.51625341
#> [283]  7.64467261  1.61203371  1.39465165  1.35237647  1.75634061  0.26098908
#> [289] -0.09065105  0.63361732  0.27062353  0.38031288  6.46845106  1.74659000
#> [295]  0.85077164  1.19166097  0.24920592  0.81483750  1.28718539  2.86074746
#> [301]  1.93219844  5.08274437 11.73785003  8.38348964  2.17480822  8.44482011
#> [307]  0.75312209  6.04138253  4.45327478  6.35933553  9.82615068  2.04839897
#> [313]  1.20886477  2.90707466  3.06795612  0.21293418  1.07692936 11.29218781
#> [319] 27.64393839 18.76361023 11.94641737  6.85319417  2.69727323 15.75207609
#> [325]  2.53212457 13.34009620  2.64580325  7.02042126  1.60245457  4.02096320
#> [331] -0.06340833  2.49931032  4.79757796  1.69846793  0.37359360  8.32118806
#> [337]  5.48972358  1.73324152  2.54953741  0.47621842  3.52347738  3.73355374
#> [343]  5.49843347  5.72808077
```
