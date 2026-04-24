# Latent Class SFA Metafrontier (groupType = "sfalcmcross")

## Overview

In many applications, the technology groups that firms belong to are
**unobserved** — we cannot directly observe which firms operate under
which technology type. The latent class model (LCM) addresses this by:

1.  Fitting a **pooled latent class SFA** on the entire dataset using
    the `sfaR` implementation based on Dakpo et al. (2021),
    simultaneously estimating class-specific frontier parameters and
    class membership probabilities for each firm.
2.  Assigning each firm to a class based on its highest **posterior
    class probability**.
3.  Using these assignments as the technology groups for the
    metafrontier.

This approach is appropriate when: - No observed group variable is
available. - Technology heterogeneity is suspected but not directly
measurable. - A priori group boundaries are unclear or arbitrary.

## Data Preparation

We use the `utility` dataset from `sfaR`, which contains 791
observations from US electricity utilities. We estimate a cost frontier
with no explicit group variable.

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
data("utility", package = "sfaR")
```

## Method 1: LCM + LP Metafrontier

Fit a 2-class latent class pooled SFA, then estimate the LP
deterministic envelope over the inferred class frontiers.

``` r

meta_lcm_lp <- smfa(
  formula    = log(tc/wf) ~ log(y) + log(wl/wf) + log(wk/wf),
  data       = utility,
  S          = -1,          # cost frontier (S = -1)
  groupType  = "sfalcmcross",
  lcmClasses = 2,           # number of latent classes
  metaMethod = "lp"
)
#> Initialization: SFA + halfnormal - normal distributions...
#> LCM 2 Classes Estimation...
#> Warning: hessian is singular for 'qr.solve' switching to 'ginv'
summary(meta_lcm_lp)
#> ============================================================ 
#> Stochastic Metafrontier Analysis
#> Metafrontier method: Linear Programming (LP) Metafrontier 
#> Stochastic Cost Frontier, e = v + u 
#> Group approach     : Latent Class Stochastic Frontier Analysis 
#> Group estimator    : sfalcmcross 
#> Group optim solver : BFGS maximization 
#>   (Pooled LCM - latent classes used as groups)
#> Groups ( 2 ): Class_1, Class_2 
#> Total observations : 791 
#> Distribution       : hnormal 
#> ============================================================ 
#> 
#> ------------------------------------------------------------ 
#> Pooled LCM (2 classes) on all data (N = 791)  Log-likelihood: 61.35325
#> ------------------------------------------------------------ 
#> 
#>   -- Latent Class 1 --
#>   Frontier:
#>             Coefficient  Std. Error z value  Pr(>|z|)    
#> (Intercept) -1.4472e+00  3.9123e-05  -36992 < 2.2e-16 ***
#> log(y)       8.4541e-01  2.3364e-06  361846 < 2.2e-16 ***
#> log(wl/wf)   3.5408e-01  4.4754e-06   79118 < 2.2e-16 ***
#> log(wk/wf)   4.2883e-01  1.3682e-05   31343 < 2.2e-16 ***
#>   Var(u):
#>                Coefficient  Std. Error   z value  Pr(>|z|)    
#> Zu_(Intercept) -1.7658e+00  5.8803e-08 -30029863 < 2.2e-16 ***
#>   Var(v):
#>                Coefficient  Std. Error    z value  Pr(>|z|)    
#> Zv_(Intercept) -3.8759e+01  3.2680e-13 -1.186e+14 < 2.2e-16 ***
#>   Sigma_u=0.4136  Sigma_v=0.0000  Sigma=0.4136  Gamma=1.0000  Lambda=107911523.6712
#> 
#>   -- Latent Class 2 --
#>   Frontier:
#>             Coefficient  Std. Error   z value  Pr(>|z|)    
#> (Intercept) -2.0490e+00  2.5608e-05 -80011.07 < 2.2e-16 ***
#> log(y)       1.0079e+00  4.1082e-04   2453.37 < 2.2e-16 ***
#> log(wl/wf)  -2.5916e-02  6.3375e-05   -408.93 < 2.2e-16 ***
#> log(wk/wf)   8.8450e-01  6.9315e-05  12760.74 < 2.2e-16 ***
#>   Var(u):
#>                Coefficient  Std. Error  z value  Pr(>|z|)    
#> Zu_(Intercept) -3.0117e+00  1.1348e-06 -2653956 < 2.2e-16 ***
#>   Var(v):
#>                Coefficient  Std. Error  z value  Pr(>|z|)    
#> Zv_(Intercept) -4.9663e+00  5.3865e-07 -9219998 < 2.2e-16 ***
#>   Sigma_u=0.2218  Sigma_v=0.0835  Sigma=0.2370  Gamma=0.8759  Lambda=2.6573
#> 
#>   -- Class Membership (logit) --
#>                 Coefficient  Std. Error  z value  Pr(>|z|)    
#> Cl1_(Intercept) -6.0163e-01  5.0453e-07 -1192458 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> Log likelihood status: successful convergence  
#> 
#> ------------------------------------------------------------ 
#> Metafrontier Coefficients (lp):
#>   (LP: deterministic envelope - no estimated parameters)
#> 
#> ------------------------------------------------------------ 
#> Efficiency Statistics (group means):
#> ------------------------------------------------------------ 
#>         N_obs N_valid TE_group_BC TE_group_JLMS TE_meta_BC TE_meta_JLMS  MTR_BC
#> Class_1   202     202     0.68291       0.68291    0.68291      0.68291 1.00000
#> Class_2   589     589     0.85142       0.84951    0.85142      0.84951 1.00000
#>         MTR_JLMS
#> Class_1  1.00000
#> Class_2  1.00000
#> 
#> Overall:
#> TE_group_BC=0.7672  TE_group_JLMS=0.7662
#> TE_meta_BC=0.7672   TE_meta_JLMS=0.7662
#> MTR_BC=1.0000     MTR_JLMS=1.0000
#> 
#> ------------------------------------------------------------ 
#> Posterior Class Membership (pooled LCM):
#> ------------------------------------------------------------ 
#>         % assigned Mean post. prob.
#> Class 1       25.5            0.354
#> Class 2       74.5            0.646
#> ------------------------------------------------------------ 
#> Total Log-likelihood: 61.35325 
#> AIC: -96.70649   BIC: -35.95362   HQIC: -73.35552 
#> ------------------------------------------------------------ 
#> Model was estimated on : Apr Fri 24, 2026 at 17:43
```

> **Note:** The `group` argument is not needed when
> `groupType = "sfalcmcross"` — the latent classes are identified
> automatically by the LCM. The `lcmClasses` argument controls the
> number of classes.

## Method 2: LCM + QP Metafrontier

``` r

meta_lcm_qp <- smfa(
  formula    = log(tc/wf) ~ log(y) + log(wl/wf) + log(wk/wf),
  data       = utility,
  S          = -1,
  groupType  = "sfalcmcross",
  lcmClasses = 2,
  metaMethod = "qp"
)
#> Initialization: SFA + halfnormal - normal distributions...
#> LCM 2 Classes Estimation...
#> Warning: hessian is singular for 'qr.solve' switching to 'ginv'
summary(meta_lcm_qp)
#> ============================================================ 
#> Stochastic Metafrontier Analysis
#> Metafrontier method: Quadratic Programming (QP) Metafrontier 
#> Stochastic Cost Frontier, e = v + u 
#> Group approach     : Latent Class Stochastic Frontier Analysis 
#> Group estimator    : sfalcmcross 
#> Group optim solver : BFGS maximization 
#>   (Pooled LCM - latent classes used as groups)
#> Groups ( 2 ): Class_1, Class_2 
#> Total observations : 791 
#> Distribution       : hnormal 
#> ============================================================ 
#> 
#> ------------------------------------------------------------ 
#> Pooled LCM (2 classes) on all data (N = 791)  Log-likelihood: 61.35325
#> ------------------------------------------------------------ 
#> 
#>   -- Latent Class 1 --
#>   Frontier:
#>             Coefficient  Std. Error z value  Pr(>|z|)    
#> (Intercept) -1.4472e+00  3.9123e-05  -36992 < 2.2e-16 ***
#> log(y)       8.4541e-01  2.3364e-06  361846 < 2.2e-16 ***
#> log(wl/wf)   3.5408e-01  4.4754e-06   79118 < 2.2e-16 ***
#> log(wk/wf)   4.2883e-01  1.3682e-05   31343 < 2.2e-16 ***
#>   Var(u):
#>                Coefficient  Std. Error   z value  Pr(>|z|)    
#> Zu_(Intercept) -1.7658e+00  5.8803e-08 -30029863 < 2.2e-16 ***
#>   Var(v):
#>                Coefficient  Std. Error    z value  Pr(>|z|)    
#> Zv_(Intercept) -3.8759e+01  3.2680e-13 -1.186e+14 < 2.2e-16 ***
#>   Sigma_u=0.4136  Sigma_v=0.0000  Sigma=0.4136  Gamma=1.0000  Lambda=107911523.6712
#> 
#>   -- Latent Class 2 --
#>   Frontier:
#>             Coefficient  Std. Error   z value  Pr(>|z|)    
#> (Intercept) -2.0490e+00  2.5608e-05 -80011.07 < 2.2e-16 ***
#> log(y)       1.0079e+00  4.1082e-04   2453.37 < 2.2e-16 ***
#> log(wl/wf)  -2.5916e-02  6.3375e-05   -408.93 < 2.2e-16 ***
#> log(wk/wf)   8.8450e-01  6.9315e-05  12760.74 < 2.2e-16 ***
#>   Var(u):
#>                Coefficient  Std. Error  z value  Pr(>|z|)    
#> Zu_(Intercept) -3.0117e+00  1.1348e-06 -2653956 < 2.2e-16 ***
#>   Var(v):
#>                Coefficient  Std. Error  z value  Pr(>|z|)    
#> Zv_(Intercept) -4.9663e+00  5.3865e-07 -9219998 < 2.2e-16 ***
#>   Sigma_u=0.2218  Sigma_v=0.0835  Sigma=0.2370  Gamma=0.8759  Lambda=2.6573
#> 
#>   -- Class Membership (logit) --
#>                 Coefficient  Std. Error  z value  Pr(>|z|)    
#> Cl1_(Intercept) -6.0163e-01  5.0453e-07 -1192458 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> Log likelihood status: successful convergence  
#> 
#> ------------------------------------------------------------ 
#> Metafrontier Coefficients (qp):
#>               Estimate Std. Error z value  Pr(>|z|)    
#> (Intercept) -1.3923607  0.0310122 -44.897 < 2.2e-16 ***
#> log(y)       0.8649986  0.0012419 696.519 < 2.2e-16 ***
#> log(wl/wf)   0.2909728  0.0047965  60.664 < 2.2e-16 ***
#> log(wk/wf)   0.5028978  0.0069500  72.359 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> ------------------------------------------------------------ 
#> Efficiency Statistics (group means):
#> ------------------------------------------------------------ 
#>         N_obs N_valid TE_group_BC TE_group_JLMS TE_meta_BC TE_meta_JLMS  MTR_BC
#> Class_1   202     202     0.68291       0.68291    0.67786      0.67786 0.99326
#> Class_2   589     589     0.85142       0.84951    0.84548      0.84359 0.99285
#>         MTR_JLMS
#> Class_1  0.99326
#> Class_2  0.99285
#> 
#> Overall:
#> TE_group_BC=0.7672  TE_group_JLMS=0.7662
#> TE_meta_BC=0.7617   TE_meta_JLMS=0.7607
#> MTR_BC=0.9931     MTR_JLMS=0.9931
#> 
#> ------------------------------------------------------------ 
#> Posterior Class Membership (pooled LCM):
#> ------------------------------------------------------------ 
#>         % assigned Mean post. prob.
#> Class 1       25.5            0.354
#> Class 2       74.5            0.646
#> ------------------------------------------------------------ 
#> Total Log-likelihood: 61.35325 
#> AIC: -88.70649   BIC: -9.26042   HQIC: -58.17061 
#> ------------------------------------------------------------ 
#> Model was estimated on : Apr Fri 24, 2026 at 17:43
```

## Method 3: LCM + SFA (Huang)

``` r

meta_lcm_huang <- smfa(
  formula     = log(tc/wf) ~ log(y) + log(wl/wf) + log(wk/wf),
  data        = utility,
  S           = -1,
  groupType   = "sfalcmcross",
  lcmClasses  = 2,
  metaMethod  = "sfa",
  sfaApproach = "huang"
)
#> Initialization: SFA + halfnormal - normal distributions...
#> LCM 2 Classes Estimation...
#> Warning: hessian is singular for 'qr.solve' switching to 'ginv'
summary(meta_lcm_huang)
#> ============================================================ 
#> Stochastic Metafrontier Analysis
#> Metafrontier method: SFA Metafrontier [Huang et al. (2014), two-stage] 
#> Stochastic Cost Frontier, e = v + u 
#> SFA approach       : huang 
#> Group approach     : Latent Class Stochastic Frontier Analysis 
#> Group estimator    : sfalcmcross 
#> Group optim solver : BFGS maximization 
#>   (Pooled LCM - latent classes used as groups)
#> Groups ( 2 ): Class_1, Class_2 
#> Total observations : 791 
#> Distribution       : hnormal 
#> ============================================================ 
#> 
#> ------------------------------------------------------------ 
#> Pooled LCM (2 classes) on all data (N = 791)  Log-likelihood: 61.35325
#> ------------------------------------------------------------ 
#> 
#>   -- Latent Class 1 --
#>   Frontier:
#>             Coefficient  Std. Error z value  Pr(>|z|)    
#> (Intercept) -1.4472e+00  3.9123e-05  -36992 < 2.2e-16 ***
#> log(y)       8.4541e-01  2.3364e-06  361846 < 2.2e-16 ***
#> log(wl/wf)   3.5408e-01  4.4754e-06   79118 < 2.2e-16 ***
#> log(wk/wf)   4.2883e-01  1.3682e-05   31343 < 2.2e-16 ***
#>   Var(u):
#>                Coefficient  Std. Error   z value  Pr(>|z|)    
#> Zu_(Intercept) -1.7658e+00  5.8803e-08 -30029863 < 2.2e-16 ***
#>   Var(v):
#>                Coefficient  Std. Error    z value  Pr(>|z|)    
#> Zv_(Intercept) -3.8759e+01  3.2680e-13 -1.186e+14 < 2.2e-16 ***
#>   Sigma_u=0.4136  Sigma_v=0.0000  Sigma=0.4136  Gamma=1.0000  Lambda=107911523.6712
#> 
#>   -- Latent Class 2 --
#>   Frontier:
#>             Coefficient  Std. Error   z value  Pr(>|z|)    
#> (Intercept) -2.0490e+00  2.5608e-05 -80011.07 < 2.2e-16 ***
#> log(y)       1.0079e+00  4.1082e-04   2453.37 < 2.2e-16 ***
#> log(wl/wf)  -2.5916e-02  6.3375e-05   -408.93 < 2.2e-16 ***
#> log(wk/wf)   8.8450e-01  6.9315e-05  12760.74 < 2.2e-16 ***
#>   Var(u):
#>                Coefficient  Std. Error  z value  Pr(>|z|)    
#> Zu_(Intercept) -3.0117e+00  1.1348e-06 -2653956 < 2.2e-16 ***
#>   Var(v):
#>                Coefficient  Std. Error  z value  Pr(>|z|)    
#> Zv_(Intercept) -4.9663e+00  5.3865e-07 -9219998 < 2.2e-16 ***
#>   Sigma_u=0.2218  Sigma_v=0.0835  Sigma=0.2370  Gamma=0.8759  Lambda=2.6573
#> 
#>   -- Class Membership (logit) --
#>                 Coefficient  Std. Error  z value  Pr(>|z|)    
#> Cl1_(Intercept) -6.0163e-01  5.0453e-07 -1192458 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> Log likelihood status: successful convergence  
#> 
#> ------------------------------------------------------------ 
#> Metafrontier Coefficients (sfa):
#> Meta-optim solver  : BFGS maximization 
#>               Estimate Std. Error  z value  Pr(>|z|)    
#> (Intercept) -2.2495079  0.0686629 -32.7616 < 2.2e-16 ***
#> log(y)       0.9909918  0.0024687 401.4291 < 2.2e-16 ***
#> log(wl/wf)   0.0399586  0.0106166   3.7638 0.0001674 ***
#> log(wk/wf)   0.7890986  0.0143801  54.8745 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#>   Meta-frontier model details:
#> -------------------------------------------------------------------------------- 
#> Normal-Half Normal SF Model 
#> Dependent Variable:                                          group_fitted_values 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                          58 
#> Log likelihood value:                                                  759.62892 
#> Log likelihood gradient norm:                                        2.06059e-03 
#> Estimation based on:                                         N =  791 and K =  6 
#> Inf. Cr:                                        AIC  =  -1507.3 AIC/N  =  -1.906 
#>                                                 BIC  =  -1479.2 BIC/N  =  -1.870 
#>                                                 HQIC =  -1496.5 HQIC/N =  -1.892 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.00068 
#>            Sigma(v)           =                                          0.00068 
#>            Sigma-squared(u)   =                                          0.02713 
#>            Sigma(u)           =                                          0.02713 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          0.16676 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.97554 
#> Lambda = sigma(u)/sigma(v)    =                                          6.31586 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.93546 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         0.13141 
#> Average efficiency E[exp(-ui)] =                                         0.88105 
#> -------------------------------------------------------------------------------- 
#> Stochastic Cost Frontier, e = v + u 
#> -----[ Tests vs. No Inefficiency ]-----
#> Likelihood Ratio Test of Inefficiency
#> Deg. freedom for inefficiency model                                            1 
#> Log Likelihood for OLS Log(H0) =                                       588.58962 
#> LR statistic:  
#> Chisq = 2*[LogL(H0)-LogL(H1)]  =                                       342.07861 
#> Kodde-Palm C*:       95%: 2.70554                                   99%: 5.41189 
#> Coelli (1995) skewness test on OLS residuals
#> M3T: z                         =                                        10.23625 
#> M3T: p.value                   =                                         0.00000 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error  z value  Pr(>|z|)    
#> (Intercept)       -2.24951    0.06866 -32.7616 < 2.2e-16 ***
#> .X2                0.99099    0.00247 401.4291 < 2.2e-16 ***
#> .X3                0.03996    0.01062   3.7638 0.0001674 ***
#> .X4                0.78910    0.01438  54.8745 < 2.2e-16 ***
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zu_(Intercept)    -3.60721    0.05642 -63.932 < 2.2e-16 ***
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> Zv_(Intercept)    -7.29334    0.12966  -56.25 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 17:43 
#> Log likelihood status: successful convergence  
#> --------------------------------------------------------------------------------  
#> Log likelihood status: successful convergence  
#> 
#> ------------------------------------------------------------ 
#> Efficiency Statistics (group means):
#> ------------------------------------------------------------ 
#>         N_obs N_valid TE_group_BC TE_group_JLMS TE_meta_BC TE_meta_JLMS  MTR_BC
#> Class_1   202     202     0.68291       0.68291    0.51861      0.51845 0.76693
#> Class_2   589     589     0.85142       0.84951    0.80511      0.80308 0.94559
#>         MTR_JLMS
#> Class_1  0.76669
#> Class_2  0.94532
#> 
#> Overall:
#> TE_group_BC=0.7672  TE_group_JLMS=0.7662
#> TE_meta_BC=0.6619   TE_meta_JLMS=0.6608
#> MTR_BC=0.8563     MTR_JLMS=0.8560
#> 
#> ------------------------------------------------------------ 
#> Posterior Class Membership (pooled LCM):
#> ------------------------------------------------------------ 
#>         % assigned Mean post. prob.
#> Class 1       25.5            0.354
#> Class 2       74.5            0.646
#> ------------------------------------------------------------ 
#> Total Log-likelihood: 820.9822 
#> AIC: -1603.964   BIC: -1515.172   HQIC: -1569.836 
#> ------------------------------------------------------------ 
#> Model was estimated on : Apr Fri 24, 2026 at 17:43
```

## Method 4: LCM + SFA (O’Donnell)

``` r

meta_lcm_odonnell <- smfa(
  formula     = log(tc/wf) ~ log(y) + log(wl/wf) + log(wk/wf),
  data        = utility,
  S           = -1,
  groupType   = "sfalcmcross",
  lcmClasses  = 2,
  metaMethod  = "sfa",
  sfaApproach = "ordonnell"
)
#> Initialization: SFA + halfnormal - normal distributions...
#> LCM 2 Classes Estimation...
#> Warning: hessian is singular for 'qr.solve' switching to 'ginv'
#> Warning: hessian is singular for 'qr.solve' switching to 'ginv'
summary(meta_lcm_odonnell)
#> Warning: 761 MTR value(s) > 1 detected in O'Donnell SFA approach. This
#> typically occurs when the second-stage SFA estimates near-zero inefficiency
#> (sigma_u -> 0), causing TE_meta ~= 1 and MTR = TE_meta/TE_group > 1. Consider
#> using metaMethod='lp' or sfaApproach='huang' instead.
#> ============================================================ 
#> Stochastic Metafrontier Analysis
#> Metafrontier method: SFA Metafrontier [O'Donnell et al. (2008), envelope] 
#> Stochastic Cost Frontier, e = v + u 
#> SFA approach       : ordonnell 
#> Group approach     : Latent Class Stochastic Frontier Analysis 
#> Group estimator    : sfalcmcross 
#> Group optim solver : BFGS maximization 
#>   (Pooled LCM - latent classes used as groups)
#> Groups ( 2 ): Class_1, Class_2 
#> Total observations : 791 
#> Distribution       : hnormal 
#> ============================================================ 
#> 
#> ------------------------------------------------------------ 
#> Pooled LCM (2 classes) on all data (N = 791)  Log-likelihood: 61.35325
#> ------------------------------------------------------------ 
#> 
#>   -- Latent Class 1 --
#>   Frontier:
#>             Coefficient  Std. Error z value  Pr(>|z|)    
#> (Intercept) -1.4472e+00  3.9123e-05  -36992 < 2.2e-16 ***
#> log(y)       8.4541e-01  2.3364e-06  361846 < 2.2e-16 ***
#> log(wl/wf)   3.5408e-01  4.4754e-06   79118 < 2.2e-16 ***
#> log(wk/wf)   4.2883e-01  1.3682e-05   31343 < 2.2e-16 ***
#>   Var(u):
#>                Coefficient  Std. Error   z value  Pr(>|z|)    
#> Zu_(Intercept) -1.7658e+00  5.8803e-08 -30029863 < 2.2e-16 ***
#>   Var(v):
#>                Coefficient  Std. Error    z value  Pr(>|z|)    
#> Zv_(Intercept) -3.8759e+01  3.2680e-13 -1.186e+14 < 2.2e-16 ***
#>   Sigma_u=0.4136  Sigma_v=0.0000  Sigma=0.4136  Gamma=1.0000  Lambda=107911523.6712
#> 
#>   -- Latent Class 2 --
#>   Frontier:
#>             Coefficient  Std. Error   z value  Pr(>|z|)    
#> (Intercept) -2.0490e+00  2.5608e-05 -80011.07 < 2.2e-16 ***
#> log(y)       1.0079e+00  4.1082e-04   2453.37 < 2.2e-16 ***
#> log(wl/wf)  -2.5916e-02  6.3375e-05   -408.93 < 2.2e-16 ***
#> log(wk/wf)   8.8450e-01  6.9315e-05  12760.74 < 2.2e-16 ***
#>   Var(u):
#>                Coefficient  Std. Error  z value  Pr(>|z|)    
#> Zu_(Intercept) -3.0117e+00  1.1348e-06 -2653956 < 2.2e-16 ***
#>   Var(v):
#>                Coefficient  Std. Error  z value  Pr(>|z|)    
#> Zv_(Intercept) -4.9663e+00  5.3865e-07 -9219998 < 2.2e-16 ***
#>   Sigma_u=0.2218  Sigma_v=0.0835  Sigma=0.2370  Gamma=0.8759  Lambda=2.6573
#> 
#>   -- Class Membership (logit) --
#>                 Coefficient  Std. Error  z value  Pr(>|z|)    
#> Cl1_(Intercept) -6.0163e-01  5.0453e-07 -1192458 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> Log likelihood status: successful convergence  
#> 
#> ------------------------------------------------------------ 
#> Metafrontier Coefficients (sfa):
#> Meta-optim solver  : BFGS maximization 
#>                Estimate  Std. Error z value  Pr(>|z|)    
#> (Intercept) -1.4655e+00  1.4682e-05  -99822 < 2.2e-16 ***
#> log(y)       8.5052e-01  2.0413e-06  416655 < 2.2e-16 ***
#> log(wl/wf)   3.4196e-01  8.2433e-06   41483 < 2.2e-16 ***
#> log(wk/wf)   4.4325e-01  1.1455e-05   38693 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#>   Meta-frontier model details:
#> -------------------------------------------------------------------------------- 
#> Normal-Half Normal SF Model 
#> Dependent Variable:                                                  lp_envelope 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                        1139 
#> Log likelihood value:                                                 1949.16740 
#> Log likelihood gradient norm:                                        3.76958e+02 
#> Estimation based on:                                         N =  791 and K =  6 
#> Inf. Cr:                                        AIC  =  -3886.3 AIC/N  =  -4.913 
#>                                                 BIC  =  -3858.3 BIC/N  =  -4.878 
#>                                                 HQIC =  -3875.6 HQIC/N =  -4.900 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.00000 
#>            Sigma(v)           =                                          0.00000 
#>            Sigma-squared(u)   =                                          0.00170 
#>            Sigma(u)           =                                          0.00170 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          0.04117 
#> Gamma = sigma(u)^2/sigma^2    =                                          1.00000 
#> Lambda = sigma(u)/sigma(v)    =                                    5005571.02335 
#> Var[u]/{Var[u]+Var[v]}        =                                          1.00000 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         0.03285 
#> Average efficiency E[exp(-ui)] =                                         0.96798 
#> -------------------------------------------------------------------------------- 
#> Stochastic Cost Frontier, e = v + u 
#> -----[ Tests vs. No Inefficiency ]-----
#> Likelihood Ratio Test of Inefficiency
#> Deg. freedom for inefficiency model                                            1 
#> Log Likelihood for OLS Log(H0) =                                      1595.35974 
#> LR statistic:  
#> Chisq = 2*[LogL(H0)-LogL(H1)]  =                                       707.61532 
#> Kodde-Palm C*:       95%: 2.70554                                   99%: 5.41189 
#> Coelli (1995) skewness test on OLS residuals
#> M3T: z                         =                                        30.29500 
#> M3T: p.value                   =                                         0.00000 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> (Intercept)       -1.46554    0.00001  -99822 < 2.2e-16 ***
#> .X2                0.85052    0.00000  416655 < 2.2e-16 ***
#> .X3                0.34196    0.00001   41483 < 2.2e-16 ***
#> .X4                0.44325    0.00001   38693 < 2.2e-16 ***
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error     z value  Pr(>|z|)    
#> Zu_(Intercept)     -6.3799     0.0000 -1.8575e+13 < 2.2e-16 ***
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error     z value  Pr(>|z|)    
#> Zv_(Intercept)     -37.232      0.000 -4.2838e+13 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 17:43 
#> Log likelihood status: successful convergence  
#> --------------------------------------------------------------------------------  
#> Log likelihood status: successful convergence  
#> 
#> ------------------------------------------------------------ 
#> Efficiency Statistics (group means):
#> ------------------------------------------------------------ 
#>         N_obs N_valid TE_group_BC TE_group_JLMS TE_meta_BC TE_meta_JLMS  MTR_BC
#> Class_1   202     202     0.68291       0.68291    0.98665      0.98665 1.53705
#> Class_2   589     589     0.85142       0.84951    0.98091      0.98091 1.16150
#>         MTR_JLMS
#> Class_1  1.53705
#> Class_2  1.16424
#> 
#> Overall:
#> TE_group_BC=0.7672  TE_group_JLMS=0.7662
#> TE_meta_BC=0.9838   TE_meta_JLMS=0.9838
#> MTR_BC=1.3493     MTR_JLMS=1.3506
#> 
#> ------------------------------------------------------------ 
#> Posterior Class Membership (pooled LCM):
#> ------------------------------------------------------------ 
#>         % assigned Mean post. prob.
#> Class 1       25.5            0.354
#> Class 2       74.5            0.646
#> ------------------------------------------------------------ 
#> Total Log-likelihood: 2010.521 
#> AIC: -3983.041   BIC: -3894.249   HQIC: -3948.913 
#> ------------------------------------------------------------ 
#> Model was estimated on : Apr Fri 24, 2026 at 17:43
```

## Choosing the Number of Classes

The number of latent classes (`lcmClasses`) should be guided by economic
theory and information criteria. You can compare models with different
numbers of classes:

``` r

meta_lcm_2 <- smfa(
  formula    = log(tc/wf) ~ log(y) + log(wl/wf) + log(wk/wf),
  data       = utility, S = -1,
  groupType  = "sfalcmcross", lcmClasses = 2, metaMethod = "lp"
)
#> Initialization: SFA + halfnormal - normal distributions...
#> LCM 2 Classes Estimation...
#> Warning: hessian is singular for 'qr.solve' switching to 'ginv'
meta_lcm_3 <- smfa(
  formula    = log(tc/wf) ~ log(y) + log(wl/wf) + log(wk/wf),
  data       = utility, S = -1,
  groupType  = "sfalcmcross", lcmClasses = 3, metaMethod = "lp"
)
#> Initialization: SFA + halfnormal - normal distributions...
#> LCM 3 Classes Estimation...
#> Warning: hessian is singular for 'qr.solve' switching to 'ginv'
# Compare information criteria
ic(meta_lcm_2)
#>         AIC       BIC      HQIC
#> 1 -96.70649 -35.95362 -73.35552
ic(meta_lcm_3)
#>         AIC       BIC      HQIC
#> 1 -116.0641 -22.59815 -80.13954
```

Prefer the model with the lower AIC/BIC.

## Extracting Efficiencies and Posterior Probabilities

For LCM models,
[`efficiencies()`](https://SulmanOlieko.github.io/smfa/reference/efficiencies.md)
returns extra columns for posterior class membership probabilities,
which can be used for robustness checks or classification:

``` r

eff_lcm <- efficiencies(meta_lcm_lp)
head(eff_lcm)
#>   id Group_c        u_g TE_group_JLMS TE_group_BC TE_group_BC_reciprocal
#> 1  1       2 0.15842759     0.8534848   0.8557686               1.174838
#> 2  2       2 0.11418774     0.8920905   0.8939942               1.123406
#> 3  3       2 0.08540291     0.9181423   0.9196136               1.090950
#> 4  4       2 0.08020641     0.9229258   0.9243036               1.085175
#> 5  5       2 0.05774132     0.9438941   0.9448245               1.060519
#> 6  6       2 0.08181796     0.9214397   0.9228469               1.086964
#>   PosteriorProb_c PosteriorProb_c1 PriorProb_c1       u_c1   teBC_c1
#> 1       0.7249992        0.2750008    0.3539715 0.19428008 0.8234272
#> 2       0.7334016        0.2665984    0.3539715 0.16086081 0.8514106
#> 3       0.7039780        0.2960220    0.3539715 0.10301513 0.9021133
#> 4       0.6909119        0.3090881    0.3539715 0.07745685 0.9254670
#> 5       0.5808752        0.4191248    0.3539715 0.05410185 0.9473356
#> 6       0.6927818        0.3072182    0.3539715 0.05781993 0.9438199
#>   teBC_reciprocal_c1 PosteriorProb_c2 PriorProb_c2       u_c2   teBC_c2
#> 1           1.214436        0.7249992    0.6460285 0.15842759 0.8557686
#> 2           1.174521        0.7334016    0.6460285 0.11418774 0.8939942
#> 3           1.108508        0.7039780    0.6460285 0.08540291 0.9196136
#> 4           1.080536        0.6909119    0.6460285 0.08020641 0.9243036
#> 5           1.055592        0.5808752    0.6460285 0.05774132 0.9448245
#> 6           1.059524        0.6927818    0.6460285 0.08181796 0.9228469
#>   teBC_reciprocal_c2 ineff_c1   ineff_c2 effBC_c1  effBC_c2 ReffBC_c1 ReffBC_c2
#> 1           1.174838       NA 0.15842759       NA 0.8557686        NA  1.174838
#> 2           1.123406       NA 0.11418774       NA 0.8939942        NA  1.123406
#> 3           1.090950       NA 0.08540291       NA 0.9196136        NA  1.090950
#> 4           1.085175       NA 0.08020641       NA 0.9243036        NA  1.085175
#> 5           1.060519       NA 0.05774132       NA 0.9448245        NA  1.060519
#> 6           1.086964       NA 0.08181796       NA 0.9228469        NA  1.086964
#>       u_meta TE_meta_JLMS TE_meta_BC MTR_JLMS MTR_BC
#> 1 0.15842759    0.8534848  0.8557686        1      1
#> 2 0.11418774    0.8920905  0.8939942        1      1
#> 3 0.08540291    0.9181423  0.9196136        1      1
#> 4 0.08020641    0.9229258  0.9243036        1      1
#> 5 0.05774132    0.9438941  0.9448245        1      1
#> 6 0.08181796    0.9214397  0.9228469        1      1

# Key LCM-specific columns:
# Group_c          — most likely class assignment
# PosteriorProb_c  — posterior probability of assigned class
# PosteriorProb_c1 — posterior probability of Class 1
# PosteriorProb_c2 — posterior probability of Class 2
```

### Class membership summary

``` r

# Proportion assigned to each class and mean posterior probability
with(eff_lcm, table(Group_c)) / nrow(eff_lcm) * 100   # % in each class
#> Group_c
#>        1        2 
#> 25.53729 74.46271
```

## Key Difference from `sfacross`

| Feature | `sfacross` | `sfalcmcross` |
|----|----|----|
| Group variable | Required | Not required |
| Group estimation | Separate SFA per group | Pooled LCM simultaneously |
| Output | Group-level SFA summaries | Pooled LCM summary with class-specific parameters |
| Extra efficiency columns | Confidence bounds | Posterior class probabilities |
