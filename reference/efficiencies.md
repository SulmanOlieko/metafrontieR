# Compute efficiency estimates and metatechnology ratios from stochastic metafrontier models

`efficiencies` returns all efficiency estimates and metatechnology ratio
(MTR) measures for objects of class `"sfametafrontier"` returned by
[`sfametafrontier`](https://SulmanOlieko.github.io/metafrontieR/reference/sfametafrontier.md).
The function supports models estimated via linear programming (LP),
quadratic programming (QP), and stochastic second-stage SFA (`"sfa"`),
and for each observation it computes the group-specific technical
efficiency, the metafrontier technical efficiency, and the
metatechnology ratio (MTR), using both the Jondrow, Lovell, Materov, and
Schmidt (1982) (JLMS) and the Battese and Coelli (1988) (BC) estimators.
Additional model-specific columns are returned depending on `groupType`.

## Usage

``` r
# S3 method for class 'sfametafrontier'
efficiencies(object, level = 0.95, newData = NULL, ...)
```

## Arguments

- object:

  An object of class `"sfametafrontier"` returned by
  [`sfametafrontier`](https://SulmanOlieko.github.io/metafrontieR/reference/sfametafrontier.md).

- level:

  A number strictly between 0 and 0.9999 specifying the nominal coverage
  for (in-)efficiency confidence intervals. Default `0.95`. This
  argument is passed to the underlying `efficiencies` method of the
  group-level model (class `"sfacross"`, `"sfalcmcross"`, or
  `"sfaselectioncross"`).

- newData:

  Optional data frame for out-of-sample prediction of efficiency
  estimates. When `NULL` (default), efficiencies are computed for the
  observations used in the estimation.

- ...:

  Further arguments (currently ignored).

## Value

A data frame with one row per observation (in the original row order),
containing the following columns. The exact set of columns depends on
`groupType`:

**Columns present for all model types:**

- `id`:

  Observation identifier. Contains the row name of each observation as
  it appeared in the data supplied to
  [`sfametafrontier`](https://SulmanOlieko.github.io/metafrontieR/reference/sfametafrontier.md).
  When the data frame has no explicit row names, sequential integers
  (`"1"`, `"2"`, ...) are used. This column is always the first column
  of the returned data frame.

- `<group>` or `Group_c`:

  The technology group identifier for each observation. For
  `groupType = "sfacross"` and `"sfaselectioncross"`, this column takes
  the name of the user-supplied `group` variable and contains the group
  label to which each observation belongs. For
  `groupType = "sfalcmcross"`, it is named `Group_c` and contains the
  integer index of the latent class assigned by the maximum posterior
  probability criterion.

- `u_g`:

  Group-specific conditional mean of the inefficiency term, computed as
  \\E\[u_i \mid \varepsilon_i\]\\. This is the JLMS (Jondrow, Lovell,
  Materov, and Schmidt, 1982) point estimate of the inefficiency at the
  group-frontier level. For `groupType = "sfaselectioncross"`, `u_g` is
  `NA` for observations not selected into the sample (selection
  indicator = 0).

- `TE_group_JLMS`:

  Group-specific technical efficiency using the Jondrow, Lovell,
  Materov, and Schmidt (1982) estimator: \\TE^g_i = \exp(-E\[u_i \mid
  \varepsilon_i\])\\. For `groupType = "sfaselectioncross"`, `NA` for
  non-selected observations.

- `TE_group_BC`:

  Group-specific technical efficiency using the Battese and
  Coelli (1988) estimator: \\TE^g_i = E\[\exp(-u_i) \mid
  \varepsilon_i\]\\. For `groupType = "sfaselectioncross"`, `NA` for
  non-selected observations.

- `TE_group_BC_reciprocal`:

  Reciprocal of the Battese and Coelli (1988) group technical
  efficiency: \\1 / TE^{g,BC}\_i\\. For production frontiers this equals
  the cost-efficiency index implied by the BC estimator. Present for all
  three model types. For `groupType = "sfaselectioncross"`, `NA` for
  non-selected observations.

- `u_meta`:

  Metafrontier inefficiency, measuring the technology-gap component
  \\U_i \ge 0\\ that separates the group frontier from the global
  metafrontier. Computed from the second-stage SFA when
  `metaMethod = "sfa"`, or derived from the LP/QP gap as \\U_i = \max\\S
  \cdot (\ln \hat{y}^\*\_i - \ln \hat{y}^g_i), 0\\\\ when
  `metaMethod = "lp"` or `"qp"`.

- `TE_meta_JLMS`:

  Metafrontier technical efficiency based on the JLMS group efficiency:
  \\TE^\*\_{JLMS,i} = TE^g\_{JLMS,i} \times MTR\_{JLMS,i}\\.

- `TE_meta_BC`:

  Metafrontier technical efficiency based on the Battese and
  Coelli (1988) group efficiency: \\TE^\*\_{BC,i} = TE^g\_{BC,i} \times
  MTR\_{BC,i}\\.

- `MTR_JLMS`:

  Metatechnology ratio computed using the JLMS group efficiency:
  \\MTR\_{JLMS,i} = TE^\*\_{JLMS,i} / TE^g\_{JLMS,i} = \exp(-U_i)\\.
  Values range from 0 to 1. A value of 1 indicates that the group
  frontier for this observation coincides with the metafrontier.

- `MTR_BC`:

  Metatechnology ratio computed using the Battese and Coelli (1988)
  group efficiency: \\MTR\_{BC,i} = TE^\*\_{BC,i} / TE^g\_{BC,i} =
  \exp(-U_i)\\.

**Additional columns for `groupType = "sfacross"` only:**

- `uLB_g`, `uUB_g`:

  Lower and upper bounds of the `level` confidence interval for the
  conditional mean inefficiency `u_g`, constructed using the asymptotic
  distribution of the conditional estimator. Available for distributions
  with closed-form expressions for the confidence bounds, such as
  `udist = "hnormal"` and `udist = "tnormal"`.

- `m_g`:

  Mode of the conditional distribution of the one-sided error term \\u_i
  \mid \varepsilon_i\\. This is an alternative point estimate of
  inefficiency. Available for distributions for which the conditional
  mode has a closed-form expression.

- `TE_group_mode`:

  Group-specific technical efficiency evaluated at the conditional mode:
  \\TE^g\_{\mathrm{mode},i} = \exp(-m_i)\\.

- `teBCLB_g`, `teBCUB_g`:

  Lower and upper bounds of the `level` confidence interval for the
  Battese and Coelli (1988) group technical efficiency `TE_group_BC`.
  Constructed from the corresponding bounds on the conditional
  distribution of \\\exp(-u_i \mid \varepsilon_i)\\.

**Additional columns for `groupType = "sfalcmcross"` only:**

- `PosteriorProb_c`:

  Posterior probability that observation \\i\\ belongs to its assigned
  class (the one with the highest posterior probability). Computed via
  Bayes' rule as \\P(j \mid y_i, x_i) \propto \pi(i,j) \\ P(i \mid j)\\,
  where \\\pi(i,j)\\ is the prior class probability and \\P(i \mid j)\\
  is the class-conditional likelihood.

- `PosteriorProb_cJ` (per class, \\J = 1, 2, \ldots\\):

  Posterior probability of belonging to latent class \\J\\, computed via
  Bayes' rule for each class separately. One column is produced for each
  estimated class.

- `PriorProb_cJ` (per class, \\J = 1, 2, \ldots\\):

  Prior (unconditional) probability of belonging to latent class \\J\\,
  given by the logistic specification \\\pi(i,J) =
  \exp(\bm{\theta}\_J'\mathbf{Z}\_{hi}) / \sum_m
  \exp(\bm{\theta}\_m'\mathbf{Z}\_{hi})\\.

- `u_cJ` (per class, \\J = 1, 2, \ldots\\):

  Conditional mean of the inefficiency term for class \\J\\: \\E\[u\_{i
  \mid J} \mid \varepsilon\_{i \mid J}\]\\.

- `teBC_cJ` (per class, \\J = 1, 2, \ldots\\):

  Battese and Coelli (1988) technical efficiency for class \\J\\:
  \\E\[\exp(-u\_{i \mid J}) \mid \varepsilon\_{i \mid J}\]\\.

- `teBC_reciprocal_cJ` (per class, \\J = 1, 2, \ldots\\):

  Reciprocal of the class-\\J\\ Battese and Coelli (1988) efficiency:
  \\1/TE^{BC}\_{i \mid J}\\.

- `ineff_cJ` (per class, \\J = 1, 2, \ldots\\):

  Inefficiency estimate for the observation restricted to class \\J\\
  (i.e. the value assigned to the class to which the observation *does*
  belong; `NA` for other classes).

- `effBC_cJ` (per class, \\J = 1, 2, \ldots\\):

  Battese and Coelli (1988) efficiency for the observation's assigned
  class; `NA` for non-assigned classes.

- `ReffBC_cJ` (per class, \\J = 1, 2, \ldots\\):

  Reciprocal Battese and Coelli (1988) efficiency for the observation's
  assigned class; `NA` for non-assigned classes.

## Details

### Group-specific efficiency estimates

For each group, the group-level frontier model is estimated by
maximising the log-likelihood using the distribution specified by
`udist` in
[`sfametafrontier`](https://SulmanOlieko.github.io/metafrontieR/reference/sfametafrontier.md).
Given the estimated composite error \\\varepsilon_i = v_i - Su_i\\, the
conditional distribution of \\u_i \mid \varepsilon_i\\ is used to
compute:

- the JLMS estimator (Jondrow, Lovell, Materov, and Schmidt, 1982):
  \\\hat{u}\_i = E\[u_i \mid \varepsilon_i\]\\, and \\TE^g\_{JLMS,i} =
  \exp(-\hat{u}\_i)\\;

- the BC estimator (Battese and Coelli, 1988): \\TE^g\_{BC,i} =
  E\[\exp(-u_i) \mid \varepsilon_i\]\\;

- the mode estimator: \\m_i = \mathrm{mode}\[u_i \mid \varepsilon_i\]\\,
  and \\TE^g\_{\mathrm{mode},i} = \exp(-m_i)\\;

- confidence bounds on \\u_i\\ and \\TE^g\_{BC,i}\\ at the nominal level
  `level`.

For `groupType = "sfaselectioncross"`, all estimates are `NA` for
observations not selected into the sample (binary selection indicator
equal to 0). For `groupType = "sfalcmcross"`, the composite efficiencies
`u_g`, `TE_group_JLMS`, and `TE_group_BC` are computed using the
posterior-probability-weighted class assignments.

### Metatechnology ratio and metafrontier efficiency

The MTR measures how far the group frontier lies below the metafrontier
for each observation. Let \\\ln \hat{y}^g_i\\ be the group-specific
fitted frontier value and \\\ln \hat{y}^\*\_i\\ the metafrontier fitted
value.

- For `metaMethod = "lp"` or `"qp"` (Battese, Rao, and O'Donnell, 2004):
  \$\$MTR_i = \exp\\\bigl( -\max\\\bigl\\S \cdot (\ln \hat{y}^\*\_i -
  \ln \hat{y}^g_i),\\ 0\bigr\\ \bigr)\$\$ where \\S = 1\\ for
  production/profit frontiers and \\S = -1\\ for cost frontiers. The
  technology gap \\U_i = \max\\S \cdot (\ln \hat{y}^\*\_i - \ln
  \hat{y}^g_i), 0\\\\ is stored in `u_meta`.

- For `metaMethod = "sfa"` with `sfaApproach = "huang"` (Huang, Huang,
  and Liu, 2014): \$\$MTR_i = TE^\*\_i = \exp(-U_i)\$\$ where \\U_i\\ is
  the one-sided error term from the second-stage SFA.

- For `metaMethod = "sfa"` with `sfaApproach = "ordonnell"` (O'Donnell,
  Rao, and Battese, 2008): \\MTR_i = TE^{\*,\mathrm{sfa}}\_i / TE^g_i\\,
  where \\TE^{\*,\mathrm{sfa}}\_i\\ is the technical efficiency from the
  second-stage SFA fitted on the LP envelope values.

The metafrontier technical efficiency is then: \$\$TE^\*\_i = TE^g_i
\times MTR_i\$\$ computed separately for the JLMS and BC group
efficiency estimators. Both `MTR_JLMS` and `MTR_BC` are reported,
distinguishing which group-level efficiency estimator was used as the
basis.

## References

Battese, G. E., and Coelli, T. J. 1988. Prediction of firm-level
technical efficiencies with a generalized frontier production function
and panel data. *Journal of Econometrics*, **38**(3), 387–399.
<https://doi.org/10.1016/0304-4076(88)90053-X>

Battese, G. E., Rao, D. S. P., and O'Donnell, C. J. 2004. A metafrontier
production function for estimation of technical efficiencies and
technology gaps for firms operating under different technologies.
*Journal of Productivity Analysis*, **21**(1), 91–103.
<https://doi.org/10.1023/B:PROD.0000012454.06094.29>

Huang, C. J., Huang, T.-H., and Liu, N.-H. 2014. A new approach to
estimating the metafrontier production function based on a stochastic
frontier framework. *Journal of Productivity Analysis*, **42**(3),
241–254. <https://doi.org/10.1007/s11123-014-0402-2>

Jondrow, J., Lovell, C. A. K., Materov, I. S., and Schmidt, P. 1982. On
the estimation of technical inefficiency in the stochastic frontier
production function model. *Journal of Econometrics*, **19**(2-3),
233–238. <https://doi.org/10.1016/0304-4076(82)90004-5>

O'Donnell, C. J., Rao, D. S. P., and Battese, G. E. 2008. Metafrontier
frameworks for the study of firm-level efficiencies and technology
ratios. *Empirical Economics*, **34**(2), 231–255.
<https://doi.org/10.1007/s00181-007-0119-4>

Orea, L., and Kumbhakar, S. C. 2004. Efficiency measurement using a
latent class stochastic frontier model. *Empirical Economics*,
**29**(1), 169–183. <https://doi.org/10.1007/s00181-003-0184-2>

Dakpo, K. H., Desjeux, Y., and Latruffe, L. 2023. sfaR: Stochastic
Frontier Analysis using R. R package version 1.0.1.
<https://CRAN.R-project.org/package=sfaR>

## See also

[`sfametafrontier`](https://SulmanOlieko.github.io/metafrontieR/reference/sfametafrontier.md),
for the stochastic metafrontier analysis model fitting function using
cross-sectional or pooled data;
[`efficiencies`](https://rdrr.io/pkg/sfaR/man/efficiencies.html), for
the underlying group-level efficiency extractor.
