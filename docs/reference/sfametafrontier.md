# Stochastic metafrontier estimation

`sfametafrontier` estimates a stochastic metafrontier model for
cross-sectional or pooled data. The function follows the theoretical
frameworks of Battese, Rao, and O'Donnell (2004) and O'Donnell, Rao, and
Battese (2008), and additionally implements the two-stage stochastic
approach of Huang, Huang, and Liu (2014). Three types of group-level
frontier models are supported: standard stochastic frontier analysis
([`sfacross`](https://rdrr.io/pkg/sfaR/man/sfacross.html)), sample
selection stochastic frontier analysis
([`sfaselectioncross`](https://rdrr.io/pkg/sfaR/man/sfaselectioncross.html)),
and latent class stochastic frontier analysis
([`sfalcmcross`](https://rdrr.io/pkg/sfaR/man/sfalcmcross.html)).

## Usage

``` r
sfametafrontier(
  formula,
  muhet,
  uhet,
  vhet,
  thet,
  logDepVar = TRUE,
  data,
  subset,
  weights,
  wscale = TRUE,
  group = NULL,
  S = 1L,
  udist = "hnormal",
  start = NULL,
  scaling = FALSE,
  modelType = "greene10",
  groupType = "sfacross",
  metaMethod = "lp",
  sfaApproach = "ordonnell",
  selectionF = NULL,
  lcmClasses = 2L,
  whichStart = 2L,
  initAlg = "nm",
  initIter = 100L,
  lType = "ghermite",
  Nsub = 100L,
  uBound = Inf,
  intol = 1e-06,
  method = "bfgs",
  hessianType = NULL,
  simType = "halton",
  Nsim = 100L,
  prime = 2L,
  burn = 10L,
  antithetics = FALSE,
  seed = 12345L,
  itermax = 2000L,
  printInfo = FALSE,
  tol = 1e-12,
  gradtol = 1e-06,
  stepmax = 0.1,
  qac = "marquardt",
  ...
)

# S3 method for class 'sfametafrontier'
print(x, ...)
```

## Arguments

- formula:

  A symbolic description of the frontier model to be estimated, based on
  the generic function
  [`formula`](https://rdrr.io/r/stats/formula.html). For
  `groupType = "sfaselectioncross"`, this argument specifies the
  frontier (outcome) equation and must be a standard formula whose
  left-hand side is the output (or cost) variable and whose right-hand
  side contains the frontier regressors (see also `selectionF`).

- muhet:

  A one-part formula to account for heterogeneity in the mean of the
  pre-truncated normal distribution. Applicable only when
  `groupType = "sfacross"` and `udist = "tnormal"`. The variables
  specified model the conditional mean \\\mu_i =
  \bm{\omega}'\mathbf{Z}\_{\mu}\\ of the truncated normal inefficiency
  distribution (see section ‘Details’).

- uhet:

  A one-part formula to account for heteroscedasticity in the one-sided
  error variance. Applicable for all three model types. The variance of
  the inefficiency term is modelled as \\\sigma^2_u =
  \exp(\bm{\delta}'\mathbf{Z}\_u)\\, where \\\mathbf{Z}\_u\\ are the
  inefficiency drivers and \\\bm{\delta}\\ the associated coefficients
  (see section ‘Details’).

- vhet:

  A one-part formula to account for heteroscedasticity in the two-sided
  error variance. Applicable for all three model types. The variance of
  the noise term is modelled as \\\sigma^2_v =
  \exp(\bm{\phi}'\mathbf{Z}\_v)\\, where \\\mathbf{Z}\_v\\ are the
  heteroscedasticity variables and \\\bm{\phi}\\ the coefficients (see
  section ‘Details’).

- thet:

  A one-part formula to account for technological heterogeneity in the
  construction of the latent classes. Applicable only when
  `groupType = "sfalcmcross"`. The variables specified enter the logit
  formulation that determines the prior class membership probabilities
  \\\pi(i,j)\\ (see section ‘Details’).

- logDepVar:

  Logical. Informs whether the dependent variable is logged (`TRUE`) or
  not (`FALSE`). Default `TRUE`. Must match the transformation applied
  to the left-hand side of `formula`.

- data:

  A data frame containing all variables referenced in `formula`,
  `selectionF`, `muhet`, `uhet`, `vhet`, `thet`, and `group`.

- subset:

  An optional vector specifying a subset of observations to be used in
  the estimation process.

- weights:

  An optional vector of weights to be used for weighted log-likelihood
  estimation. Should be `NULL` or a numeric vector with strictly
  positive values. When `NULL` (default), all observations receive equal
  weight.

- wscale:

  Logical. When `weights` is not `NULL`, a scaling transformation is
  applied such that the weights sum to the sample size:
  \$\$w\_{\mathrm{new}} = n \times \frac{w\_{\mathrm{old}}}{\sum
  w\_{\mathrm{old}}}\$\$ Default `TRUE`. When `FALSE`, the raw weights
  are used without scaling.

- group:

  Character string. The name of the column in `data` identifying the
  technology group of each observation. The column is coerced to a
  factor internally and must have at least two unique values. When
  `groupType = "sfalcmcross"` and `group` is `NULL`, a single pooled
  latent class model is estimated and class assignments serve as groups
  (see section ‘Details’).

- S:

  Integer. Frontier orientation.

  - `S = 1` (default): production or profit frontier, \\\varepsilon_i =
    v_i - u_i\\.

  - `S = -1`: cost frontier, \\\varepsilon_i = v_i + u_i\\.

- udist:

  Character string. Distribution for the one-sided error term \\u_i \ge
  0\\. The following distributions are available for
  `groupType = "sfacross"`:

  - `"hnormal"` (default): half-normal distribution (Aigner *et al.*,
    1977; Meeusen and van den Broeck, 1977).

  - `"exponential"`: exponential distribution.

  - `"tnormal"`: truncated normal distribution (Stevenson, 1980).

  - `"rayleigh"`: Rayleigh distribution (Hajargasht, 2015).

  - `"uniform"`: uniform distribution (Li, 1996; Nguyen, 2010).

  - `"gamma"`: Gamma distribution, estimated by maximum simulated
    likelihood (Greene, 2003).

  - `"lognormal"`: log-normal distribution, estimated by maximum
    simulated likelihood (Migon and Medici, 2001; Wang and Ye, 2020).

  - `"weibull"`: Weibull distribution, estimated by maximum simulated
    likelihood (Tsionas, 2007).

  - `"genexponential"`: generalised exponential distribution
    (Papadopoulos, 2020).

  - `"tslaplace"`: truncated skewed Laplace distribution (Wang, 2012).

  For `groupType = "sfaselectioncross"` and `"sfalcmcross"`, only
  `"hnormal"` is currently supported.

- start:

  Numeric vector. Optional starting values for the maximum
  likelihood (ML) or maximum simulated likelihood (MSL) estimation of
  the group-level frontier models. When `NULL` (default), starting
  values are computed automatically. For `groupType = "sfacross"`, they
  are derived from OLS residuals. For `groupType = "sfalcmcross"`, they
  depend on `whichStart`.

- scaling:

  Logical. Applicable only when `groupType = "sfacross"` and
  `udist = "tnormal"`. When `TRUE`, the scaling property model (Wang and
  Schmidt, 2002) is estimated, whereby \\u_i = h(\mathbf{Z}\_u,
  \bm{\delta}) u^\*\_i\\ and \\u^\*\_i\\ follows a truncated normal
  distribution \\N^+(\tau, \exp(c_u))\\. Default `FALSE`.

- modelType:

  Character string. Applicable only when
  `groupType = "sfaselectioncross"`. Specifies the model used to correct
  for selection bias. Currently, only `"greene10"` (default) is
  supported, corresponding to the two-step approach of Greene (2010): a
  probit model is estimated for the selection equation, and its inverse
  Mills ratio is included as a correction term in the stochastic
  frontier second step.

- groupType:

  Character string. Type of frontier model estimated for each technology
  group. Three options are available:

  - `"sfacross"` (default): standard cross-sectional stochastic frontier
    analysis ([`sfacross`](https://rdrr.io/pkg/sfaR/man/sfacross.html)).
    Groups are defined by the `group` variable. All 10 distributions for
    `udist` are supported, along with heteroscedasticity in both error
    components (`uhet`, `vhet`), heterogeneity in the truncated mean
    (`muhet`), and the scaling property.

  - `"sfaselectioncross"`: sample selection stochastic frontier analysis
    ([`sfaselectioncross`](https://rdrr.io/pkg/sfaR/man/sfaselectioncross.html)).
    Corrects for sample selection bias via the generalised Heckman
    approach (Greene, 2010). Requires `selectionF`. Only observations
    for which the selection indicator equals one enter the frontier and
    metafrontier; efficiency estimates for non-selected observations are
    `NA`. Only `udist = "hnormal"` is supported.

  - `"sfalcmcross"`: latent class stochastic frontier analysis
    ([`sfalcmcross`](https://rdrr.io/pkg/sfaR/man/sfalcmcross.html)).
    Estimates a finite mixture of frontier models with the number of
    classes determined by `lcmClasses`. When `group` is supplied, a
    separate latent class model is estimated per group-stratum and
    combined for the metafrontier. When `group` is omitted, a single
    pooled model is estimated and class assignments serve as technology
    groups. Supports `thet` for class-membership covariates and `uhet`,
    `vhet` for within-class heteroscedasticity. Only `udist = "hnormal"`
    is supported.

- metaMethod:

  Character string. Method for estimating the global metafrontier that
  envelopes all group frontiers. Three options are available:

  - `"lp"` (default): deterministic linear programming envelope. Finds
    the parameter vector \\\bm{\beta}^\*\\ minimising \\\sum_i \|\ln
    \hat{f}(x_i, \bm{\beta}^\*) - \ln \hat{f}(x_i,
    \hat{\bm{\beta}}\_{(g)})\|\\ subject to \\\ln \hat{f}(x_i,
    \bm{\beta}^\*) \ge \ln \hat{f}(x_i, \hat{\bm{\beta}}\_{(g)})\\ for
    all observations and all groups (Battese *et al.*, 2004).

  - `"qp"`: deterministic quadratic programming envelope. Minimises the
    sum of squared deviations under the same envelope constraint.

  - `"sfa"`: stochastic metafrontier estimated by a second-stage pooled
    SFA. The specific construction of the dependent variable is
    determined by `sfaApproach`.

- sfaApproach:

  Character string. Applicable only when `metaMethod = "sfa"`.
  Determines how the second-stage SFA is constructed:

  - `"ordonnell"` (default): The LP envelope of the group frontier
    predicted values is re-estimated with a stochastic frontier,
    following O'Donnell, Rao, and Battese (2008). The second-stage SFA
    directly targets the global technology envelope.

  - `"huang"`: the group-specific fitted frontier value \\\ln
    \hat{y}^g_i\\ for each observation is used as the dependent variable
    in a pooled cross-sectional SFA (Huang, Huang, and Liu, 2014). The
    technology gap \\U_i \ge 0\\ and second-stage noise \\V_i\\ are
    estimated directly by the SFA procedure.

  - `"ordonnell"`: the column-wise maximum of all group-fitted frontier
    values (the deterministic LP envelope) is used as the dependent
    variable in the second-stage SFA (O'Donnell, Rao, and Battese,
    2008).

- selectionF:

  A two-sided formula specifying the sample selection equation, e.g.,
  `selected ~ z1 + z2`. The left-hand side must be a binary (0/1)
  indicator already present in `data`: `1` means the observation
  participates in the frontier and metafrontier; `0` means it is
  excluded (efficiency estimates will be `NA`). Alternatively, a named
  list of formulas, one per group level, may be supplied to allow
  group-specific selection equations. Required when
  `groupType = "sfaselectioncross"`; ignored otherwise.

- lcmClasses:

  Integer. Number of latent classes to be estimated per group when
  `groupType = "sfalcmcross"`. Must be between `2` and `5` (default
  `2`). The optimal number of classes can be selected based on
  information criteria (see
  [`ic`](https://rdrr.io/pkg/sfaR/man/ic.html)).

- whichStart:

  Integer. Strategy for obtaining starting values in the latent class
  model (`groupType = "sfalcmcross"`):

  - `1`: starting values are obtained from the method of moments.

  - `2` (default): the model is initialised by first solving a
    homoscedastic pooled cross-sectional SFA using the algorithm
    specified by `initAlg` for at most `initIter` iterations.

- initAlg:

  Character string. Optimisation algorithm used during the
  initialisation of the latent class model when `whichStart = 2`. Only
  algorithms from the `maxLik` package are supported:

  - `"nm"` (default): Nelder-Mead (see
    [`maxNM`](https://rdrr.io/pkg/maxLik/man/maxBFGS.html)).

  - `"bfgs"`: Broyden-Fletcher-Goldfarb-Shanno (see
    [`maxBFGS`](https://rdrr.io/pkg/maxLik/man/maxBFGS.html)).

  - `"bhhh"`: Berndt-Hall-Hall-Hausman (see
    [`maxBHHH`](https://rdrr.io/pkg/maxLik/man/maxNR.html)).

  - `"nr"`: Newton-Raphson (see
    [`maxNR`](https://rdrr.io/pkg/maxLik/man/maxNR.html)).

  - `"cg"`: Conjugate Gradient (see
    [`maxCG`](https://rdrr.io/pkg/maxLik/man/maxBFGS.html)).

  - `"sann"`: Simulated Annealing (see
    [`maxSANN`](https://rdrr.io/pkg/maxLik/man/maxBFGS.html)).

- initIter:

  Integer. Maximum number of iterations for the initialisation algorithm
  when `whichStart = 2` and `groupType = "sfalcmcross"`. Default `100`.

- lType:

  Character string. Specifies how the likelihood is evaluated for the
  selection model (`groupType = "sfaselectioncross"`). Five options are
  available:

  - `"ghermite"` (default): Gauss-Hermite quadrature (see
    [`gaussHermiteData`](https://rdrr.io/pkg/fastGHQuad/man/gaussHermiteData.html)).

  - `"kronrod"`: Gauss-Kronrod quadrature (see
    [`integrate`](https://rdrr.io/r/stats/integrate.html)).

  - `"hcubature"`: adaptive integration over hypercubes (see
    [`hcubature`](https://bnaras.github.io/cubature/reference/hcubature.html)).

  - `"pcubature"`: p-adaptive cubature (see
    [`pcubature`](https://bnaras.github.io/cubature/reference/hcubature.html)).

  - `"msl"`: maximum simulated likelihood (controlled by `simType`,
    `Nsim`, `prime`, `burn`, `antithetics`, and `seed`).

- Nsub:

  Integer. Number of quadrature nodes or integration subdivisions when
  `lType` is `"ghermite"`, `"kronrod"`, `"hcubature"`, or `"pcubature"`.
  Applicable only when `groupType = "sfaselectioncross"`. Default `100`.

- uBound:

  Numeric. Upper bound for the numerical integration of the inefficiency
  component when `lType` is `"kronrod"`, `"hcubature"`, or
  `"pcubature"`. For Gauss-Hermite the bound is automatically infinite.
  Applicable only when `groupType = "sfaselectioncross"`. Default `Inf`.

- intol:

  Numeric. Integration tolerance for the quadrature approaches
  `"kronrod"`, `"hcubature"`, and `"pcubature"`. Applicable only when
  `groupType = "sfaselectioncross"`. Default `1e-6`.

- method:

  Character string. Optimisation algorithm for the main ML/MSL
  estimation of each group-level frontier model. Default `"bfgs"`.
  Eleven algorithms are available:

  - `"bfgs"`: Broyden-Fletcher-Goldfarb-Shanno (see
    [`maxBFGS`](https://rdrr.io/pkg/maxLik/man/maxBFGS.html)).

  - `"bhhh"`: Berndt-Hall-Hall-Hausman (see
    [`maxBHHH`](https://rdrr.io/pkg/maxLik/man/maxNR.html)).

  - `"nr"`: Newton-Raphson (see
    [`maxNR`](https://rdrr.io/pkg/maxLik/man/maxNR.html)).

  - `"nm"`: Nelder-Mead (see
    [`maxNM`](https://rdrr.io/pkg/maxLik/man/maxBFGS.html)).

  - `"cg"`: Conjugate Gradient (see
    [`maxCG`](https://rdrr.io/pkg/maxLik/man/maxBFGS.html)).

  - `"sann"`: Simulated Annealing (see
    [`maxSANN`](https://rdrr.io/pkg/maxLik/man/maxBFGS.html)).

  - `"ucminf"`: quasi-Newton optimisation with BFGS updating of the
    inverse Hessian and soft line search (see
    [`ucminf`](https://rdrr.io/pkg/ucminf/man/ucminf.html)).

  - `"mla"`: Marquardt-Levenberg algorithm (see
    [`mla`](https://rdrr.io/pkg/marqLevAlg/man/marqLevAlg.html)).

  - `"sr1"`: Symmetric Rank 1 trust-region method (see
    [`trust.optim`](https://braunm.github.io/trustOptim/reference/trust.optim.html)).

  - `"sparse"`: trust-region method with sparse Hessian (see
    [`trust.optim`](https://braunm.github.io/trustOptim/reference/trust.optim.html)).

  - `"nlminb"`: PORT routines optimisation (see
    [`nlminb`](https://rdrr.io/r/stats/nlminb.html)).

- hessianType:

  Integer. Specifies which Hessian is returned for the group-level
  frontier estimation. The accepted values match those of the underlying
  `sfaR` function for each `groupType`:

  - For `groupType = "sfacross"`: if `1` (default), the analytic Hessian
    is returned; if `2`, the BHHH Hessian \\\mathbf{G}'\mathbf{G}\\ is
    estimated.

  - For `groupType = "sfalcmcross"`: if `1` (default), the analytic
    Hessian is returned; if `2`, the BHHH Hessian is estimated.

  - For `groupType = "sfaselectioncross"`: if `1`, the analytic Hessian
    is returned; if `2` (default), the BHHH Hessian
    \\\mathbf{G}'\mathbf{G}\\ is estimated. The BHHH default reflects
    the two-step nature of the selection estimator.

  When `NULL` (the package default), each group-level model uses the
  natural default of the corresponding `sfaR` function, ensuring that
  standard errors computed by `sfametafrontier` are identical to those
  from a standalone `sfaR` call on the same group subset.

- simType:

  Character string. Simulation method for maximum simulated likelihood
  (MSL). Applicable to `groupType = "sfacross"` when `udist` is
  `"gamma"`, `"lognormal"`, or `"weibull"`, and to
  `groupType = "sfaselectioncross"` when `lType = "msl"`:

  - `"halton"` (default): Halton quasi-random sequences.

  - `"ghalton"`: Generalised-Halton sequences.

  - `"sobol"`: Sobol low-discrepancy sequences.

  - `"uniform"`: pseudo-random uniform draws.

- Nsim:

  Integer. Number of simulation draws for MSL. Default `100`.

- prime:

  Integer. Prime number used to construct Halton or Generalised-Halton
  sequences. Default `2`.

- burn:

  Integer. Number of leading draws discarded from the Halton sequence to
  reduce serial correlation. Default `10`.

- antithetics:

  Logical. If `TRUE`, antithetic draws are added: the first `Nsim/2`
  draws are taken, and the remaining `Nsim/2` are \\1 - \text{draw}\\.
  Default `FALSE`.

- seed:

  Integer. Random seed for simulation draws, ensuring reproducibility of
  MSL estimates. Default `12345`.

- itermax:

  Integer. Maximum number of iterations for the main optimisation.
  Default `2000`. For `method = "sann"`, it is recommended to increase
  this substantially (e.g., `itermax = 20000`).

- printInfo:

  Logical. If `TRUE`, optimisation progress is printed during estimation
  of each group-level model. Default `FALSE`.

- tol:

  Numeric. Convergence tolerance. The algorithm is considered converged
  when the change in the log-likelihood between successive iterations is
  smaller than `tol` in absolute value. Default `1e-12`.

- gradtol:

  Numeric. Gradient convergence tolerance. The algorithm is considered
  converged when the Euclidean norm of the gradient is smaller than
  `gradtol`. Default `1e-6`.

- stepmax:

  Numeric. Maximum step length used by the `"ucminf"` algorithm. Default
  `0.1`.

- qac:

  Character string. Quadratic Approximation Correction for the `"bhhh"`
  and `"nr"` algorithms when the Hessian is not negative definite:

  - `"marquardt"` (default): step length is decreased while also
    shifting closer to the gradient direction.

  - `"stephalving"`: step length is halved, preserving the current
    direction.

  See [`maxBHHH`](https://rdrr.io/pkg/maxLik/man/maxNR.html) and
  [`maxNR`](https://rdrr.io/pkg/maxLik/man/maxNR.html) for details.

- ...:

  Additional arguments passed through to the second-stage SFA call when
  `metaMethod = "sfa"`.

- x:

  An object of class `"sfametafrontier"`, as returned by
  `sfametafrontier`, for use with the `print` method.

## Value

`sfametafrontier` returns an object of class `"sfametafrontier"`, which
is a list containing:

- call:

  The matched call.

- groupModels:

  A named list of fitted group-level frontier objects, one per
  technology group. Each element is of class `"sfacross"`,
  `"sfaselectioncross"`, or `"sfalcmcross"`, depending on `groupType`.

- metaSfaObj:

  The fitted metafrontier object. For `metaMethod = "sfa"`, an object of
  class `"sfacross"` from the second-stage SFA. The dependent variable
  column in `metaSfaObj$dataTable` is named according to the approach
  used: `"lp_envelope"` when `sfaApproach = "ordonnell"` (the
  column-wise maximum of all group-evaluated frontier values is the
  dependent variable) and `"group_fitted_values"` when
  `sfaApproach = "huang"` (each observation's own-group fitted frontier
  value is the dependent variable). For `metaMethod = "lp"` or `"qp"`, a
  list containing the optimisation result and the estimated envelope
  coefficients.

- metaRes:

  Estimated metafrontier coefficients (with standard errors, z-values,
  and p-values for `metaMethod = "sfa"`, or the plain coefficient vector
  for deterministic envelopes).

- formula:

  The `formula` supplied to the call.

- metaMethod:

  The metafrontier estimation method used.

- sfaApproach:

  The second-stage SFA approach; `NA` when `metaMethod` is not `"sfa"`.

- groupType:

  The type of group-level frontier model estimated.

- group:

  The name of the grouping variable.

- groups:

  Character vector of unique group labels.

- S:

  The frontier orientation (`1` or `-1`).

- dataTable:

  The data used in estimation, augmented with `.mf_yhat_group`
  (group-specific fitted frontier values) and `.mf_yhat_meta`
  (metafrontier fitted values).

- lcmNoGroup:

  Logical. `TRUE` when `groupType = "sfalcmcross"` and `group` was not
  supplied.

- lcmObj:

  When `lcmNoGroup = TRUE`, the pooled `sfalcmcross` object.

## Details

### Standard stochastic frontier (`groupType = "sfacross"`)

The stochastic frontier model is defined as: \$\$y_i = \alpha +
\mathbf{x}\_i'\bm{\beta} + v_i - Su_i\$\$ where \\y\\ is the output
(cost, revenue, or profit), \\\mathbf{x}\\ is the vector of frontier
regressors, \\u_i \ge 0\\ is the one-sided inefficiency term with
variance \\\sigma^2_u\\, and \\v_i\\ is the symmetric noise term with
variance \\\sigma^2_v\\.

Estimation is by ML for all distributions except `"gamma"`,
`"lognormal"`, and `"weibull"`, for which MSL is used with Halton,
Generalised-Halton, Sobol, or uniform draws. Antithetic draws are
available for the uniform case.

To account for heteroscedasticity, the variances are modelled as
\\\sigma^2_u = \exp(\bm{\delta}'\mathbf{Z}\_u)\\ and \\\sigma^2_v =
\exp(\bm{\phi}'\mathbf{Z}\_v)\\. For the truncated normal distribution,
heterogeneity in the pre-truncation mean is modelled as \\\mu_i =
\bm{\omega}'\mathbf{Z}\_{\mu}\\. The scaling property (Wang and Schmidt,
2002) can also be imposed for the truncated normal.

### Sample selection frontier (`groupType = "sfaselectioncross"`)

This model extends the Heckman (1979) selection framework to the
stochastic frontier setting (Greene, 2010; Dakpo *et al.*, 2021). The
selection and frontier equations are: \$\$y\_{1i}^\* =
\mathbf{Z}\_{si}'\bm{\gamma} + w_i, \quad w_i \sim \mathcal{N}(0,1)\$\$
\$\$y\_{2i}^\* = \mathbf{x}\_i'\bm{\beta} + v_i - Su_i\$\$ where
\\y\_{1i} = \mathbf{1}(y\_{1i}^\* \> 0)\\ is the binary selection
indicator and \\y\_{2i} = y\_{2i}^\*\\ is observed only when \\y\_{1i} =
1\\. Selection bias arises from \\\rho = \mathrm{Corr}(w_i, v_i) \ne
0\\. Only selected observations enter the frontier and metafrontier
estimation; efficiency estimates for non-selected observations are `NA`.

### Latent class frontier (`groupType = "sfalcmcross"`)

The latent class model (Orea and Kumbhakar, 2004) estimates a finite
mixture of \\J\\ frontier models: \$\$y_i = \alpha_j +
\mathbf{x}\_i'\bm{\beta}\_j + v\_{i\|j} - Su\_{i\|j}\$\$ The prior class
probability follows a logit specification: \$\$\pi(i,j) =
\frac{\exp(\bm{\theta}\_j'\mathbf{Z}\_{hi})}
{\sum\_{m=1}^{J}\exp(\bm{\theta}\_m'\mathbf{Z}\_{hi})}\$\$ Class
assignment is based on the maximum posterior probability computed via
Bayes' rule. When `group` is omitted, a single pooled model is estimated
and class assignments serve as technology groups.

### Metafrontier estimation

The global metafrontier \\f(x_i, \bm{\beta}^\*)\\ envelopes all group
frontiers. With LP (Battese *et al.*, 2004), \\\bm{\beta}^\*\\ minimises
\\\sum_i \|\ln \hat{f}(x_i, \bm{\beta}^\*) - \ln \hat{f}(x_i,
\hat{\bm{\beta}}\_{(g)})\|\\ subject to \\\ln \hat{f}(x_i,
\bm{\beta}^\*) \ge \ln \hat{f}(x_i, \hat{\bm{\beta}}\_{(g)})\\. QP
minimises the squared analogue. The stochastic approaches (Huang *et
al.*, 2014; O'Donnell *et al.*, 2008) treat the technology gap \\U_i\\
as a one-sided error in a second-stage SFA. Group and metafrontier
efficiencies are: \$\$TE_i^g = \exp(-u_i^g), \quad MTR_i = \exp(-U_i),
\quad TE_i^\* = TE_i^g \times MTR_i\$\$ Both Jondrow *et al.* (1982) and
Battese and Coelli (1988) estimators are provided for each measure. See
[`efficiencies`](https://SulmanOlieko.github.io/metafrontieR/reference/efficiencies.md)
for details.

## References

Aigner, D. J., Lovell, C. A. K., and Schmidt, P. 1977. Formulation and
estimation of stochastic frontier production function models. *Journal
of Econometrics*, **6**(1), 21–37.
[doi:10.1016/0304-4076(77)90052-5](https://doi.org/10.1016/0304-4076%2877%2990052-5)

Battese, G. E., and Coelli, T. J. 1988. Prediction of firm-level
technical efficiencies with a generalized frontier production function
and panel data. *Journal of Econometrics*, **38**(3), 387–399.
[doi:10.1016/0304-4076(88)90053-X](https://doi.org/10.1016/0304-4076%2888%2990053-X)

Battese, G. E., Rao, D. S. P., and O'Donnell, C. J. 2004. A metafrontier
production function for estimation of technical efficiencies and
technology gaps for firms operating under different technologies.
*Journal of Productivity Analysis*, **21**(1), 91–103.
[doi:10.1023/B:PROD.0000012454.06094.29](https://doi.org/10.1023/B%3APROD.0000012454.06094.29)

Greene, W. 2003. Simulated likelihood estimation of the normal-gamma
stochastic frontier function. *Journal of Productivity Analysis*,
**19**(2-3), 179–190.
[doi:10.1023/A:1022853416499](https://doi.org/10.1023/A%3A1022853416499)

Greene, W. 2010. A stochastic frontier model with correction for sample
selection. *Journal of Productivity Analysis*, **34**(1), 15–24.
[doi:10.1007/s11123-009-0159-1](https://doi.org/10.1007/s11123-009-0159-1)

Hajargasht, G. 2015. Stochastic frontiers with a Rayleigh distribution.
*Journal of Productivity Analysis*, **44**(2), 199–208.
[doi:10.1007/s11123-014-0417-8](https://doi.org/10.1007/s11123-014-0417-8)

Heckman, J. J. 1979. Sample selection bias as a specification error.
*Econometrica*, **47**(1), 153–161.
[doi:10.2307/1912352](https://doi.org/10.2307/1912352)

Huang, C. J., Huang, T.-H., and Liu, N.-H. 2014. A new approach to
estimating the metafrontier production function based on a stochastic
frontier framework. *Journal of Productivity Analysis*, **42**(3),
241–254.
[doi:10.1007/s11123-014-0402-2](https://doi.org/10.1007/s11123-014-0402-2)

Jondrow, J., Lovell, C. A. K., Materov, I. S., and Schmidt, P. 1982. On
the estimation of technical inefficiency in the stochastic frontier
production function model. *Journal of Econometrics*, **19**(2-3),
233–238.
[doi:10.1016/0304-4076(82)90004-5](https://doi.org/10.1016/0304-4076%2882%2990004-5)

Li, Q. 1996. Estimating a stochastic production frontier when the
adjusted error is symmetric. *Economics Letters*, **52**(3), 221–228.
[doi:10.1016/S0165-1765(96)00857-9](https://doi.org/10.1016/S0165-1765%2896%2900857-9)

Meeusen, W., and van den Broeck, J. 1977. Efficiency estimation from
Cobb-Douglas production functions with composed error. *International
Economic Review*, **18**(2), 435–444.
[doi:10.2307/2525757](https://doi.org/10.2307/2525757)

Migon, H. S., and Medici, E. 2001. Bayesian inference for generalised
exponential models. Working paper, Universidade Federal do Rio de
Janeiro.

Nguyen, N. B. 2010. Estimation of technical efficiency in stochastic
frontier analysis. PhD thesis, Bowling Green State University.

O'Donnell, C. J., Rao, D. S. P., and Battese, G. E. 2008. Metafrontier
frameworks for the study of firm-level efficiencies and technology
ratios. *Empirical Economics*, **34**(2), 231–255.
[doi:10.1007/s00181-007-0119-4](https://doi.org/10.1007/s00181-007-0119-4)

Orea, L., and Kumbhakar, S. C. 2004. Efficiency measurement using a
latent class stochastic frontier model. *Empirical Economics*,
**29**(1), 169–183.
[doi:10.1007/s00181-003-0184-2](https://doi.org/10.1007/s00181-003-0184-2)

Dakpo, K. H., Jeanneaux, P., and Latruffe, L. 2016. Modelling
pollution-generating technologies in performance benchmarking: Recent
developments, limits and future prospects in the nonparametric
framework. *European Journal of Operational Research*, **250**(2),
347–359.
[doi:10.1016/j.ejor.2015.07.024](https://doi.org/10.1016/j.ejor.2015.07.024)

Papadopoulos, A. 2015. The half-normal specification for the two-tier
stochastic frontier model. *Journal of Productivity Analysis*,
**43**(2), 225–230.
[doi:10.1007/s11123-014-0389-8](https://doi.org/10.1007/s11123-014-0389-8)

Stevenson, R. E. 1980. Likelihood functions for generalised stochastic
frontier estimation. *Journal of Econometrics*, **13**(1), 57–66.
[doi:10.1016/0304-4076(80)90042-1](https://doi.org/10.1016/0304-4076%2880%2990042-1)

Dakpo, K. H., Latruffe, L., Desjeux, Y., and Jeanneaux, P. 2021. Latent
class modelling for a robust assessment of productivity: Application to
French grazing livestock farms. *Journal of Agricultural Economics*,
**72**(3), 760–781.
[doi:10.1111/1477-9552.12422](https://doi.org/10.1111/1477-9552.12422)

Dakpo, K. H., Latruffe, L., Desjeux, Y., and Jeanneaux, P. 2022.
Modeling heterogeneous technologies in the presence of sample selection:
The case of dairy farms and the adoption of agri-environmental schemes
in France. *Agricultural Economics*, **53**(3), 422–438.
[doi:10.1111/agec.12683](https://doi.org/10.1111/agec.12683)

Tsionas, E. G. 2007. Efficiency measurement with the Weibull stochastic
frontier. *Oxford Bulletin of Economics and Statistics*, **69**(5),
693–706.
[doi:10.1111/j.1468-0084.2007.00475.x](https://doi.org/10.1111/j.1468-0084.2007.00475.x)

Wang, H.-J. 2012. Stochastic frontier models. In *A Companion to
Theoretical Econometrics*, ed. B. H. Baltagi, Blackwell, Oxford.

Wang, H.-J., and Schmidt, P. 2002. One-step and two-step estimation of
the effects of exogenous variables on technical efficiency levels.
*Journal of Productivity Analysis*, **18**(2), 129–144.
[doi:10.1023/A:1016565719882](https://doi.org/10.1023/A%3A1016565719882)

Dakpo, K. H., Desjeux, Y., and Latruffe, L. 2023. sfaR: Stochastic
Frontier Analysis using R. R package version 1.0.1.
<https://CRAN.R-project.org/package=sfaR>

## See also

[`sfacross`](https://rdrr.io/pkg/sfaR/man/sfacross.html),
[`sfaselectioncross`](https://rdrr.io/pkg/sfaR/man/sfaselectioncross.html),
[`sfalcmcross`](https://rdrr.io/pkg/sfaR/man/sfalcmcross.html),
[`efficiencies`](https://SulmanOlieko.github.io/metafrontieR/reference/efficiencies.md),
[`summary.sfametafrontier`](https://SulmanOlieko.github.io/metafrontieR/reference/summary.md),
[`ic`](https://rdrr.io/pkg/sfaR/man/ic.html)

## Examples

``` r
if (FALSE) { # \dontrun{
###########################################################################
## -------- SECTION 1: Standard SFA Group Frontier ----------------------##
## Using the rice production dataset (ricephil) from Battese et al.      ##
## Groups are formed based on farm area terciles (small/medium/large).   ##
###########################################################################

data("ricephil", package = "sfaR")
ricephil$group <- cut(ricephil$AREA,
  breaks = quantile(ricephil$AREA, probs = c(0, 1 / 3, 2 / 3, 1), na.rm = TRUE),
  labels = c("small", "medium", "large"),
  include.lowest = TRUE
)

## 1a. sfacross groups + LP metafrontier
##     Deterministic envelope via linear programming (Battese et al., 2004).
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
# Retrieve individual efficiency and metatechnology ratio estimates:
ef_lp <- efficiencies(meta_sfacross_lp)
head(ef_lp)

## 1b. sfacross groups + QP metafrontier
##     Deterministic envelope via quadratic programming.
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

## 1c. sfacross groups + Two-stage SFA metafrontier (Huang et al., 2014)
##     The group-specific fitted frontier values serve as the dependent
##     variable in the second-stage SFA, yielding a stochastic technology gap.
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
ef_huang <- efficiencies(meta_sfacross_huang)

## 1d. sfacross groups + O'Donnell et al. (2008) stochastic metafrontier
##     The LP deterministic envelope is used as the second-stage dependent
##     variable: the metafrontier is estimated stochastically around the
##     envelope.
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

###########################################################################
## -------- SECTION 2: Latent Class (LCM) Group Frontier ---------------##
## No observed group variable: a pooled sfalcmcross model assigns       ##
## observations to 2 latent technology classes; these classes become the ##
## technology groups for the metafrontier.                               ##
###########################################################################

data("utility", package = "sfaR")

## 2a. sfalcmcross (pooled, 2 classes) + LP metafrontier
meta_lcm_lp <- sfametafrontier(
  formula    = log(tc / wf) ~ log(y) + log(wl / wf) + log(wk / wf),
  data       = utility,
  S          = -1,
  groupType  = "sfalcmcross",
  lcmClasses = 2,
  metaMethod = "lp"
)
summary(meta_lcm_lp)
ef_lcm_lp <- efficiencies(meta_lcm_lp)
# Per-class posterior probabilities and class-specific efficiencies are
# included alongside group and metafrontier efficiencies.

## 2b. sfalcmcross (pooled, 2 classes) + QP metafrontier
meta_lcm_qp <- sfametafrontier(
  formula    = log(tc / wf) ~ log(y) + log(wl / wf) + log(wk / wf),
  data       = utility,
  S          = -1,
  groupType  = "sfalcmcross",
  lcmClasses = 2,
  metaMethod = "qp"
)
summary(meta_lcm_qp)

## 2c. sfalcmcross (pooled, 2 classes) + Two-stage SFA metafrontier
##     (Huang et al., 2014)
meta_lcm_huang <- sfametafrontier(
  formula     = log(tc / wf) ~ log(y) + log(wl / wf) + log(wk / wf),
  data        = utility,
  S           = -1,
  groupType   = "sfalcmcross",
  lcmClasses  = 2,
  metaMethod  = "sfa",
  sfaApproach = "huang"
)
summary(meta_lcm_huang)
ef_lcm_huang <- efficiencies(meta_lcm_huang)

## 2d. sfalcmcross (pooled, 2 classes) + O'Donnell et al. (2008)
meta_lcm_odonnell <- sfametafrontier(
  formula     = log(tc / wf) ~ log(y) + log(wl / wf) + log(wk / wf),
  data        = utility,
  S           = -1,
  groupType   = "sfalcmcross",
  lcmClasses  = 2,
  metaMethod  = "sfa",
  sfaApproach = "ordonnell"
)
summary(meta_lcm_odonnell)

###########################################################################
## -------- SECTION 3: Sample Selection SFA Group Frontier -------------##
## Simulated dataset with a Heckman selection mechanism. Only selected  ##
## observations (d == 1) participate in the frontier and metafrontier.  ##
## Efficiency estimates for non-selected observations are NA.           ##
###########################################################################

N <- 2000
set.seed(12345)
z1 <- rnorm(N)
z2 <- rnorm(N)
v1 <- rnorm(N)
v2 <- rnorm(N)
g <- rnorm(N)
e1 <- v1
e2 <- 0.7071 * (v1 + v2)
ds <- z1 + z2 + e1
d <- ifelse(ds > 0, 1, 0) # binary selection indicator
group <- ifelse(g > 0, 1, 0) # two technology groups (0 and 1)
u <- abs(rnorm(N))
x1 <- rnorm(N)
x2 <- rnorm(N)
y <- x1 + x2 + e2 - u
dat <- as.data.frame(cbind(y = y, x1 = x1, x2 = x2, z1 = z1, z2 = z2, d = d, group = group))

## 3a. sfaselectioncross + LP metafrontier
##     Selection bias is corrected via the Greene (2010) two-step probit
##     approach. The LP envelope envelopes both groups' selected-sample
##     frontier fitted values.
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
ef_sel_lp <- efficiencies(meta_sel_lp)

## 3b. sfaselectioncross + QP metafrontier
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

## 3c. sfaselectioncross + Two-stage SFA metafrontier (Huang et al., 2014)
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
ef_sel_huang <- efficiencies(meta_sel_huang)

## 3d. sfaselectioncross + O'Donnell et al. (2008) stochastic metafrontier
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
} # }
```
