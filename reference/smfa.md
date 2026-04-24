# Stochastic metafrontier estimation

`smfa` estimates a stochastic metafrontier model for cross-sectional or
pooled data. The function follows the theoretical frameworks of Battese,
Rao, and O'Donnell (2004) and O'Donnell, Rao, and Battese (2008), and
additionally implements the two-stage stochastic approach of Huang,
Huang, and Liu (2014). Three types of group-level frontier models are
supported: standard stochastic frontier analysis
([`sfacross`](https://rdrr.io/pkg/sfaR/man/sfacross.html)), sample
selection stochastic frontier analysis
([`sfaselectioncross`](https://rdrr.io/pkg/sfaR/man/sfaselectioncross.html)),
and latent class stochastic frontier analysis
([`sfalcmcross`](https://rdrr.io/pkg/sfaR/man/sfalcmcross.html)).

## Usage

``` r
smfa(
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

# S3 method for class 'smfa'
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
  standard errors computed by `smfa` are identical to those from a
  standalone `sfaR` call on the same group subset.

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

  An object of class `"smfa"`, as returned by `smfa`, for use with the
  `print` method.

## Value

`smfa` returns an object of class `"smfa"`, which is a list containing:

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
[`efficiencies`](https://SulmanOlieko.github.io/smfa/reference/efficiencies.md)
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
[`efficiencies`](https://SulmanOlieko.github.io/smfa/reference/efficiencies.md),
[`summary.smfa`](https://SulmanOlieko.github.io/smfa/reference/summary.md),
[`ic`](https://rdrr.io/pkg/sfaR/man/ic.html)

## Examples

``` r
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
meta_sfacross_lp <- smfa(
  formula    = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data       = ricephil,
  group      = "group",
  S          = 1,
  udist      = "hnormal",
  groupType  = "sfacross",
  metaMethod = "lp"
)
summary(meta_sfacross_lp)
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
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 
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
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 
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
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 
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
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 
# Retrieve individual efficiency and metatechnology ratio estimates:
ef_lp <- efficiencies(meta_sfacross_lp)
head(ef_lp)
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

## 1b. sfacross groups + QP metafrontier
##     Deterministic envelope via quadratic programming.
meta_sfacross_qp <- smfa(
  formula    = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
  data       = ricephil,
  group      = "group",
  S          = 1,
  udist      = "hnormal",
  groupType  = "sfacross",
  metaMethod = "qp"
)
summary(meta_sfacross_qp)
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
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 
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
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 
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
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 
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
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
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
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 

# \donttest{
## 1c. sfacross groups + Two-stage SFA metafrontier (Huang et al., 2014)
##     The group-specific fitted frontier values serve as the dependent
##     variable in the second-stage SFA, yielding a stochastic technology gap.
meta_sfacross_huang <- smfa(
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
summary(meta_sfacross_huang)
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
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 
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
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 
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
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 
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
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
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
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 
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
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 
ef_huang <- efficiencies(meta_sfacross_huang)
head(ef_huang)
#>   id  group       u_g TE_group_JLMS TE_group_BC TE_group_BC_reciprocal
#> 1  1 medium 0.2697165     0.7635959   0.7673345               1.316036
#> 2  2  large 0.3515642     0.7035867   0.7080897               1.430406
#> 3  3  large 0.2774565     0.7577085   0.7623358               1.327899
#> 4  4 medium 0.1710417     0.8427864   0.8461331               1.191355
#> 5  5  large 0.2119629     0.8089947   0.8133556               1.242901
#> 6  6  small 0.1987499     0.8197549   0.8275685               1.232467
#>         uLB_g     uUB_g        m_g TE_group_mode  teBCLB_g  teBCUB_g    u_meta
#> 1 0.077581942 0.4657010 0.26858570     0.7644599 0.6276949 0.9253512 0.2702214
#> 2 0.130356248 0.5739174 0.35118207     0.7038556 0.5633144 0.8777827 0.3520645
#> 3 0.065447909 0.4980807 0.27501606     0.7595599 0.6076959 0.9366478 0.2779570
#> 4 0.018022507 0.3583190 0.15885675     0.8531186 0.6988501 0.9821389 0.1715392
#> 5 0.027125654 0.4268531 0.20231520     0.8168374 0.6525594 0.9732389 0.2124638
#> 6 0.009050601 0.5251973 0.07998025     0.9231346 0.5914386 0.9909902 0.1992502
#>   TE_meta_JLMS TE_meta_BC  MTR_JLMS    MTR_BC
#> 1    0.7632105  0.7669472 0.9994953 0.9994953
#> 2    0.7032348  0.7077356 0.9994998 0.9994999
#> 3    0.7573294  0.7619545 0.9994997 0.9994997
#> 4    0.8423672  0.8457124 0.9995026 0.9995027
#> 5    0.8085896  0.8129484 0.9994993 0.9994994
#> 6    0.8193448  0.8271546 0.9994998 0.9994998

## 1d. sfacross groups + O'Donnell et al. (2008) stochastic metafrontier
##     The LP deterministic envelope is used as the second-stage dependent
##     variable: the metafrontier is estimated stochastically around the
##     envelope.
meta_sfacross_odonnell <- smfa(
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
summary(meta_sfacross_odonnell)
#> Warning: 344 MTR value(s) > 1 detected in O'Donnell SFA approach. This typically occurs when the second-stage SFA estimates near-zero inefficiency (sigma_u -> 0), causing TE_meta ~= 1 and MTR = TE_meta/TE_group > 1. Consider using metaMethod='lp' or sfaApproach='huang' instead.
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
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 
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
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 
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
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 
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
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
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
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 
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
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 
# }

###########################################################################
## -------- SECTION 2: Latent Class (LCM) Group Frontier ---------------##
## No observed group variable: a pooled sfalcmcross model assigns       ##
## observations to 2 latent technology classes; these classes become the ##
## technology groups for the metafrontier.                               ##
###########################################################################

data("utility", package = "sfaR")

## 2a. sfalcmcross (pooled, 2 classes) + LP metafrontier
meta_lcm_lp <- smfa(
  formula    = log(tc / wf) ~ log(y) + log(wl / wf) + log(wk / wf),
  data       = utility,
  S          = -1,
  groupType  = "sfalcmcross",
  lcmClasses = 2,
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
#> Pooled LCM (2 classes) on all data (N = 791)  Log-likelihood: 61.35324
#> ------------------------------------------------------------ 
#> 
#>   -- Latent Class 1 --
#>   Frontier:
#>             Coefficient  Std. Error z value  Pr(>|z|)    
#> (Intercept) -1.4472e+00  4.0975e-05  -35320 < 2.2e-16 ***
#> log(y)       8.4541e-01  2.4537e-06  344539 < 2.2e-16 ***
#> log(wl/wf)   3.5408e-01  4.7006e-06   75327 < 2.2e-16 ***
#> log(wk/wf)   4.2883e-01  1.4333e-05   29919 < 2.2e-16 ***
#>   Var(u):
#>                Coefficient  Std. Error   z value  Pr(>|z|)    
#> Zu_(Intercept) -1.7658e+00  5.8803e-08 -30029717 < 2.2e-16 ***
#>   Var(v):
#>                Coefficient  Std. Error     z value  Pr(>|z|)    
#> Zv_(Intercept) -3.8640e+01  3.6587e-13 -1.0561e+14 < 2.2e-16 ***
#>   Sigma_u=0.4136  Sigma_v=0.0000  Sigma=0.4136  Gamma=1.0000  Lambda=101648736.5567
#> 
#>   -- Latent Class 2 --
#>   Frontier:
#>             Coefficient  Std. Error   z value  Pr(>|z|)    
#> (Intercept) -2.0490e+00  2.5608e-05 -80011.10 < 2.2e-16 ***
#> log(y)       1.0079e+00  4.1082e-04   2453.37 < 2.2e-16 ***
#> log(wl/wf)  -2.5916e-02  6.3375e-05   -408.92 < 2.2e-16 ***
#> log(wk/wf)   8.8450e-01  6.9315e-05  12760.73 < 2.2e-16 ***
#>   Var(u):
#>                Coefficient  Std. Error  z value  Pr(>|z|)    
#> Zu_(Intercept) -3.0117e+00  1.1348e-06 -2653955 < 2.2e-16 ***
#>   Var(v):
#>                Coefficient  Std. Error  z value  Pr(>|z|)    
#> Zv_(Intercept) -4.9663e+00  5.3865e-07 -9219984 < 2.2e-16 ***
#>   Sigma_u=0.2218  Sigma_v=0.0835  Sigma=0.2370  Gamma=0.8759  Lambda=2.6573
#> 
#>   -- Class Membership (logit) --
#>                 Coefficient  Std. Error  z value  Pr(>|z|)    
#> Cl1_(Intercept) -6.0163e-01  5.0453e-07 -1192457 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
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
#> Total Log-likelihood: 61.35324 
#> AIC: -96.70649   BIC: -35.95362   HQIC: -73.35552 
#> ------------------------------------------------------------ 
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 
ef_lcm_lp <- efficiencies(meta_lcm_lp)
head(ef_lcm_lp)
#>   id Group_c        u_g TE_group_JLMS TE_group_BC TE_group_BC_reciprocal
#> 1  1       2 0.15842767     0.8534847   0.8557685               1.174838
#> 2  2       2 0.11418786     0.8920904   0.8939941               1.123406
#> 3  3       2 0.08540301     0.9181422   0.9196135               1.090950
#> 4  4       2 0.08020650     0.9229257   0.9243035               1.085176
#> 5  5       2 0.05774143     0.9438940   0.9448244               1.060519
#> 6  6       2 0.08181804     0.9214396   0.9228469               1.086964
#>   PosteriorProb_c PosteriorProb_c1 PriorProb_c1       u_c1   teBC_c1
#> 1       0.7249993        0.2750007    0.3539713 0.19428008 0.8234272
#> 2       0.7334018        0.2665982    0.3539713 0.16086081 0.8514106
#> 3       0.7039783        0.2960217    0.3539713 0.10301513 0.9021133
#> 4       0.6909122        0.3090878    0.3539713 0.07745685 0.9254670
#> 5       0.5808759        0.4191241    0.3539713 0.05410185 0.9473356
#> 6       0.6927820        0.3072180    0.3539713 0.05781993 0.9438199
#>   teBC_reciprocal_c1 PosteriorProb_c2 PriorProb_c2       u_c2   teBC_c2
#> 1           1.214436        0.7249993    0.6460287 0.15842767 0.8557685
#> 2           1.174521        0.7334018    0.6460287 0.11418786 0.8939941
#> 3           1.108508        0.7039783    0.6460287 0.08540301 0.9196135
#> 4           1.080536        0.6909122    0.6460287 0.08020650 0.9243035
#> 5           1.055592        0.5808759    0.6460287 0.05774143 0.9448244
#> 6           1.059524        0.6927820    0.6460287 0.08181804 0.9228469
#>   teBC_reciprocal_c2 ineff_c1   ineff_c2 effBC_c1  effBC_c2 ReffBC_c1 ReffBC_c2
#> 1           1.174838       NA 0.15842767       NA 0.8557685        NA  1.174838
#> 2           1.123406       NA 0.11418786       NA 0.8939941        NA  1.123406
#> 3           1.090950       NA 0.08540301       NA 0.9196135        NA  1.090950
#> 4           1.085176       NA 0.08020650       NA 0.9243035        NA  1.085176
#> 5           1.060519       NA 0.05774143       NA 0.9448244        NA  1.060519
#> 6           1.086964       NA 0.08181804       NA 0.9228469        NA  1.086964
#>       u_meta TE_meta_JLMS TE_meta_BC MTR_JLMS MTR_BC
#> 1 0.15842767    0.8534847  0.8557685        1      1
#> 2 0.11418786    0.8920904  0.8939941        1      1
#> 3 0.08540301    0.9181422  0.9196135        1      1
#> 4 0.08020650    0.9229257  0.9243035        1      1
#> 5 0.05774143    0.9438940  0.9448244        1      1
#> 6 0.08181804    0.9214396  0.9228469        1      1

# \donttest{
## 2b. sfalcmcross (pooled, 2 classes) + QP metafrontier
meta_lcm_qp <- smfa(
  formula    = log(tc / wf) ~ log(y) + log(wl / wf) + log(wk / wf),
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
#> Pooled LCM (2 classes) on all data (N = 791)  Log-likelihood: 61.35324
#> ------------------------------------------------------------ 
#> 
#>   -- Latent Class 1 --
#>   Frontier:
#>             Coefficient  Std. Error z value  Pr(>|z|)    
#> (Intercept) -1.4472e+00  4.0975e-05  -35320 < 2.2e-16 ***
#> log(y)       8.4541e-01  2.4537e-06  344539 < 2.2e-16 ***
#> log(wl/wf)   3.5408e-01  4.7006e-06   75327 < 2.2e-16 ***
#> log(wk/wf)   4.2883e-01  1.4333e-05   29919 < 2.2e-16 ***
#>   Var(u):
#>                Coefficient  Std. Error   z value  Pr(>|z|)    
#> Zu_(Intercept) -1.7658e+00  5.8803e-08 -30029717 < 2.2e-16 ***
#>   Var(v):
#>                Coefficient  Std. Error     z value  Pr(>|z|)    
#> Zv_(Intercept) -3.8640e+01  3.6587e-13 -1.0561e+14 < 2.2e-16 ***
#>   Sigma_u=0.4136  Sigma_v=0.0000  Sigma=0.4136  Gamma=1.0000  Lambda=101648736.5567
#> 
#>   -- Latent Class 2 --
#>   Frontier:
#>             Coefficient  Std. Error   z value  Pr(>|z|)    
#> (Intercept) -2.0490e+00  2.5608e-05 -80011.10 < 2.2e-16 ***
#> log(y)       1.0079e+00  4.1082e-04   2453.37 < 2.2e-16 ***
#> log(wl/wf)  -2.5916e-02  6.3375e-05   -408.92 < 2.2e-16 ***
#> log(wk/wf)   8.8450e-01  6.9315e-05  12760.73 < 2.2e-16 ***
#>   Var(u):
#>                Coefficient  Std. Error  z value  Pr(>|z|)    
#> Zu_(Intercept) -3.0117e+00  1.1348e-06 -2653955 < 2.2e-16 ***
#>   Var(v):
#>                Coefficient  Std. Error  z value  Pr(>|z|)    
#> Zv_(Intercept) -4.9663e+00  5.3865e-07 -9219984 < 2.2e-16 ***
#>   Sigma_u=0.2218  Sigma_v=0.0835  Sigma=0.2370  Gamma=0.8759  Lambda=2.6573
#> 
#>   -- Class Membership (logit) --
#>                 Coefficient  Std. Error  z value  Pr(>|z|)    
#> Cl1_(Intercept) -6.0163e-01  5.0453e-07 -1192457 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> Log likelihood status: successful convergence  
#> 
#> ------------------------------------------------------------ 
#> Metafrontier Coefficients (qp):
#>               Estimate Std. Error z value  Pr(>|z|)    
#> (Intercept) -1.3923609  0.0310122 -44.897 < 2.2e-16 ***
#> log(y)       0.8649986  0.0012419 696.520 < 2.2e-16 ***
#> log(wl/wf)   0.2909729  0.0047965  60.664 < 2.2e-16 ***
#> log(wk/wf)   0.5028977  0.0069500  72.359 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
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
#> Total Log-likelihood: 61.35324 
#> AIC: -88.70649   BIC: -9.26042   HQIC: -58.17061 
#> ------------------------------------------------------------ 
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 

## 2c. sfalcmcross (pooled, 2 classes) + Two-stage SFA metafrontier
##     (Huang et al., 2014)
meta_lcm_huang <- smfa(
  formula     = log(tc / wf) ~ log(y) + log(wl / wf) + log(wk / wf),
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
#> Pooled LCM (2 classes) on all data (N = 791)  Log-likelihood: 61.35324
#> ------------------------------------------------------------ 
#> 
#>   -- Latent Class 1 --
#>   Frontier:
#>             Coefficient  Std. Error z value  Pr(>|z|)    
#> (Intercept) -1.4472e+00  4.0975e-05  -35320 < 2.2e-16 ***
#> log(y)       8.4541e-01  2.4537e-06  344539 < 2.2e-16 ***
#> log(wl/wf)   3.5408e-01  4.7006e-06   75327 < 2.2e-16 ***
#> log(wk/wf)   4.2883e-01  1.4333e-05   29919 < 2.2e-16 ***
#>   Var(u):
#>                Coefficient  Std. Error   z value  Pr(>|z|)    
#> Zu_(Intercept) -1.7658e+00  5.8803e-08 -30029717 < 2.2e-16 ***
#>   Var(v):
#>                Coefficient  Std. Error     z value  Pr(>|z|)    
#> Zv_(Intercept) -3.8640e+01  3.6587e-13 -1.0561e+14 < 2.2e-16 ***
#>   Sigma_u=0.4136  Sigma_v=0.0000  Sigma=0.4136  Gamma=1.0000  Lambda=101648736.5567
#> 
#>   -- Latent Class 2 --
#>   Frontier:
#>             Coefficient  Std. Error   z value  Pr(>|z|)    
#> (Intercept) -2.0490e+00  2.5608e-05 -80011.10 < 2.2e-16 ***
#> log(y)       1.0079e+00  4.1082e-04   2453.37 < 2.2e-16 ***
#> log(wl/wf)  -2.5916e-02  6.3375e-05   -408.92 < 2.2e-16 ***
#> log(wk/wf)   8.8450e-01  6.9315e-05  12760.73 < 2.2e-16 ***
#>   Var(u):
#>                Coefficient  Std. Error  z value  Pr(>|z|)    
#> Zu_(Intercept) -3.0117e+00  1.1348e-06 -2653955 < 2.2e-16 ***
#>   Var(v):
#>                Coefficient  Std. Error  z value  Pr(>|z|)    
#> Zv_(Intercept) -4.9663e+00  5.3865e-07 -9219984 < 2.2e-16 ***
#>   Sigma_u=0.2218  Sigma_v=0.0835  Sigma=0.2370  Gamma=0.8759  Lambda=2.6573
#> 
#>   -- Class Membership (logit) --
#>                 Coefficient  Std. Error  z value  Pr(>|z|)    
#> Cl1_(Intercept) -6.0163e-01  5.0453e-07 -1192457 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> Log likelihood status: successful convergence  
#> 
#> ------------------------------------------------------------ 
#> Metafrontier Coefficients (sfa):
#> Meta-optim solver  : BFGS maximization 
#>               Estimate Std. Error  z value  Pr(>|z|)    
#> (Intercept) -2.2495090  0.0686629 -32.7617 < 2.2e-16 ***
#> log(y)       0.9909917  0.0024687 401.4295 < 2.2e-16 ***
#> log(wl/wf)   0.0399592  0.0106166   3.7638 0.0001673 ***
#> log(wk/wf)   0.7890982  0.0143800  54.8745 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#>   Meta-frontier model details:
#> -------------------------------------------------------------------------------- 
#> Normal-Half Normal SF Model 
#> Dependent Variable:                                          group_fitted_values 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                          58 
#> Log likelihood value:                                                  759.62958 
#> Log likelihood gradient norm:                                        2.06038e-03 
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
#> Log Likelihood for OLS Log(H0) =                                       588.59019 
#> LR statistic:  
#> Chisq = 2*[LogL(H0)-LogL(H1)]  =                                       342.07878 
#> Kodde-Palm C*:       95%: 2.70554                                   99%: 5.41189 
#> Coelli (1995) skewness test on OLS residuals
#> M3T: z                         =                                        10.23625 
#> M3T: p.value                   =                                         0.00000 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error  z value  Pr(>|z|)    
#> (Intercept)       -2.24951    0.06866 -32.7617 < 2.2e-16 ***
#> .X2                0.99099    0.00247 401.4295 < 2.2e-16 ***
#> .X3                0.03996    0.01062   3.7638 0.0001673 ***
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
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 
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
#> Total Log-likelihood: 820.9828 
#> AIC: -1603.966   BIC: -1515.173   HQIC: -1569.837 
#> ------------------------------------------------------------ 
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 
ef_lcm_huang <- efficiencies(meta_lcm_huang)
head(ef_lcm_huang)
#>   id Group_c        u_g TE_group_JLMS TE_group_BC TE_group_BC_reciprocal
#> 1  1       2 0.15842767     0.8534847   0.8557685               1.174838
#> 2  2       2 0.11418786     0.8920904   0.8939941               1.123406
#> 3  3       2 0.08540301     0.9181422   0.9196135               1.090950
#> 4  4       2 0.08020650     0.9229257   0.9243035               1.085176
#> 5  5       2 0.05774143     0.9438940   0.9448244               1.060519
#> 6  6       2 0.08181804     0.9214396   0.9228469               1.086964
#>   PosteriorProb_c PosteriorProb_c1 PriorProb_c1       u_c1   teBC_c1
#> 1       0.7249993        0.2750007    0.3539713 0.19428008 0.8234272
#> 2       0.7334018        0.2665982    0.3539713 0.16086081 0.8514106
#> 3       0.7039783        0.2960217    0.3539713 0.10301513 0.9021133
#> 4       0.6909122        0.3090878    0.3539713 0.07745685 0.9254670
#> 5       0.5808759        0.4191241    0.3539713 0.05410185 0.9473356
#> 6       0.6927820        0.3072180    0.3539713 0.05781993 0.9438199
#>   teBC_reciprocal_c1 PosteriorProb_c2 PriorProb_c2       u_c2   teBC_c2
#> 1           1.214436        0.7249993    0.6460287 0.15842767 0.8557685
#> 2           1.174521        0.7334018    0.6460287 0.11418786 0.8939941
#> 3           1.108508        0.7039783    0.6460287 0.08540301 0.9196135
#> 4           1.080536        0.6909122    0.6460287 0.08020650 0.9243035
#> 5           1.055592        0.5808759    0.6460287 0.05774143 0.9448244
#> 6           1.059524        0.6927820    0.6460287 0.08181804 0.9228469
#>   teBC_reciprocal_c2 ineff_c1   ineff_c2 effBC_c1  effBC_c2 ReffBC_c1 ReffBC_c2
#> 1           1.174838       NA 0.15842767       NA 0.8557685        NA  1.174838
#> 2           1.123406       NA 0.11418786       NA 0.8939941        NA  1.123406
#> 3           1.090950       NA 0.08540301       NA 0.9196135        NA  1.090950
#> 4           1.085176       NA 0.08020650       NA 0.9243035        NA  1.085176
#> 5           1.060519       NA 0.05774143       NA 0.9448244        NA  1.060519
#> 6           1.086964       NA 0.08181804       NA 0.9228469        NA  1.086964
#>      u_meta TE_meta_JLMS TE_meta_BC  MTR_JLMS    MTR_BC
#> 1 0.2309811    0.7937545  0.7961367 0.9300161 0.9303178
#> 2 0.1920051    0.8253026  0.8273347 0.9251335 0.9254364
#> 3 0.1633520    0.8492921  0.8509317 0.9250116 0.9253145
#> 4 0.1540442    0.8572341  0.8587931 0.9288224 0.9291245
#> 5 0.1372512    0.8717512  0.8728968 0.9235690 0.9238720
#> 6 0.1501943    0.8605407  0.8621315 0.9339090 0.9342086

## 2d. sfalcmcross (pooled, 2 classes) + O'Donnell et al. (2008)
meta_lcm_odonnell <- smfa(
  formula     = log(tc / wf) ~ log(y) + log(wl / wf) + log(wk / wf),
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
#> Warning: 761 MTR value(s) > 1 detected in O'Donnell SFA approach. This typically occurs when the second-stage SFA estimates near-zero inefficiency (sigma_u -> 0), causing TE_meta ~= 1 and MTR = TE_meta/TE_group > 1. Consider using metaMethod='lp' or sfaApproach='huang' instead.
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
#> Pooled LCM (2 classes) on all data (N = 791)  Log-likelihood: 61.35324
#> ------------------------------------------------------------ 
#> 
#>   -- Latent Class 1 --
#>   Frontier:
#>             Coefficient  Std. Error z value  Pr(>|z|)    
#> (Intercept) -1.4472e+00  4.0975e-05  -35320 < 2.2e-16 ***
#> log(y)       8.4541e-01  2.4537e-06  344539 < 2.2e-16 ***
#> log(wl/wf)   3.5408e-01  4.7006e-06   75327 < 2.2e-16 ***
#> log(wk/wf)   4.2883e-01  1.4333e-05   29919 < 2.2e-16 ***
#>   Var(u):
#>                Coefficient  Std. Error   z value  Pr(>|z|)    
#> Zu_(Intercept) -1.7658e+00  5.8803e-08 -30029717 < 2.2e-16 ***
#>   Var(v):
#>                Coefficient  Std. Error     z value  Pr(>|z|)    
#> Zv_(Intercept) -3.8640e+01  3.6587e-13 -1.0561e+14 < 2.2e-16 ***
#>   Sigma_u=0.4136  Sigma_v=0.0000  Sigma=0.4136  Gamma=1.0000  Lambda=101648736.5567
#> 
#>   -- Latent Class 2 --
#>   Frontier:
#>             Coefficient  Std. Error   z value  Pr(>|z|)    
#> (Intercept) -2.0490e+00  2.5608e-05 -80011.10 < 2.2e-16 ***
#> log(y)       1.0079e+00  4.1082e-04   2453.37 < 2.2e-16 ***
#> log(wl/wf)  -2.5916e-02  6.3375e-05   -408.92 < 2.2e-16 ***
#> log(wk/wf)   8.8450e-01  6.9315e-05  12760.73 < 2.2e-16 ***
#>   Var(u):
#>                Coefficient  Std. Error  z value  Pr(>|z|)    
#> Zu_(Intercept) -3.0117e+00  1.1348e-06 -2653955 < 2.2e-16 ***
#>   Var(v):
#>                Coefficient  Std. Error  z value  Pr(>|z|)    
#> Zv_(Intercept) -4.9663e+00  5.3865e-07 -9219984 < 2.2e-16 ***
#>   Sigma_u=0.2218  Sigma_v=0.0835  Sigma=0.2370  Gamma=0.8759  Lambda=2.6573
#> 
#>   -- Class Membership (logit) --
#>                 Coefficient  Std. Error  z value  Pr(>|z|)    
#> Cl1_(Intercept) -6.0163e-01  5.0453e-07 -1192457 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> Log likelihood status: successful convergence  
#> 
#> ------------------------------------------------------------ 
#> Metafrontier Coefficients (sfa):
#> Meta-optim solver  : BFGS maximization 
#>                Estimate  Std. Error z value  Pr(>|z|)    
#> (Intercept) -1.4655e+00  1.4795e-05  -99060 < 2.2e-16 ***
#> log(y)       8.5052e-01  2.0492e-06  415057 < 2.2e-16 ***
#> log(wl/wf)   3.4196e-01  8.1363e-06   42028 < 2.2e-16 ***
#> log(wk/wf)   4.4325e-01  1.1627e-05   38121 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#>   Meta-frontier model details:
#> -------------------------------------------------------------------------------- 
#> Normal-Half Normal SF Model 
#> Dependent Variable:                                                  lp_envelope 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                        1703 
#> Log likelihood value:                                                 1949.16842 
#> Log likelihood gradient norm:                                        6.64643e+02 
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
#> Lambda = sigma(u)/sigma(v)    =                                    4078429.86815 
#> Var[u]/{Var[u]+Var[v]}        =                                          1.00000 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         0.03285 
#> Average efficiency E[exp(-ui)] =                                         0.96798 
#> -------------------------------------------------------------------------------- 
#> Stochastic Cost Frontier, e = v + u 
#> -----[ Tests vs. No Inefficiency ]-----
#> Likelihood Ratio Test of Inefficiency
#> Deg. freedom for inefficiency model                                            1 
#> Log Likelihood for OLS Log(H0) =                                      1595.36072 
#> LR statistic:  
#> Chisq = 2*[LogL(H0)-LogL(H1)]  =                                       707.61541 
#> Kodde-Palm C*:       95%: 2.70554                                   99%: 5.41189 
#> Coelli (1995) skewness test on OLS residuals
#> M3T: z                         =                                        30.29501 
#> M3T: p.value                   =                                         0.00000 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> (Intercept)       -1.46554    0.00001  -99060 < 2.2e-16 ***
#> .X2                0.85052    0.00000  415057 < 2.2e-16 ***
#> .X3                0.34196    0.00001   42028 < 2.2e-16 ***
#> .X4                0.44325    0.00001   38121 < 2.2e-16 ***
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error     z value  Pr(>|z|)    
#> Zu_(Intercept)       -6.38       0.00 -7.3289e+13 < 2.2e-16 ***
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error     z value  Pr(>|z|)    
#> Zv_(Intercept)     -36.822      0.000 -4.5151e+13 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 
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
#> Total Log-likelihood: 2010.522 
#> AIC: -3983.043   BIC: -3894.251   HQIC: -3948.915 
#> ------------------------------------------------------------ 
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 
# }

###########################################################################
## -------- SECTION 3: Sample Selection SFA Group Frontier -------------##
###########################################################################

## 3a. Small toy example for automatic testing (< 5s)
N <- 100
set.seed(12345)
z1 <- rnorm(N); v1 <- rnorm(N); g <- rnorm(N)
ds <- z1 + v1; d <- ifelse(ds > 0, 1, 0)
group <- ifelse(g > 0, 1, 0)
x1 <- rnorm(N); y <- x1 + rnorm(N) - abs(rnorm(N))
dat <- data.frame(y = y, x1 = x1, z1 = z1, d = d, group = group)

meta_toy <- smfa(
  formula    = y ~ x1,
  selectionF = d ~ z1,
  data       = dat,
  group      = "group",
  groupType  = "sfaselectioncross",
  lType      = "ghermite",
  Nsub       = 10,
  itermax    = 100,
  metaMethod = "lp"
)
#> First step probit model...
#> Second step Frontier model...
#> First step probit model...
#> Second step Frontier model...
summary(meta_toy)
#> ============================================================ 
#> Stochastic Metafrontier Analysis
#> Metafrontier method: Linear Programming (LP) Metafrontier 
#> Stochastic Production/Profit Frontier, e = v - u 
#> Group approach     : Sample Selection Stochastic Frontier Analysis 
#> Group estimator    : sfaselectioncross 
#> Group optim solver : BFGS maximization 
#> Groups ( 2 ): 0, 1 
#> Total observations : 100 
#> Distribution       : hnormal 
#> ============================================================ 
#> 
#> ------------------------------------------------------------ 
#> Group: 0 (N = 49)  Log-likelihood: -45.62034
#> ------------------------------------------------------------ 
#> -------------------------------------------------------------------------------- 
#> Sample Selection Correction Stochastic Frontier Model 
#> Dependent Variable:                                                            y 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                          38 
#> Log likelihood value:                                                  -45.62034 
#> Log likelihood gradient norm:                                        1.42170e-06 
#> Estimation based on:                               N =  25 of 49 obs. and K =  5 
#> Inf. Cr:                                           AIC  =  101.2 AIC/N  =  4.050 
#>                                                    BIC  =  107.3 BIC/N  =  4.293 
#>                                                    HQIC =  102.9 HQIC/N =  4.117 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.33834 
#>            Sigma(v)           =                                          0.33834 
#>            Sigma-squared(u)   =                                          2.00957 
#>            Sigma(u)           =                                          2.00957 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          1.53229 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.85590 
#> Lambda = sigma(u)/sigma(v)    =                                          2.43712 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.68338 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         1.13108 
#> Average efficiency E[exp(-ui)] =                                         0.42693 
#> -------------------------------------------------------------------------------- 
#> Stochastic Production/Profit Frontier, e = v - u 
#> Estimator is 2 step Maximum Likelihood 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> (Intercept)       -0.13372    0.66939 -0.1998 0.8416690    
#> x1                 1.24696    0.34297  3.6357 0.0002772 ***
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value Pr(>|z|)
#> Zu_(Intercept)     0.69792    0.47313  1.4751   0.1402
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value Pr(>|z|)
#> Zv_(Intercept)     -1.0837     1.0154 -1.0672   0.2859
#> -------------------------------------------------------------------------------- 
#>                             Selection bias parameter 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value Pr(>|z|)
#> rho                0.30006    1.54608  0.1941   0.8461
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 
#> Log likelihood status: successful convergence  
#> --------------------------------------------------------------------------------  
#> 
#> ------------------------------------------------------------ 
#> Group: 1 (N = 51)  Log-likelihood: -49.65893
#> ------------------------------------------------------------ 
#> -------------------------------------------------------------------------------- 
#> Sample Selection Correction Stochastic Frontier Model 
#> Dependent Variable:                                                            y 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                          40 
#> Log likelihood value:                                                  -49.65893 
#> Log likelihood gradient norm:                                        2.95384e-07 
#> Estimation based on:                               N =  31 of 51 obs. and K =  5 
#> Inf. Cr:                                           AIC  =  109.3 AIC/N  =  3.526 
#>                                                    BIC  =  116.5 BIC/N  =  3.758 
#>                                                    HQIC =  111.7 HQIC/N =  3.602 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.54157 
#>            Sigma(v)           =                                          0.54157 
#>            Sigma-squared(u)   =                                          1.16138 
#>            Sigma(u)           =                                          1.16138 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          1.30497 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.68198 
#> Lambda = sigma(u)/sigma(v)    =                                          1.46439 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.43797 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         0.85986 
#> Average efficiency E[exp(-ui)] =                                         0.50254 
#> -------------------------------------------------------------------------------- 
#> Stochastic Production/Profit Frontier, e = v - u 
#> Estimator is 2 step Maximum Likelihood 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> (Intercept)        0.06066    0.46421  0.1307 0.8960330    
#> x1                 0.91060    0.24333  3.7422 0.0001824 ***
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value Pr(>|z|)
#> Zu_(Intercept)     0.14961    0.87309  0.1714   0.8639
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value Pr(>|z|)
#> Zv_(Intercept)    -0.61328    0.82095  -0.747    0.455
#> -------------------------------------------------------------------------------- 
#>                             Selection bias parameter 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value Pr(>|z|)
#> rho               -0.54013    0.62891 -0.8588   0.3904
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 
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
#> 0    49      25     0.42326       0.38832    0.38768      0.35571 0.91622
#> 1    51      31     0.53949       0.50028    0.51314      0.47549 0.95606
#>   MTR_JLMS
#> 0  0.91622
#> 1  0.95606
#> 
#> Overall:
#> TE_group_BC=0.4814  TE_group_JLMS=0.4443
#> TE_meta_BC=0.4504   TE_meta_JLMS=0.4156
#> MTR_BC=0.9361     MTR_JLMS=0.9361
#> ------------------------------------------------------------ 
#> Total Log-likelihood: -95.27927 
#> AIC: 210.5585   BIC: 236.6103   HQIC: 221.1021 
#> ------------------------------------------------------------ 
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 

# \donttest{
## 3b. More complex selection models
## Simulated dataset with a Heckman selection mechanism.

N <- 2000
set.seed(12345)
z1 <- rnorm(N); z2 <- rnorm(N); v1 <- rnorm(N); v2 <- rnorm(N); g <- rnorm(N)
e1 <- v1; e2 <- 0.7071 * (v1 + v2)
ds <- z1 + z2 + e1; d <- ifelse(ds > 0, 1, 0)
group <- ifelse(g > 0, 1, 0)
u <- abs(rnorm(N)); x1 <- rnorm(N); x2 <- rnorm(N)
y <- x1 + x2 + e2 - u
dat <- data.frame(y = y, x1 = x1, x2 = x2, z1 = z1, z2 = z2, d = d, group = group)

meta_sel_lp <- smfa(
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
#> Total observations : 2000 
#> Distribution       : hnormal 
#> ============================================================ 
#> 
#> ------------------------------------------------------------ 
#> Group: 0 (N = 994)  Log-likelihood: -920.25257
#> ------------------------------------------------------------ 
#> -------------------------------------------------------------------------------- 
#> Sample Selection Correction Stochastic Frontier Model 
#> Dependent Variable:                                                            y 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                          96 
#> Log likelihood value:                                                 -920.25257 
#> Log likelihood gradient norm:                                        1.98044e-03 
#> Estimation based on:                             N =  489 of 994 obs. and K =  6 
#> Inf. Cr:                                          AIC  =  1852.5 AIC/N  =  3.788 
#>                                                   BIC  =  1877.7 BIC/N  =  3.840 
#>                                                   HQIC =  1862.4 HQIC/N =  3.809 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          0.80380 
#>            Sigma(v)           =                                          0.80380 
#>            Sigma-squared(u)   =                                          1.39840 
#>            Sigma(u)           =                                          1.39840 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          1.48398 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.63500 
#> Lambda = sigma(u)/sigma(v)    =                                          1.31899 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.38733 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         0.94353 
#> Average efficiency E[exp(-ui)] =                                         0.47686 
#> -------------------------------------------------------------------------------- 
#> Stochastic Production/Profit Frontier, e = v - u 
#> Estimator is 2 step Maximum Likelihood 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value Pr(>|z|)    
#> (Intercept)        0.24618    0.14779  1.6658  0.09576 .  
#> x1                 0.93211    0.05109 18.2435  < 2e-16 ***
#> x2                 1.03022    0.04679 22.0190  < 2e-16 ***
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value Pr(>|z|)
#> Zu_(Intercept)     0.33533    0.29801  1.1252   0.2605
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value Pr(>|z|)
#> Zv_(Intercept)    -0.21841    0.18987 -1.1503     0.25
#> -------------------------------------------------------------------------------- 
#>                             Selection bias parameter 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> rho                0.70836    0.12664  5.5935 2.226e-08 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 
#> Log likelihood status: successful convergence  
#> --------------------------------------------------------------------------------  
#> 
#> ------------------------------------------------------------ 
#> Group: 1 (N = 1006)  Log-likelihood: -979.61766
#> ------------------------------------------------------------ 
#> -------------------------------------------------------------------------------- 
#> Sample Selection Correction Stochastic Frontier Model 
#> Dependent Variable:                                                            y 
#> Log likelihood solver:                                         BFGS maximization 
#> Log likelihood iter:                                                          67 
#> Log likelihood value:                                                 -979.61766 
#> Log likelihood gradient norm:                                        7.21516e-06 
#> Estimation based on:                            N =  498 of 1006 obs. and K =  6 
#> Inf. Cr:                                          AIC  =  1971.2 AIC/N  =  3.958 
#>                                                   BIC  =  1996.5 BIC/N  =  4.009 
#>                                                   HQIC =  1981.2 HQIC/N =  3.978 
#> -------------------------------------------------------------------------------- 
#> Variances: Sigma-squared(v)   =                                          1.11132 
#>            Sigma(v)           =                                          1.11132 
#>            Sigma-squared(u)   =                                          1.36932 
#>            Sigma(u)           =                                          1.36932 
#> Sigma = Sqrt[(s^2(u)+s^2(v))] =                                          1.57500 
#> Gamma = sigma(u)^2/sigma^2    =                                          0.55200 
#> Lambda = sigma(u)/sigma(v)    =                                          1.11003 
#> Var[u]/{Var[u]+Var[v]}        =                                          0.30927 
#> -------------------------------------------------------------------------------- 
#> Average inefficiency E[ui]     =                                         0.93367 
#> Average efficiency E[exp(-ui)] =                                         0.47977 
#> -------------------------------------------------------------------------------- 
#> Stochastic Production/Profit Frontier, e = v - u 
#> Estimator is 2 step Maximum Likelihood 
#> Final maximum likelihood estimates 
#> -------------------------------------------------------------------------------- 
#>                          Deterministic Component of SFA 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value Pr(>|z|)    
#> (Intercept)        0.05240    0.12706  0.4124   0.6801    
#> x1                 1.03347    0.04666 22.1479   <2e-16 ***
#> x2                 1.04437    0.05247 19.9048   <2e-16 ***
#> -------------------------------------------------------------------------------- 
#>                   Parameter in variance of u (one-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value Pr(>|z|)
#> Zu_(Intercept)     0.31432    0.25455  1.2348   0.2169
#> -------------------------------------------------------------------------------- 
#>                  Parameters in variance of v (two-sided error) 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value Pr(>|z|)
#> Zv_(Intercept)     0.10555    0.13100  0.8057   0.4204
#> -------------------------------------------------------------------------------- 
#>                             Selection bias parameter 
#> -------------------------------------------------------------------------------- 
#>                Coefficient Std. Error z value  Pr(>|z|)    
#> rho                0.88016    0.08445  10.422 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> -------------------------------------------------------------------------------- 
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 
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
#> 0   994     489     0.41302       0.37188    0.41254      0.37145 0.99892
#> 1  1006     498     0.40229       0.36337    0.33354      0.30120 0.82816
#>   MTR_JLMS
#> 0  0.99892
#> 1  0.82816
#> 
#> Overall:
#> TE_group_BC=0.4077  TE_group_JLMS=0.3676
#> TE_meta_BC=0.3730   TE_meta_JLMS=0.3363
#> MTR_BC=0.9135     MTR_JLMS=0.9135
#> ------------------------------------------------------------ 
#> Total Log-likelihood: -1899.87 
#> AIC: 3823.74   BIC: 3890.951   HQIC: 3848.419 
#> ------------------------------------------------------------ 
#> Model was estimated on : Apr Fri 24, 2026 at 15:13 
# }
```
