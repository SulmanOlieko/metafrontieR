################################################################################
#                                                                              #
# R functions for the metafrontieR package                                             #
#                                                                              #
################################################################################

#------------------------------------------------------------------------------#
# Model: Stochastic Metafrontier Analysis                                      #
# Data: Cross-sectional or pooled data with group variable                     #
#------------------------------------------------------------------------------#

#' Stochastic metafrontier estimation
#'
#' @description
#' \code{\link{sfametafrontier}} estimates a stochastic metafrontier model
#' following the theoretical frameworks of Battese, Rao, and O'Donnell (2004) and O'Donnell, Rao, and
#' Battese (2008). It additionally implements the two-stage stochastic approach of Huang, Huang, and
#' Liu (2014). Three types of group-level frontier models from the \code{sfaR} package are supported:
#' standard stochastic frontier analysis (\code{\link[sfaR]{sfacross}}), sample selection SFA
#' (\code{\link[sfaR]{sfaselectioncross}}), and latent class SFA (\code{\link[sfaR]{sfalcmcross}}).
#'
#' @aliases sfametafrontier print.sfametafrontier
#'
#' @param formula A symbolic description of the frontier model.
#' @param muhet A one-part formula for heterogeneity in the mean of the
#'   pre-truncated distribution (only for \code{groupType = "sfacross"}). This
#'   formula is used to capture structural differences in the modeled efficiency
#'   mean.
#' @param uhet A one-part formula for heteroscedasticity in the one-sided error
#'   term. To accommodate heteroscedasticity in the variance parameters, the
#'   variances are modeled as: \eqn{\sigma^2_{u} = \exp(\delta'Z_u)}, where
#'   \eqn{Z_u} are the inefficiency drivers and \eqn{\delta} are the coefficients.
#' @param vhet A one-part formula for heteroscedasticity in the two-sided error
#'   term. Modeled as \eqn{\sigma^2_{v} = \exp(\phi'Z_v)}, where \eqn{Z_v} are
#'   the heteroscedasticity variables and \eqn{\phi} the coefficients.
#' @param thet A one-part formula for technological heterogeneity in LCM class
#'   construction (only for \code{groupType = "sfalcmcross"}). The variables
#'   specified are used in the logit formulation of the finite mixture model to
#'   compute the prior class membership probabilities.
#' @param logDepVar Logical. Whether the dependent variable is logged. Default
#'   \code{TRUE}.
#' @param data The data frame.
#' @param subset An optional subset vector.
#' @param group Character string. Name of the column in \code{data} that
#'   identifies the technology groups. Must have at least 2 unique values.
#' @param S \code{1} (default) for production/profit frontier; \code{-1} for
#'   cost frontier.
#' @param udist Character string. Distribution for the one-sided error term.
#'   For \code{groupType = "sfacross"}: all 10 distributions supported (see
#'   \code{\link[sfaR]{sfacross}}). For \code{groupType = "sfaselectioncross"} or
#'   \code{"sfalcmcross"}: only \code{'hnormal'}.
#' @param scaling Logical. Scaling property model for \code{groupType =
#'   "sfacross"} when \code{udist = 'tnormal'}. If \code{TRUE}, the scaling
#'   property is used to model the one-sided error conditional on the inefficiency drivers
#'   \eqn{Z_u} (e.g. \eqn{u = h(Z_u, \delta)u^*} where \eqn{u^*} is a homoscedastic
#'   random variable). Default \code{FALSE}.
#' @param groupType Character string. Type of model used for each group's
#'   frontier. \code{"sfacross"} (default) estimates a standard cross-sectional
#'   SFA, \code{"sfaselectioncross"} estimates a sample selection SFA adjusting
#'   for bias via a generalized Heckman approach, or \code{"sfalcmcross"} estimates
#'   a latent class SFA estimating a pooled mixture model.
#' @param metaMethod Character string. Method for estimating the metafrontier.
#'   \itemize{
#'     \item \code{"lp"} (default): Deterministic envelope (column-wise maximum
#'       of group frontier values evaluated at all observations).
#'     \item \code{"qp"}: Constrained OLS of the envelope on \eqn{X}.
#'     \item \code{"sfa"}: Second-stage pooled SFA. The approach depends on
#'       \code{sfaApproach}.
#'   }
#' @param sfaApproach Character string. Only relevant when \code{metaMethod =
#'   "sfa"}. \code{"huang"} (default):
#'   uses each observation's own group fitted value as the meta-stage dependent
#'   variable (Huang et al., 2014). \code{"ordonnell"}: uses the deterministic envelope of group betas
#'   evaluated at all observations (O'Donnell et al., 2008).
#' @param selectionF A two-sided formula (e.g. \code{selected ~ z1 + z2}) or a named
#'   list of formulas (one per group) specifying the sample selection equation. Only
#'   used when \code{groupType = "sfaselectioncross"}.
#' @param lcmClasses Integer (2--5). Number of latent classes for
#'   \code{groupType = "sfalcmcross"}. Default \code{2}. If \code{group} is
#'   not specified, \code{sfametafrontier} automatically splits the data evenly
#'   into \code{lcmClasses} classes.
#' @param whichStart Integer. Strategy for obtaining initial values for LCM
#'   optimization. \code{whichStart = 1} uses initialized values for each class,
#'   whereas \code{whichStart = 2} (default) estimates a pooled base homoscedastic
#'   model to provide uniform starting points.
#' @param initAlg Character. Initialization algorithm for LCM. A string specifying
#'   the non-gradient or gradient method. Options include \code{"nm"} for
#'   Nelder-Mead (default), \code{"bhhh"}, \code{"bfgs"}, \code{"cg"}, or
#'   \code{"sann"}.
#' @param initIter Integer. Initialization iterations for LCM. Default
#'   \code{100}.
#' @param lType Character. Likelihood type for selection model. Options include
#'   \code{"ghermite"} for Gauss-Hermite quadrature (default) or \code{"msl"} for
#'   Maximum Simulated Likelihood.
#' @param Nsub Integer. Number of quadrature nodes/subdivisions for the
#'   selection model integration when \code{lType = "ghermite"}. Default
#'   \code{100}.
#' @param uBound Numeric. Upper bound for integration in the selection model.
#'   Default \code{Inf}.
#' @param intol Numeric. Integration tolerance for the selection model. Default
#'   \code{1e-6}.
#' @param method Optimization algorithm for group models. Default \code{'bfgs'}.
#'   Other options include \code{"bhhh"}, \code{"nr"}, \code{"nm"}, \code{"cg"},
#'   and \code{"sann"}.
#' @param hessianType Integer (\code{1} or \code{2}). Determines how the
#'   Hessian matrix is computed for standard SFA models. Default \code{1} uses
#'   analytic (when available) or finite-difference numerical Hessians.
#' @param simType Character. Simulation type for MSL. Options are \code{'halton'}
#'   (default), \code{'generalized_halton'}, \code{'sobol'}, or \code{'random'}.
#' @param Nsim Integer. Number of MSL draws. Default \code{100}.
#' @param prime Integer. Prime number for Halton draws. Default \code{2}.
#' @param burn Integer. Number of initial Halton draws discarded. Default \code{10}.
#' @param antithetics Logical. If \code{TRUE}, evaluates antithetic draws to reduce
#'   variance in MSL. Default \code{FALSE}.
#' @param seed Numeric seed. Default \code{12345}.
#' @param itermax Maximum iterations. Default \code{2000}.
#' @param printInfo Logical. Default \code{FALSE}.
#' @param tol Convergence tolerance. Default \code{1e-12}.
#' @param gradtol Gradient tolerance. Default \code{1e-06}.
#' @param stepmax Step max for \code{ucminf}. Default \code{0.1}.
#' @param qac QAC for \code{'bhhh'}/\code{'nr'}. Default \code{'marquardt'}.
#' @param x An object of class \code{'sfametafrontier'} (for printing).
#' @param ... Additional arguments passed to the second-stage SFA call when
#'   \code{metaMethod = "sfa"}.
#'
#' @return \code{\link{sfametafrontier}} returns a list of class \code{'sfametafrontier'} containing the following elements:
#' \item{groupModels}{A list containing the fitted frontier models for each group (class \code{'sfacross'}, \code{'sfaselectioncross'}, or \code{'sfalcmcross'}).}
#' \item{metaSfaObj}{The fitted metafrontier model object. If \code{metaMethod = "sfa"}, this is an object of class \code{'sfacross'} representing the second-stage stochastic frontier. If \code{metaMethod = "lp"} or \code{"qp"}, this contains optimization statistics and coefficients from the deterministic envelope.}
#' \item{metaRes}{A matrix or data frame of the estimated metafrontier coefficients, standard errors, z-values, and p-values.}
#' \item{efficiencies}{A list containing efficiency estimates, including group-specific technical efficiencies (\code{TE_group}), metafrontier technical efficiencies (\code{TE_meta}), and the metatechnology ratios (\code{MTR}).}
#' \item{formula}{The formula used for the frontier.}
#' \item{metaMethod}{The metafrontier estimation method used (\code{"lp"}, \code{"qp"}, or \code{"sfa"}).}
#' \item{sfaApproach}{The SFA approach used in the second stage (\code{"huang"} or \code{"ordonnell"}).}
#' \item{groupType}{The type of group-level models estimated (\code{"sfacross"}, \code{"sfaselectioncross"}, or \code{"sfalcmcross"}).}
#' \item{groups}{A character vector containing the names of the unique groups identified in the data.}
#'
#' @details
#' The stochastic metafrontier analysis workflow comprises two sequential stages.
#' Suppose we have \eqn{N} observations divided into \eqn{G} technology groups.
#' The underlying group frontier for firm \eqn{i} in group \eqn{g} at time \eqn{t} is specified as:
#' \deqn{y_{it} = f(x_{it}, \beta_{(g)}) e^{v_{it} - u_{it}}}
#' where \eqn{f(\cdot)} is the deterministic frontier (e.g., translog or Cobb-Douglas),
#' \eqn{x_{it}} represents the input vector (or output vector for cost models),
#' \eqn{\beta_{(g)}} is the technology parameter vector for group \eqn{g},
#' \eqn{v_{it}} is the statistical noise, and \eqn{u_{it} \ge 0} represents technical inefficiency.
#'
#' \enumerate{
#'   \item \strong{Group-specific estimations:} The data is split into \eqn{G} groups
#'     based on the \code{group} variable (unless \code{groupType="sfalcmcross"}
#'     with \code{group} omitted, in which case a single pooled latent class model
#'     is estimated to map observations to underlying groups automatically). For each explicitly or
#'     implicitly defined group, a frontier model is estimated natively by maximizing
#'     the log-likelihood function.
#'   \item \strong{Metafrontier estimation:} The global metafrontier \eqn{f(x_{it}, \beta^*)}
#'     envelopes all group frontiers \eqn{f(x_{it}, \beta_{(g)})}. The methodologies are:
#'     \itemize{
#'       \item \strong{Linear Programming (LP):} Fits a deterministic envelope over the fitted
#'         group frontier values by minimizing the absolute sum of deviations (Battese et al., 2004).
#'         The objective is to find a single parameter vector \eqn{\beta^*} such that the
#'         metafrontier predictions are completely uniformly larger than the group-specific predictions:
#'         \deqn{\min_{\beta^*} \sum_{i} \sum_{t} \left| \ln f(x_{it}, \beta^*) - \ln \hat{f}(x_{it}, \hat{\beta}_{(g)}) \right|}
#'         subject to \eqn{\ln f(x_{it}, \beta^*) \ge \ln \hat{f}(x_{it}, \hat{\beta}_{(g)})}.
#'       \item \strong{Quadratic Programming (QP):} Similar to LP, but minimizes the sum of squared deviations
#'         between the metafrontier and the underlying group frontiers.
#'       \item \strong{Stochastic Metafrontier (Huang et al., 2014):} Formulates the relationship between the
#'         metafrontier and the group frontier stochastically. Because the true group frontiers
#'         are unobservable, Huang et al. replace them with their estimates:
#'         \deqn{\ln \hat{f}(x_{it}, \hat{\beta}_{(g)}) = \ln f(x_{it}, \beta^*) + V_{it} - U_{it}}
#'         where \eqn{V_{it}} absorbs the statistical noise mapping from the first-stage estimation
#'         (representing the estimation error of the group frontier),
#'         and \eqn{U_{it} \ge 0} captures the technology gap between the group frontier and the
#'         global metafrontier. This method relies directly on estimating a second-stage stochastic frontier
#'         using the group-specific fitted values \eqn{\ln \hat{y}_{it}} as the dependent variable.
#'       \item \strong{Stochastic Metafrontier (O'Donnell et al., 2008):} Derives a stochastic metafrontier by fundamentally
#'         fitting an SFA over the pre-computed, deterministically enveloped values across all observations.
#'         Specifically, the dependent variable in this secondary stage relies on the theoretical
#'         values extracted via mathematical programming.
#'     }
#'   \item \strong{Efficiencies and Metatechnology Ratios (MTR):}
#'     \itemize{
#'       \item Group-specific Technical Efficiency (TE_group): \eqn{TE_{it}^g = exp(-u_{it})}.
#'       \item Metafrontier Technical Efficiency (TE_meta): \eqn{TE_{it}^*} = \eqn{exp(-u_{it} - U_{it})}.
#'       \item Metatechnology Ratio (MTR): The ratio mapping the distance from the group frontier
#'       to the potential metafrontier: \eqn{MTR_{it} = \frac{TE_{it}^*}{TE_{it}^g} = exp(-U_{it})}.
#'     }
#' }
#'
#' When \code{groupType = "sfaselectioncross"}, only the selected observations
#' (\code{selectDum == 1}) participate in the metafrontier; the MTR evaluates as
#' \code{NA} for non-selected observations. When \code{groupType = "sfalcmcross"},
#' the best-posterior-class fitted value \eqn{y_{it}^{c^*}} is routed to the metafrontier algorithm.
#'
#' @references
#' Battese, G. E., Rao, D. S. P., and O'Donnell, C. J. 2004. A metafrontier
#' production function for estimation of technical efficiencies and technology
#' gaps for firms operating under different technologies.
#' \emph{Journal of Productivity Analysis}, \bold{21}(1), 91--103.
#' \url{https://doi.org/10.1023/B:PROD.0000012454.06094.29}
#'
#' Huang, C. J., Huang, T.-H., and Liu, N.-H. 2014. A new approach to
#' estimating the metafrontier production function based on a stochastic
#' frontier framework. \emph{Journal of Productivity Analysis},
#' \bold{42}(3), 241--254.
#' \url{https://doi.org/10.1007/s11123-014-0402-2}
#'
#' O'Donnell, C. J., Rao, D. S. P., and Battese, G. E. 2008. Metafrontier
#' frameworks for the study of firm-level efficiencies and technology ratios.
#' \emph{Empirical Economics}, \bold{34}(2), 231--255.
#' \url{https://doi.org/10.1007/s00181-007-0119-4}
#'
#' @seealso \code{\link[sfaR]{sfacross}}, \code{\link[sfaR]{sfaselectioncross}},
#'   \code{\link[sfaR]{sfalcmcross}}
#'
#' @keywords models optimize metafrontier
#'
#' @examples
#' \dontrun{
#' ## 1. Standard SFA Metafrontier Models
#' data("ricephil", package = "sfaR")
#' ricephil$group <- cut(ricephil$AREA,
#'   breaks = quantile(ricephil$AREA, probs = c(0, 1 / 3, 2 / 3, 1), na.rm = TRUE),
#'   labels = c("small", "medium", "large"),
#'   include.lowest = TRUE
#' )
#'
#' # Linear Programming (LP) Metafrontier
#' meta_lp <- sfametafrontier(
#'   formula = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
#'   data = ricephil, group = "group", S = 1, udist = "hnormal",
#'   metaMethod = "lp"
#' )
#' summary(meta_lp)
#'
#' # Quadratic Programming (QP) Metafrontier
#' meta_qp <- sfametafrontier(
#'   formula = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
#'   data = ricephil, group = "group", S = 1, udist = "hnormal",
#'   metaMethod = "qp"
#' )
#'
#' # Huang (2014) Two-stage SFA Metafrontier
#' meta_huang <- sfametafrontier(
#'   formula = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
#'   data = ricephil, group = "group", S = 1, udist = "hnormal",
#'   metaMethod = "sfa", sfaApproach = "huang"
#' )
#'
#' # O'Donnell (2008) Stochastic Metafrontier on LP base
#' data("utility", package = "sfaR")
#' meta_ordonnell <- sfametafrontier(
#'   formula = log(tc / wf) ~ log(y) + I(1 / 2 * (log(y))^2) +
#'     log(wl / wf) + log(wk / wf) + I(1 / 2 * (log(wl / wf))^2) + I(1 / 2 * (log(wk / wf))^2) +
#'     I(log(wl / wf) * log(wk / wf)) + I(log(y) * log(wl / wf)) + I(log(y) * log(wk / wf)),
#'   data = utility, group = "regu", S = -1, udist = "hnormal",
#'   metaMethod = "sfa", sfaApproach = "ordonnell"
#' )
#'
#' ## 2. Latent Class Metafrontier (LCM) Models - Unobserved Groups
#' # LP Metafrontier using 2 posterior classes built automatically
#' meta_lcm_lp <- sfametafrontier(
#'   formula = log(tc / wf) ~ log(y) + I(1 / 2 * (log(y))^2) +
#'     log(wl / wf) + log(wk / wf) + I(1 / 2 * (log(wl / wf))^2) + I(1 / 2 * (log(wk / wf))^2) +
#'     I(log(wl / wf) * log(wk / wf)) + I(log(y) * log(wl / wf)) + I(log(y) * log(wk / wf)),
#'   data = utility, S = -1, groupType = "sfalcmcross", lcmClasses = 2,
#'   metaMethod = "lp"
#' )
#' summary(meta_lcm_lp)
#'
#' # Huang (2014) stochastic metafrontier on LCM classes
#' meta_lcm_huang <- sfametafrontier(
#'   formula = log(tc / wf) ~ log(y) + I(1 / 2 * (log(y))^2) +
#'     log(wl / wf) + log(wk / wf) + I(1 / 2 * (log(wl / wf))^2) + I(1 / 2 * (log(wk / wf))^2) +
#'     I(log(wl / wf) * log(wk / wf)) + I(log(y) * log(wl / wf)) + I(log(y) * log(wk / wf)),
#'   data = utility, S = -1, groupType = "sfalcmcross", lcmClasses = 2,
#'   metaMethod = "sfa", sfaApproach = "huang"
#' )
#'
#' ## 3. Sample Selection Metafrontier Models
#' ricephil$laterSurvey <- as.integer(ricephil$YEARDUM > 3)
#' # LP Metafrontier using generalized sample selection handling bias
#' meta_sel_lp <- sfametafrontier(
#'   formula = log(PROD) ~ log(AREA) + log(LABOR) + log(NPK),
#'   data = ricephil, group = "group", S = 1, groupType = "sfaselectioncross",
#'   selectionF = laterSurvey ~ EDYRS + AGE, metaMethod = "lp"
#' )
#' summary(meta_sel_lp)
#' }
#'
#' @importFrom sfaR sfacross sfalcmcross sfaselectioncross
#' @importFrom stats as.formula lm model.frame model.matrix model.response na.pass pnorm printCoefmat setNames terms update
#' @export
sfametafrontier <- function(
  formula,
  muhet,
  uhet,
  vhet,
  thet,
  logDepVar = TRUE,
  data,
  subset,
  group = NULL,
  S = 1L,
  udist = "hnormal",
  scaling = FALSE,
  groupType = "sfacross",
  metaMethod = "lp",
  sfaApproach = "huang",
  selectionF = NULL,
  lcmClasses = 2L,
  whichStart = 2L,
  initAlg = "nm",
  initIter = 100L,
  lType = "ghermite",
  Nsub = 100L,
  uBound = Inf,
  intol = 1e-6,
  method = "bfgs",
  hessianType = 1L,
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
) {
  cl <- match.call()

  # ---------- Reject dollar-sign formula notation ----------
  # Using df$var inside a formula (e.g. log(ricephil$PROD) ~ log(ricephil$AREA))
  # bypasses the 'data=' argument when data is subset per group, causing
  # model.frame to use the full dataset and producing length mismatches.
  .check_no_dollar <- function(fml, nm) {
    if (!missing(fml) && !is.null(fml)) {
      txt <- deparse(fml)
      if (any(grepl("$", txt, fixed = TRUE))) {
        stop(
          "Do not use '$' notation in '",
          nm,
          "'. ",
          "Use bare variable names with 'data = <your data frame>' instead.\n",
          "  Bad : log(ricephil$PROD) ~ log(ricephil$AREA)\n",
          "  Good: log(PROD) ~ log(AREA)  with  data = ricephil",
          call. = FALSE
        )
      }
    }
  }
  .check_no_dollar(formula, "formula")
  if (!missing(muhet)) {
    .check_no_dollar(muhet, "muhet")
  }
  if (!missing(uhet)) {
    .check_no_dollar(uhet, "uhet")
  }
  if (!missing(vhet)) {
    .check_no_dollar(vhet, "vhet")
  }
  if (!missing(thet)) {
    .check_no_dollar(thet, "thet")
  }
  if (!is.null(selectionF) && inherits(selectionF, "formula")) {
    .check_no_dollar(selectionF, "selectionF")
  }

  # ---------- Validate groupType ----------
  groupType <- tolower(groupType)
  if (!(groupType %in% c("sfacross", "sfaselectioncross", "sfalcmcross"))) {
    stop(
      "argument 'groupType' must be 'sfacross', 'sfaselectioncross', or 'sfalcmcross'",
      call. = FALSE
    )
  }

  # ---------- Validate metaMethod ----------
  metaMethod <- tolower(metaMethod)
  if (!(metaMethod %in% c("lp", "qp", "sfa"))) {
    stop("argument 'metaMethod' must be 'lp', 'qp', or 'sfa'", call. = FALSE)
  }

  # ---------- Validate sfaApproach ----------
  sfaApproach <- tolower(sfaApproach)
  if (!(sfaApproach %in% c("ordonnell", "huang"))) {
    stop("argument 'sfaApproach' must be 'ordonnell' or 'huang'", call. = FALSE)
  }

  # ---------- Validate / normalise group ----------
  # For sfalcmcross the group variable is OPTIONAL: if omitted, a pooled
  # sfalcmcross is fitted on all data and latent classes serve as groups.
  lcmNoGroup <- (groupType == "sfalcmcross" && is.null(group))

  if (!lcmNoGroup) {
    if (is.null(group) || !is.character(group) || length(group) != 1) {
      stop(
        "argument 'group' must be a single character string naming a column in 'data'",
        call. = FALSE
      )
    }
    if (!group %in% names(data)) {
      stop("'group' variable '", group, "' not found in 'data'", call. = FALSE)
    }
  }

  # Apply subset if provided
  if (!missing(subset)) {
    data <- data[subset, , drop = FALSE]
  }

  # ---- LCM NO-GROUP early path ----
  # Fit a single pooled sfalcmcross; use posterior class assignments as groups.
  if (lcmNoGroup) {
    return(.sfametafrontier_lcm_nogroup(
      formula = formula,
      data = data,
      S = S,
      udist = udist,
      lcmClasses = lcmClasses,
      whichStart = whichStart,
      initAlg = initAlg,
      initIter = initIter,
      metaMethod = metaMethod,
      sfaApproach = sfaApproach,
      method = method,
      hessianType = hessianType,
      itermax = itermax,
      printInfo = printInfo,
      tol = tol,
      gradtol = gradtol,
      stepmax = stepmax,
      qac = qac,
      logDepVar = logDepVar,
      muhet_missing = missing(muhet),
      uhet_missing = missing(uhet),
      vhet_missing = missing(vhet),
      thet_missing = missing(thet),
      uhet = if (!missing(uhet)) uhet else NULL,
      vhet = if (!missing(vhet)) vhet else NULL,
      thet = if (!missing(thet)) thet else NULL,
      cl = cl,
      ...
    ))
  }

  # Ensure group is a factor (normal grouped path)
  data[[group]] <- as.factor(data[[group]])
  groupLevels <- levels(data[[group]])
  nGroups <- length(groupLevels)
  if (nGroups < 2) {
    stop(
      "at least 2 groups are required for metafrontier analysis",
      call. = FALSE
    )
  }

  # ---------- Validate S ----------
  if (length(S) != 1 || !(S %in% c(-1L, 1L))) {
    stop("argument 'S' must equal either 1 or -1", call. = FALSE)
  }
  typeSfa <- if (S == 1L) {
    "Stochastic Production/Profit Frontier, e = v - u"
  } else {
    "Stochastic Cost Frontier, e = v + u"
  }

  # ---------- selectionF validation for sfaselectioncross ----------
  if (groupType == "sfaselectioncross") {
    if (is.null(selectionF)) {
      stop(
        "'selectionF' must be provided when groupType = 'sfaselectioncross'",
        call. = FALSE
      )
    }
    # selectionF must be a two-sided formula whose LHS is the selection indicator,
    # e.g. selected ~ EDYRS + AGE  - consistent with sfaselectioncross's own API.
    # The LHS variable name is used to extract the selectDum column.
    .validate_selF <- function(fml, gname) {
      if (!inherits(fml, "formula")) {
        stop(
          "'selectionF' for group '",
          gname,
          "' must be a formula",
          call. = FALSE
        )
      }
      lhs_vars <- all.vars(update(fml, . ~ 0))
      if (length(lhs_vars) < 1L) {
        stop(
          "'selectionF' must be a two-sided formula with the selection indicator ",
          "as the LHS, e.g. selected ~ EDYRS + AGE",
          call. = FALSE
        )
      }
    }
    if (inherits(selectionF, "formula")) {
      .validate_selF(selectionF, "all groups")
      selecFList <- setNames(rep(list(selectionF), nGroups), groupLevels)
    } else if (is.list(selectionF)) {
      missing_g <- setdiff(groupLevels, names(selectionF))
      if (length(missing_g) > 0) {
        stop(
          "'selectionF' list is missing entries for groups: ",
          paste(missing_g, collapse = ", "),
          call. = FALSE
        )
      }
      selecFList <- selectionF[groupLevels]
      lapply(seq_along(selecFList), function(k) {
        .validate_selF(selecFList[[k]], names(selecFList)[k])
      })
    } else {
      stop(
        "'selectionF' must be a two-sided formula (e.g. selected ~ z1 + z2) ",
        "or a named list of such formulas",
        call. = FALSE
      )
    }
  }

  # ---------- Build sfacross arguments ----------
  sfaBaseArgs <- list(
    formula = formula,
    logDepVar = logDepVar,
    S = S,
    udist = udist,
    method = method,
    hessianType = hessianType,
    simType = simType,
    Nsim = Nsim,
    prime = prime,
    burn = burn,
    antithetics = antithetics,
    seed = seed,
    itermax = itermax,
    printInfo = printInfo,
    tol = tol,
    gradtol = gradtol,
    stepmax = stepmax,
    qac = qac
  )
  if (groupType == "sfacross") {
    sfaBaseArgs$scaling <- scaling
    if (!missing(muhet)) {
      sfaBaseArgs$muhet <- muhet
    }
    if (!missing(uhet)) {
      sfaBaseArgs$uhet <- uhet
    }
    if (!missing(vhet)) sfaBaseArgs$vhet <- vhet
  } else if (groupType == "sfalcmcross") {
    sfaBaseArgs$lcmClasses <- lcmClasses
    sfaBaseArgs$whichStart <- whichStart
    sfaBaseArgs$initAlg <- initAlg
    sfaBaseArgs$initIter <- initIter
    if (!missing(uhet)) {
      sfaBaseArgs$uhet <- uhet
    }
    if (!missing(vhet)) {
      sfaBaseArgs$vhet <- vhet
    }
    if (!missing(thet)) {
      sfaBaseArgs$thet <- thet
    }
    # sfalcmcross does not accept: seed, simType, Nsim, prime, burn, antithetics
    sfaBaseArgs$seed <- NULL
    sfaBaseArgs$simType <- NULL
    sfaBaseArgs$Nsim <- NULL
    sfaBaseArgs$prime <- NULL
    sfaBaseArgs$burn <- NULL
    sfaBaseArgs$antithetics <- NULL
  } else if (groupType == "sfaselectioncross") {
    # sfaselectioncross uses frontierF (not formula) and has its own lType/Nsub args.
    # It does not accept: seed, simType, Nsim, prime, burn, antithetics, scaling.
    sfaBaseArgs$formula <- NULL
    sfaBaseArgs$frontierF <- formula # plain frontier formula, 1 RHS part
    sfaBaseArgs$lType <- lType
    sfaBaseArgs$Nsub <- Nsub
    sfaBaseArgs$uBound <- uBound
    sfaBaseArgs$intol <- intol
    sfaBaseArgs$seed <- NULL
    sfaBaseArgs$simType <- NULL
    sfaBaseArgs$Nsim <- NULL
    sfaBaseArgs$prime <- NULL
    sfaBaseArgs$burn <- NULL
    sfaBaseArgs$antithetics <- NULL
    sfaBaseArgs$scaling <- NULL
    if (!missing(uhet)) {
      sfaBaseArgs$uhet <- uhet
    }
    if (!missing(vhet)) sfaBaseArgs$vhet <- vhet
  }

  # ---------- Step 1: Fit group-specific frontier models ----------
  cat(
    "Estimating group-specific stochastic frontiers (",
    groupType,
    ") ...\n",
    sep = ""
  )
  groupModels <- vector("list", nGroups)
  names(groupModels) <- groupLevels

  for (g in groupLevels) {
    cat("  Group:", g, "\n")
    subData <- data[data[[group]] == g, , drop = FALSE]
    if (nrow(subData) == 0) {
      stop("Group '", g, "' has zero observations", call. = FALSE)
    }
    grpArgs <- c(sfaBaseArgs, list(data = subData))

    if (groupType == "sfaselectioncross") {
      grpArgs$selectionF <- selecFList[[g]]
    }

    estimFun <- switch(groupType,
      sfacross = sfacross,
      sfaselectioncross = sfaselectioncross,
      sfalcmcross = sfalcmcross
    )

    groupModels[[g]] <- tryCatch(
      do.call(estimFun, grpArgs),
      error = function(e) {
        stop(
          "Estimation failed for group '",
          g,
          "': ",
          e$message,
          call. = FALSE
        )
      }
    )
  }
  cat("Group frontiers estimated.\n")

  # ---------- Collect full dataset and fitted values ----------
  dataFull <- data
  dataFull$.mf_rowid <- seq_len(nrow(dataFull))
  dataFull$.mf_group <- dataFull[[group]]
  dataFull$.mf_yhat_group <- NA_real_
  dataFull$.mf_logL_OBS <- NA_real_

  # Selection mask (only relevant for sfaselectioncross)
  if (groupType == "sfaselectioncross") {
    dataFull$.mf_selected <- NA_integer_
  }

  for (g in groupLevels) {
    idx <- which(dataFull[[group]] == g)
    dt_g <- groupModels[[g]]$dataTable

    if (groupType == "sfaselectioncross") {
      # Use the LHS of selectionF to identify the selection indicator - exactly
      # as chalfnormeff_ss does via all.vars(object$selectionF)[1]
      selColName <- all.vars(selecFList[[g]])[1]
      selDum <- dt_g[[selColName]]
      if (is.null(selDum)) {
        stop(
          "Selection indicator column '",
          selColName,
          "' not found in ",
          "dataTable for group '",
          g,
          "'",
          call. = FALSE
        )
      }
      dataFull$.mf_selected[idx] <- as.integer(selDum)
      sel_idx <- idx[selDum == 1]
      dataFull$.mf_yhat_group[sel_idx] <- dt_g$mlFitted[selDum == 1]
      dataFull$.mf_logL_OBS[sel_idx] <- dt_g$logL_OBS[selDum == 1]
    } else if (groupType == "sfalcmcross") {
      best_fit <- extract_lcm_fitted(groupModels[[g]])
      dataFull$.mf_yhat_group[idx] <- best_fit
      dataFull$.mf_logL_OBS[idx] <- dt_g$logL_OBS
    } else {
      # sfacross
      dataFull$.mf_yhat_group[idx] <- dt_g$mlFitted
      dataFull$.mf_logL_OBS[idx] <- dt_g$logL_OBS
    }
  }

  # ---------- Build Xvar matrix for meta-stage ----------
  # Use formula from one of the group models
  if (groupType == "sfaselectioncross") {
    firstModel <- groupModels[[1]]
    formulaFull <- firstModel$frontierF
    # Only use selected obs for metafrontier
    metaIdx <- which(!is.na(dataFull$.mf_yhat_group))
    dataMeta <- dataFull[metaIdx, , drop = FALSE]
    mcFull <- model.frame(formulaFull, data = dataMeta, na.action = na.pass)
    validMeta <- rowSums(is.na(mcFull)) == 0
    Xvar_full <- model.matrix(terms(formulaFull, rhs = 1), mcFull)[
      validMeta, ,
      drop = FALSE
    ]
  } else {
    firstModel <- groupModels[[1]]
    formulaFull <- firstModel$formula
    mcFull <- model.frame(formulaFull, data = dataFull, na.action = na.pass)
    validFull <- rowSums(
      is.na(mcFull) | Reduce("|", lapply(mcFull, is.infinite))
    ) ==
      0
    Xvar_full <- model.matrix(terms(formulaFull, rhs = 1), mcFull)[
      validFull, ,
      drop = FALSE
    ]
    metaIdx <- seq_len(nrow(dataFull))[validFull]
  }

  nXvar <- ncol(Xvar_full)

  # ---------- Build N x G matrix of group betas evaluated at all obs ----------
  # (needed for LP, QP, and ordonnell SFA; not needed for Huang)
  if (
    metaMethod %in%
      c("lp", "qp") ||
      (metaMethod == "sfa" && sfaApproach == "ordonnell")
  ) {
    groupFrontierAll <- matrix(NA_real_, nrow = length(metaIdx), ncol = nGroups)
    colnames(groupFrontierAll) <- groupLevels

    for (g in groupLevels) {
      if (groupType == "sfalcmcross") {
        # For LCM: for obs in group g, use their best-posterior-class fitted
        # value directly from dataTable (already computed by sfalcmcross).
        # For obs NOT in group g, evaluate group g's class-1 beta at their X.
        lcm_nXvar <- groupModels[[g]]$nXvar
        lcm_nuZU <- groupModels[[g]]$nuZUvar
        lcm_nvZV <- groupModels[[g]]$nvZVvar
        stride <- lcm_nXvar + lcm_nuZU + lcm_nvZV
        dt_g <- groupModels[[g]]$dataTable

        # Positions of obs in group g within metaIdx
        grp_pos_in_meta <- which(dataFull[[group]][metaIdx] == g)
        non_pos_in_meta <- which(dataFull[[group]][metaIdx] != g)

        # Own-group obs: use best-class fitted value from dataTable
        # extract_lcm_fitted() returns N_g-length vector ordered as dt_g rows
        if (length(grp_pos_in_meta) > 0) {
          best_fit_g <- extract_lcm_fitted(groupModels[[g]])
          groupFrontierAll[grp_pos_in_meta, g] <- best_fit_g
        }

        # Non-own-group obs: evaluate class-1 beta from group g at their X
        if (length(non_pos_in_meta) > 0) {
          beta1_g <- groupModels[[g]]$mlParam[seq_len(lcm_nXvar)]
          groupFrontierAll[non_pos_in_meta, g] <-
            as.numeric(Xvar_full[non_pos_in_meta, , drop = FALSE] %*% beta1_g)
        }
      } else if (groupType == "sfaselectioncross") {
        # frontier beta: mlParam[1:nXvar_F] as in sfaselectioncross.R
        betaG <- groupModels[[g]]$mlParam[seq_len(groupModels[[g]]$nXvar)]
        groupFrontierAll[, g] <- as.numeric(Xvar_full %*% betaG)
      } else {
        # sfacross: mlParam[1:nXvar]
        betaG <- groupModels[[g]]$mlParam[seq_len(nXvar)]
        groupFrontierAll[, g] <- as.numeric(Xvar_full %*% betaG)
      }
    }
  }

  # ---------- Step 2: Estimate metafrontier ----------
  cat(
    "Estimating metafrontier using method:",
    mfauxdist(metaMethod, sfaApproach),
    "\n"
  )

  metaFrontierParam <- NULL
  metaFrontierVcov <- NULL
  metaSfaObj <- NULL
  yhat_meta_local <- NULL # meta fitted values on metaIdx rows

  if (metaMethod == "lp") {
    yhat_meta_local <- mf_lp(groupFrontierAll)
    metaFrontierParam <- NULL
    metaFrontierVcov <- NULL
  } else if (metaMethod == "qp") {
    qpRes <- mf_qp(Xvar = Xvar_full, groupFrontierMat = groupFrontierAll)
    metaFrontierParam <- qpRes$beta
    names(metaFrontierParam) <- colnames(Xvar_full)
    metaFrontierVcov <- qpRes$vcov
    yhat_meta_local <- qpRes$yhat
  } else {
    # sfa
    if (sfaApproach == "huang") {
      yhat_group_meta <- dataFull$.mf_yhat_group[metaIdx]
      sfaMetaRes <- mf_huang(
        Xvar = Xvar_full,
        yhat_group = yhat_group_meta,
        S = S,
        method = method,
        udist = udist,
        itermax = itermax,
        tol = tol,
        gradtol = gradtol,
        stepmax = stepmax,
        qac = qac,
        ...
      )
    } else {
      sfaMetaRes <- mf_sfa_ordonnell(
        Xvar = Xvar_full,
        groupFrontierMat = groupFrontierAll,
        S = S,
        method = method,
        udist = udist,
        itermax = itermax,
        tol = tol,
        gradtol = gradtol,
        stepmax = stepmax,
        qac = qac,
        ...
      )
    }
    metaFrontierParam <- sfaMetaRes$beta
    names(metaFrontierParam) <- colnames(Xvar_full)
    metaFrontierVcov <- sfaMetaRes$vcov
    metaSfaObj <- sfaMetaRes$sfaObj
    yhat_meta_local <- sfaMetaRes$yhat
  }

  # Store meta fitted values in full data table
  dataFull$.mf_yhat_meta <- NA_real_
  dataFull$.mf_yhat_meta[metaIdx] <- yhat_meta_local
  dataFull$.mf_gap <- NA_real_
  dataFull$.mf_gap[metaIdx] <-
    S * (dataFull$.mf_yhat_meta[metaIdx] - dataFull$.mf_yhat_group[metaIdx])

  # ---------- Compute total log-likelihood and nParm ----------
  groupLogLiks <- sapply(groupModels, function(m) m$mlLoglik)
  groupNParms <- sapply(groupModels, function(m) m$nParm)
  totalLogLik <- sum(groupLogLiks)
  totalNParm <- sum(groupNParms)

  if (metaMethod == "sfa" && !is.null(metaSfaObj)) {
    totalLogLik <- totalLogLik + metaSfaObj$mlLoglik
    totalNParm <- totalNParm + metaSfaObj$nParm
  } else if (metaMethod == "qp") {
    totalNParm <- totalNParm + length(metaFrontierParam)
  }

  # ---------- Assemble return object ----------
  mlDate <- format(Sys.time(), "Model was estimated on : %b %a %d, %Y at %H:%M")

  returnObj <- list()
  returnObj$call <- cl
  returnObj$formula <- formula
  returnObj$group <- group
  returnObj$groups <- groupLevels
  returnObj$nGroups <- nGroups
  returnObj$S <- S
  returnObj$typeSfa <- typeSfa
  returnObj$udist <- udist
  returnObj$groupType <- groupType
  returnObj$metaMethod <- metaMethod
  returnObj$sfaApproach <- sfaApproach
  returnObj$lcmNoGroup <- FALSE
  returnObj$logDepVar <- logDepVar
  returnObj$Nobs <- nrow(dataFull)
  returnObj$NobsMeta <- length(metaIdx)
  returnObj$nXvar <- nXvar
  returnObj$groupModels <- groupModels
  if (exists("groupFrontierAll")) {
    returnObj$groupFrontierAll <- groupFrontierAll
  }
  returnObj$metaFrontierParam <- metaFrontierParam
  returnObj$metaFrontierVcov <- metaFrontierVcov
  returnObj$metaSfaObj <- metaSfaObj
  returnObj$mlLoglik <- totalLogLik
  returnObj$nParm <- totalNParm
  returnObj$dataTable <- dataFull
  returnObj$mlDate <- mlDate

  class(returnObj) <- "sfametafrontier"
  return(returnObj)
}

# print for sfametafrontier ----------
#' @rdname sfametafrontier
#' @export
print.sfametafrontier <- function(x, ...) {
  cat("Call:\n")
  cat(deparse(x$call), "\n\n")
  cat("Stochastic Metafrontier Analysis\n")
  grp_appr <- switch(x$groupType,
    sfacross = "Stochastic Frontier Analysis",
    sfaselectioncross = "Sample Selection Stochastic Frontier Analysis",
    sfalcmcross = "Latent Class Stochastic Frontier Analysis",
    x$groupType
  )
  cat("Group approach   :", grp_appr, "\n")
  cat("Group estimator  :", x$groupType, "\n")
  if (isTRUE(x$lcmNoGroup)) {
    cat("  (Pooled LCM - latent classes used as groups)\n")
  }
  cat("Metafrontier method:", mfauxdist(x$metaMethod, x$sfaApproach), "\n")
  cat("Groups (", x$nGroups, "):", paste(x$groups, collapse = ", "), "\n")
  cat("Total observations:", x$Nobs)
  if (!is.null(x$NobsMeta) && x$NobsMeta < x$Nobs) {
    cat(" (", x$NobsMeta, "in metafrontier)", sep = "")
  }
  cat("\n")
  cat("Distribution:", x$udist, "\n")
  cat(x$typeSfa, "\n\n")
  if (!is.null(x$metaFrontierParam)) {
    cat("Metafrontier coefficients:\n")
    print.default(format(x$metaFrontierParam), print.gap = 2, quote = FALSE)
  } else {
    cat("Metafrontier: deterministic envelope (LP) - no estimated parameters\n")
  }
  if (isTRUE(x$lcmNoGroup)) {
    cat("\nLog-likelihood (pooled LCM):", round(x$mlLoglik, 4), "\n")
  } else {
    cat("\nGroup log-likelihoods:\n")
    for (g in x$groups) {
      cat(sprintf("  %-20s: %.4f\n", g, x$groupModels[[g]]$mlLoglik))
    }
    cat(sprintf("  %-20s: %.4f\n", "Total", x$mlLoglik))
  }
  invisible(x)
}

# ---------------------------------------------------------------------------
# Internal helper: pooled LCM metafrontier (no explicit group variable)
# ---------------------------------------------------------------------------
.sfametafrontier_lcm_nogroup <- function(
  formula,
  data,
  S,
  udist,
  lcmClasses,
  whichStart,
  initAlg,
  initIter,
  metaMethod,
  sfaApproach,
  method,
  hessianType,
  itermax,
  printInfo,
  tol,
  gradtol,
  stepmax,
  qac,
  logDepVar,
  muhet_missing,
  uhet_missing,
  vhet_missing,
  thet_missing,
  uhet = NULL,
  vhet = NULL,
  thet = NULL,
  cl,
  ...
) {
  typeSfa <- if (S == 1L) {
    "Stochastic Production/Profit Frontier, e = v - u"
  } else {
    "Stochastic Cost Frontier, e = v + u"
  }

  # ---- Step 1: Fit single pooled sfalcmcross ----
  cat(
    "Fitting pooled sfalcmcross (",
    lcmClasses,
    " classes) on all data ...\n",
    sep = ""
  )
  lcmArgs <- list(
    formula = formula,
    data = data,
    S = S,
    udist = udist,
    logDepVar = logDepVar,
    lcmClasses = lcmClasses,
    whichStart = whichStart,
    initAlg = initAlg,
    initIter = initIter,
    method = method,
    hessianType = hessianType,
    itermax = itermax,
    printInfo = printInfo,
    tol = tol,
    gradtol = gradtol,
    stepmax = stepmax,
    qac = qac
  )
  if (!uhet_missing && !is.null(uhet)) {
    lcmArgs$uhet <- uhet
  }
  if (!vhet_missing && !is.null(vhet)) {
    lcmArgs$vhet <- vhet
  }
  if (!thet_missing && !is.null(thet)) {
    lcmArgs$thet <- thet
  }

  lcmObj <- tryCatch(
    do.call(sfalcmcross, lcmArgs),
    error = function(e) stop("sfalcmcross failed: ", e$message, call. = FALSE)
  )
  cat("Pooled LCM estimated.\n")

  # ---- Step 2: Extract class assignments and TEs ----
  effLcm <- efficiencies(lcmObj)
  Group_c <- effLcm$Group_c # integer 1..nc per obs
  teGroup_BC <- effLcm$teBC_c # best-class BC efficiency

  nc <- lcmClasses
  groupNames <- paste0("Class_", seq_len(nc))
  nObs <- nrow(data)

  # Build .mf_group column (factor with class labels)
  mf_group <- factor(paste0("Class_", Group_c), levels = groupNames)

  # ---- Step 3: Build Xvar matrix for meta-stage ----
  mcFull <- model.frame(formula, data = data, na.action = na.pass)
  validFull <- rowSums(
    is.na(mcFull) |
      Reduce("|", lapply(mcFull, is.infinite))
  ) ==
    0
  Xvar_full <- model.matrix(terms(formula, rhs = 1), mcFull)[
    validFull, ,
    drop = FALSE
  ]
  metaIdx <- seq_len(nObs)[validFull]
  nXvar <- ncol(Xvar_full)

  # ---- Step 4: Extract class-k betas and build groupFrontierAll ----
  # sfalcmcross mlParam layout: [beta_1 ... beta_nc | Zu_1 ... Zu_nc | Zv_1 ... Zv_nc | theta]
  nX <- lcmObj$nXvar
  nU <- lcmObj$nuZUvar
  nV <- lcmObj$nvZVvar

  # Best-class fitted values (y-hat for each obs using its assigned class frontier)
  # Use mlFitted_ck columns if available, otherwise reconstruct.
  dt_lcm <- lcmObj$dataTable
  yhat_group <- rep(NA_real_, nObs)

  # Try to use mlFitted_ck columns directly
  fit_cols <- paste0("mlFitted_c", seq_len(nc))
  has_fit <- all(fit_cols %in% names(dt_lcm))
  if (has_fit) {
    for (k in seq_len(nc)) {
      idx_k <- which(Group_c == k)
      if (length(idx_k) > 0) {
        yhat_group[idx_k] <- dt_lcm[[fit_cols[k]]][idx_k]
      }
    }
  } else {
    # Reconstruct from class betas
    for (k in seq_len(nc)) {
      # Interleaved layout: [beta_1, Zu_1, Zv_1, beta_2, Zu_2, Zv_2, ...]
      s_idx <- (k - 1) * (nX + nU + nV) + 1
      beta_k <- lcmObj$mlParam[s_idx:(s_idx + nX - 1)]
      yhat_group <- yhat_group # keep NA for invalid
      idx_k <- which(mf_group[validFull] == groupNames[k])
      if (length(idx_k) > 0) {
        yhat_group[metaIdx[idx_k]] <- as.numeric(Xvar_full[idx_k, ] %*% beta_k)
      }
    }
  }

  # groupFrontierAll: N_meta x nc matrix - class k beta evaluated at all obs
  if (
    metaMethod %in%
      c("lp", "qp") ||
      (metaMethod == "sfa" && sfaApproach == "ordonnell")
  ) {
    groupFrontierAll <- matrix(
      NA_real_,
      nrow = length(metaIdx),
      ncol = nc,
      dimnames = list(NULL, groupNames)
    )
    for (k in seq_len(nc)) {
      s_idx <- (k - 1) * (nX + nU + nV) + 1
      beta_k <- lcmObj$mlParam[s_idx:(s_idx + nX - 1)]
      groupFrontierAll[, k] <- as.numeric(Xvar_full %*% beta_k)
    }
  }

  # ---- Step 5: Estimate metafrontier ----
  cat(
    "Estimating metafrontier using method:",
    mfauxdist(metaMethod, sfaApproach),
    "\n"
  )
  metaFrontierParam <- NULL
  metaFrontierVcov <- NULL
  metaSfaObj <- NULL
  yhat_meta_local <- NULL

  if (metaMethod == "lp") {
    yhat_meta_local <- mf_lp(groupFrontierAll)
  } else if (metaMethod == "qp") {
    qpRes <- mf_qp(Xvar = Xvar_full, groupFrontierMat = groupFrontierAll)
    metaFrontierParam <- setNames(qpRes$beta, colnames(Xvar_full))
    metaFrontierVcov <- qpRes$vcov
    yhat_meta_local <- qpRes$yhat
  } else {
    # sfa
    if (sfaApproach == "huang") {
      sfaMetaRes <- mf_huang(
        Xvar = Xvar_full,
        yhat_group = yhat_group[metaIdx],
        S = S,
        method = method,
        udist = udist,
        itermax = itermax,
        tol = tol,
        gradtol = gradtol,
        stepmax = stepmax,
        qac = qac,
        ...
      )
    } else {
      sfaMetaRes <- mf_sfa_ordonnell(
        Xvar = Xvar_full,
        groupFrontierMat = groupFrontierAll,
        S = S,
        method = method,
        udist = udist,
        itermax = itermax,
        tol = tol,
        gradtol = gradtol,
        stepmax = stepmax,
        qac = qac,
        ...
      )
    }
    metaFrontierParam <- setNames(sfaMetaRes$beta, colnames(Xvar_full))
    metaFrontierVcov <- sfaMetaRes$vcov
    metaSfaObj <- sfaMetaRes$sfaObj
    yhat_meta_local <- sfaMetaRes$yhat
  }

  # ---- Step 6: Assemble dataTable ----
  dataFull <- data
  dataFull$.mf_rowid <- seq_len(nObs)
  dataFull$.mf_group <- mf_group # Class_1 / Class_2 / ...
  dataFull$.mf_yhat_group <- yhat_group
  dataFull$.mf_yhat_meta <- NA_real_
  dataFull$.mf_yhat_meta[metaIdx] <- yhat_meta_local
  dataFull$.mf_gap <- NA_real_
  dataFull$.mf_gap[metaIdx] <-
    S * (dataFull$.mf_yhat_meta[metaIdx] - dataFull$.mf_yhat_group[metaIdx])
  dataFull$.mf_logL_OBS <- dt_lcm$logL_OBS

  # ---- Step 7: Assemble return object ----
  mlDate <- format(Sys.time(), "Model was estimated on : %b %a %d, %Y at %H:%M")
  totalNParm <- lcmObj$nParm
  totalLogLik <- lcmObj$mlLoglik
  if (metaMethod == "sfa" && !is.null(metaSfaObj)) {
    totalLogLik <- totalLogLik + metaSfaObj$mlLoglik
    totalNParm <- totalNParm + metaSfaObj$nParm
  } else if (metaMethod == "qp") {
    totalNParm <- totalNParm + length(metaFrontierParam)
  }

  obj <- list(
    call = cl,
    formula = formula,
    group = ".mf_group",
    groups = groupNames,
    nGroups = nc,
    S = S,
    typeSfa = typeSfa,
    udist = udist,
    groupType = "sfalcmcross",
    metaMethod = metaMethod,
    sfaApproach = sfaApproach,
    lcmNoGroup = TRUE,
    lcmObj = lcmObj, # single pooled LCM model
    groupModels = setNames(
      # mirror same object per class for compatibility
      rep(list(lcmObj), nc),
      groupNames
    ),
    logDepVar = logDepVar,
    Nobs = nObs,
    NobsMeta = length(metaIdx),
    nXvar = nXvar,
    metaFrontierParam = metaFrontierParam,
    metaFrontierVcov = metaFrontierVcov,
    metaSfaObj = metaSfaObj,
    mlLoglik = totalLogLik,
    nParm = totalNParm,
    dataTable = dataFull,
    mlDate = mlDate
  )
  if (exists("groupFrontierAll")) {
    obj$groupFrontierAll <- groupFrontierAll
  }
  class(obj) <- "sfametafrontier"
  return(obj)
}
