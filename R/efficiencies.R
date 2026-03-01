################################################################################
#                                                                              #
# R functions for the metafrontieR package                                     #
#                                                                              #
################################################################################

#------------------------------------------------------------------------------#
# Efficiency/Inefficiency estimation                                           #
# Models:                                                                      #
#        -Stochastic metafrontier analysis and its variants                    #
# Data:  Cross sectional & Pooled data                                         #
#------------------------------------------------------------------------------#

#' Compute efficiency estimates and metatechnology ratios from stochastic metafrontier models
#'
#' @description
#' \code{efficiencies} returns all efficiency estimates and metatechnology ratio
#' (MTR) measures for objects of class \code{"sfametafrontier"} returned by
#' \code{\link{sfametafrontier}}. The function supports models estimated via
#' linear programming (LP), quadratic programming (QP), and stochastic
#' second-stage SFA (\code{"sfa"}), and for each observation it computes the
#' group-specific technical efficiency, the metafrontier technical efficiency,
#' and the metatechnology ratio (MTR), using both the Jondrow, Lovell, Materov,
#' and Schmidt (1982) (JLMS) and the Battese and Coelli (1988) (BC) estimators.
#' Additional model-specific columns are returned depending on \code{groupType}.
#'
#' @name efficiencies
#'
#' @param object An object of class \code{"sfametafrontier"} returned by
#'   \code{\link{sfametafrontier}}.
#' @param level A number strictly between 0 and 0.9999 specifying the nominal
#'   coverage for (in-)efficiency confidence intervals. Default \code{0.95}.
#'   This argument is passed to the underlying \code{efficiencies} method of the
#'   group-level model (class \code{"sfacross"}, \code{"sfalcmcross"}, or
#'   \code{"sfaselectioncross"}).
#' @param newData Optional data frame for out-of-sample prediction of efficiency
#'   estimates. When \code{NULL} (default), efficiencies are computed for the
#'   observations used in the estimation.
#' @param ... Further arguments (currently ignored).
#'
#' @return A data frame with one row per observation (in the original row order),
#'   containing the following columns. The exact set of columns depends on
#'   \code{groupType}:
#'
#'   \strong{Columns present for all model types:}
#'   \describe{
#'     \item{\code{<group>} or \code{Group_c}}{
#'       The technology group identifier for each observation. For
#'       \code{groupType = "sfacross"} and \code{"sfaselectioncross"}, this
#'       column takes the name of the user-supplied \code{group} variable and
#'       contains the group label to which each observation belongs. For
#'       \code{groupType = "sfalcmcross"}, it is named \code{Group_c} and
#'       contains the integer index of the latent class assigned by the maximum
#'       posterior probability criterion.}
#'     \item{\code{u_g}}{
#'       Group-specific conditional mean of the inefficiency term, computed as
#'       \eqn{E[u_i \mid \varepsilon_i]}. This is the JLMS (Jondrow, Lovell,
#'       Materov, and Schmidt, 1982) point estimate of the inefficiency at the
#'       group-frontier level. For \code{groupType = "sfaselectioncross"},
#'       \code{u_g} is \code{NA} for observations not selected into the sample
#'       (selection indicator = 0).}
#'     \item{\code{TE_group_JLMS}}{
#'       Group-specific technical efficiency using the Jondrow, Lovell,
#'       Materov, and Schmidt (1982) estimator:
#'       \eqn{TE^g_i = \exp(-E[u_i \mid \varepsilon_i])}. For
#'       \code{groupType = "sfaselectioncross"}, \code{NA} for non-selected
#'       observations.}
#'     \item{\code{TE_group_BC}}{
#'       Group-specific technical efficiency using the Battese and Coelli
#'       (1988) estimator:
#'       \eqn{TE^g_i = E[\exp(-u_i) \mid \varepsilon_i]}. For
#'       \code{groupType = "sfaselectioncross"}, \code{NA} for non-selected
#'       observations.}
#'     \item{\code{TE_group_BC_reciprocal}}{
#'       Reciprocal of the Battese and Coelli (1988) group technical efficiency:
#'       \eqn{1 / TE^{g,BC}_i}. For production frontiers this equals the
#'       cost-efficiency index implied by the BC estimator. Present for all three
#'       model types. For \code{groupType = "sfaselectioncross"}, \code{NA} for
#'       non-selected observations.}
#'     \item{\code{u_meta}}{
#'       Metafrontier inefficiency, measuring the technology-gap component
#'       \eqn{U_i \ge 0} that separates the group frontier from the global
#'       metafrontier. Computed from the second-stage SFA when
#'       \code{metaMethod = "sfa"}, or derived from the LP/QP gap as
#'       \eqn{U_i = \max\{S \cdot (\ln \hat{y}^*_i - \ln \hat{y}^g_i), 0\}}
#'       when \code{metaMethod = "lp"} or \code{"qp"}.}
#'     \item{\code{TE_meta_JLMS}}{
#'       Metafrontier technical efficiency based on the JLMS group efficiency:
#'       \eqn{TE^*_{JLMS,i} = TE^g_{JLMS,i} \times MTR_{JLMS,i}}.}
#'     \item{\code{TE_meta_BC}}{
#'       Metafrontier technical efficiency based on the Battese and Coelli
#'       (1988) group efficiency:
#'       \eqn{TE^*_{BC,i} = TE^g_{BC,i} \times MTR_{BC,i}}.}
#'     \item{\code{MTR_JLMS}}{
#'       Metatechnology ratio computed using the JLMS group efficiency:
#'       \eqn{MTR_{JLMS,i} = TE^*_{JLMS,i} / TE^g_{JLMS,i} = \exp(-U_i)}.
#'       Values range from 0 to 1. A value of 1 indicates that the group
#'       frontier for this observation coincides with the metafrontier.}
#'     \item{\code{MTR_BC}}{
#'       Metatechnology ratio computed using the Battese and Coelli (1988)
#'       group efficiency:
#'       \eqn{MTR_{BC,i} = TE^*_{BC,i} / TE^g_{BC,i} = \exp(-U_i)}.}
#'   }
#'
#'   \strong{Additional columns for \code{groupType = "sfacross"} only:}
#'   \describe{
#'     \item{\code{uLB_g}, \code{uUB_g}}{
#'       Lower and upper bounds of the \code{level} confidence interval for the
#'       conditional mean inefficiency \code{u_g}, constructed using the
#'       asymptotic distribution of the conditional estimator. Available for
#'       distributions with closed-form expressions for the confidence bounds,
#'       such as \code{udist = "hnormal"} and \code{udist = "tnormal"}.}
#'     \item{\code{m_g}}{
#'       Mode of the conditional distribution of the one-sided error term
#'       \eqn{u_i \mid \varepsilon_i}. This is an alternative point estimate of
#'       inefficiency. Available for distributions for which the conditional mode
#'       has a closed-form expression.}
#'     \item{\code{TE_group_mode}}{
#'       Group-specific technical efficiency evaluated at the conditional mode:
#'       \eqn{TE^g_{\mathrm{mode},i} = \exp(-m_i)}.}
#'     \item{\code{teBCLB_g}, \code{teBCUB_g}}{
#'       Lower and upper bounds of the \code{level} confidence interval for the
#'       Battese and Coelli (1988) group technical efficiency \code{TE_group_BC}.
#'       Constructed from the corresponding bounds on the conditional
#'       distribution of \eqn{\exp(-u_i \mid \varepsilon_i)}.}
#'   }
#'
#'   \strong{Additional columns for \code{groupType = "sfalcmcross"} only:}
#'   \describe{
#'     \item{\code{PosteriorProb_c}}{
#'       Posterior probability that observation \eqn{i} belongs to its assigned
#'       class (the one with the highest posterior probability). Computed via
#'       Bayes' rule as
#'       \eqn{P(j \mid y_i, x_i) \propto \pi(i,j) \, P(i \mid j)},
#'       where \eqn{\pi(i,j)} is the prior class probability and \eqn{P(i \mid j)}
#'       is the class-conditional likelihood.}
#'     \item{\code{PosteriorProb_cJ} (per class, \eqn{J = 1, 2, \ldots})}{
#'       Posterior probability of belonging to latent class \eqn{J}, computed
#'       via Bayes' rule for each class separately. One column is produced for
#'       each estimated class.}
#'     \item{\code{PriorProb_cJ} (per class, \eqn{J = 1, 2, \ldots})}{
#'       Prior (unconditional) probability of belonging to latent class \eqn{J},
#'       given by the logistic specification
#'       \eqn{\pi(i,J) = \exp(\bm{\theta}_J'\mathbf{Z}_{hi}) /
#'       \sum_m \exp(\bm{\theta}_m'\mathbf{Z}_{hi})}.}
#'     \item{\code{u_cJ} (per class, \eqn{J = 1, 2, \ldots})}{
#'       Conditional mean of the inefficiency term for class \eqn{J}:
#'       \eqn{E[u_{i \mid J} \mid \varepsilon_{i \mid J}]}.}
#'     \item{\code{teBC_cJ} (per class, \eqn{J = 1, 2, \ldots})}{
#'       Battese and Coelli (1988) technical efficiency for class \eqn{J}:
#'       \eqn{E[\exp(-u_{i \mid J}) \mid \varepsilon_{i \mid J}]}.}
#'     \item{\code{teBC_reciprocal_cJ} (per class, \eqn{J = 1, 2, \ldots})}{
#'       Reciprocal of the class-\eqn{J} Battese and Coelli (1988) efficiency:
#'       \eqn{1/TE^{BC}_{i \mid J}}.}
#'     \item{\code{ineff_cJ} (per class, \eqn{J = 1, 2, \ldots})}{
#'       Inefficiency estimate for the observation restricted to class \eqn{J}
#'       (i.e. the value assigned to the class to which the observation
#'       \emph{does} belong; \code{NA} for other classes).}
#'     \item{\code{effBC_cJ} (per class, \eqn{J = 1, 2, \ldots})}{
#'       Battese and Coelli (1988) efficiency for the observation's assigned
#'       class; \code{NA} for non-assigned classes.}
#'     \item{\code{ReffBC_cJ} (per class, \eqn{J = 1, 2, \ldots})}{
#'       Reciprocal Battese and Coelli (1988) efficiency for the observation's
#'       assigned class; \code{NA} for non-assigned classes.}
#'   }
#'
#' @details
#' \subsection{Group-specific efficiency estimates}{
#'   For each group, the group-level frontier model is estimated by maximising
#'   the log-likelihood using the distribution specified by \code{udist} in
#'   \code{\link{sfametafrontier}}. Given the estimated composite error
#'   \eqn{\varepsilon_i = v_i - Su_i}, the conditional distribution of
#'   \eqn{u_i \mid \varepsilon_i} is used to compute:
#'   \itemize{
#'     \item the JLMS estimator (Jondrow, Lovell, Materov, and Schmidt, 1982):
#'       \eqn{\hat{u}_i = E[u_i \mid \varepsilon_i]}, and
#'       \eqn{TE^g_{JLMS,i} = \exp(-\hat{u}_i)};
#'     \item the BC estimator (Battese and Coelli, 1988):
#'       \eqn{TE^g_{BC,i} = E[\exp(-u_i) \mid \varepsilon_i]};
#'     \item the mode estimator: \eqn{m_i = \mathrm{mode}[u_i \mid \varepsilon_i]},
#'       and \eqn{TE^g_{\mathrm{mode},i} = \exp(-m_i)};
#'     \item confidence bounds on \eqn{u_i} and \eqn{TE^g_{BC,i}} at the
#'       nominal level \code{level}.
#'   }
#'   For \code{groupType = "sfaselectioncross"}, all estimates are \code{NA}
#'   for observations not selected into the sample (binary selection indicator
#'   equal to 0). For \code{groupType = "sfalcmcross"}, the composite
#'   efficiencies \code{u_g}, \code{TE_group_JLMS}, and \code{TE_group_BC}
#'   are computed using the posterior-probability-weighted class assignments.
#' }
#'
#' \subsection{Metatechnology ratio and metafrontier efficiency}{
#'   The MTR measures how far the group frontier lies below the metafrontier
#'   for each observation. Let \eqn{\ln \hat{y}^g_i} be the group-specific
#'   fitted frontier value and \eqn{\ln \hat{y}^*_i} the metafrontier fitted
#'   value.
#'   \itemize{
#'     \item For \code{metaMethod = "lp"} or \code{"qp"} (Battese, Rao, and
#'       O'Donnell, 2004):
#'       \deqn{MTR_i = \exp\!\bigl(
#'           -\max\!\bigl\{S \cdot (\ln \hat{y}^*_i - \ln \hat{y}^g_i),\, 0\bigr\}
#'         \bigr)}
#'       where \eqn{S = 1} for production/profit frontiers and \eqn{S = -1}
#'       for cost frontiers. The technology gap
#'       \eqn{U_i = \max\{S \cdot (\ln \hat{y}^*_i - \ln \hat{y}^g_i), 0\}}
#'       is stored in \code{u_meta}.
#'     \item For \code{metaMethod = "sfa"} with \code{sfaApproach = "huang"}
#'       (Huang, Huang, and Liu, 2014):
#'       \deqn{MTR_i = TE^*_i = \exp(-U_i)}
#'       where \eqn{U_i} is the one-sided error term from the second-stage SFA.
#'     \item For \code{metaMethod = "sfa"} with
#'       \code{sfaApproach = "ordonnell"} (O'Donnell, Rao, and Battese, 2008):
#'       \eqn{MTR_i = TE^{*,\mathrm{sfa}}_i / TE^g_i}, where
#'       \eqn{TE^{*,\mathrm{sfa}}_i} is the technical efficiency from the
#'       second-stage SFA fitted on the LP envelope values.
#'   }
#'   The metafrontier technical efficiency is then:
#'   \deqn{TE^*_i = TE^g_i \times MTR_i}
#'   computed separately for the JLMS and BC group efficiency estimators.
#'   Both \code{MTR_JLMS} and \code{MTR_BC} are reported, distinguishing
#'   which group-level efficiency estimator was used as the basis.
#' }
#'
#' @references
#' Battese, G. E., and Coelli, T. J. 1988. Prediction of firm-level technical
#' efficiencies with a generalized frontier production function and panel data.
#' \emph{Journal of Econometrics}, \bold{38}(3), 387--399.
#' \url{https://doi.org/10.1016/0304-4076(88)90053-X}
#'
#' Battese, G. E., Rao, D. S. P., and O'Donnell, C. J. 2004. A metafrontier
#' production function for estimation of technical efficiencies and technology
#' gaps for firms operating under different technologies.
#' \emph{Journal of Productivity Analysis}, \bold{21}(1), 91--103.
#' \url{https://doi.org/10.1023/B:PROD.0000012454.06094.29}
#'
#' Huang, C. J., Huang, T.-H., and Liu, N.-H. 2014. A new approach to
#' estimating the metafrontier production function based on a stochastic
#' frontier framework. \emph{Journal of Productivity Analysis},
#' \bold{42}(3), 241--254. \url{https://doi.org/10.1007/s11123-014-0402-2}
#'
#' Jondrow, J., Lovell, C. A. K., Materov, I. S., and Schmidt, P. 1982. On
#' the estimation of technical inefficiency in the stochastic frontier
#' production function model. \emph{Journal of Econometrics},
#' \bold{19}(2-3), 233--238.
#' \url{https://doi.org/10.1016/0304-4076(82)90004-5}
#'
#' O'Donnell, C. J., Rao, D. S. P., and Battese, G. E. 2008. Metafrontier
#' frameworks for the study of firm-level efficiencies and technology ratios.
#' \emph{Empirical Economics}, \bold{34}(2), 231--255.
#' \url{https://doi.org/10.1007/s00181-007-0119-4}
#'
#' Orea, L., and Kumbhakar, S. C. 2004. Efficiency measurement using a latent
#' class stochastic frontier model. \emph{Empirical Economics}, \bold{29}(1),
#' 169--183. \url{https://doi.org/10.1007/s00181-003-0184-2}
#'
#' Dakpo, K. H., Desjeux, Y., and Latruffe, L. 2023. sfaR: Stochastic Frontier
#' Analysis using R. R package version 1.0.1.
#' \url{https://CRAN.R-project.org/package=sfaR}
#'
#' @seealso \code{\link{sfametafrontier}}, for the stochastic metafrontier
#' analysis model fitting function using cross-sectional or pooled data;
#' \code{\link[sfaR]{efficiencies}}, for the underlying group-level efficiency
#' extractor.
#'
#' @aliases efficiencies.sfametafrontier
#' @importFrom sfaR efficiencies
#' @export
efficiencies.sfametafrontier <- function(
  object,
  level = 0.95,
  newData = NULL,
  ...
) {
  if (level < 0 || level > 0.9999) {
    stop("'level' must be between 0 and 0.9999", call. = FALSE)
  }
  if (!is.null(newData)) {
    warning(
      "'newData' is not supported for sfametafrontier objects; ignored.",
      call. = FALSE
    )
  }

  dataFull <- object$dataTable
  groupVar <- object$group
  groups <- object$groups
  S <- object$S
  N <- nrow(dataFull)
  groupType <- if (!is.null(object$groupType)) object$groupType else "sfacross"

  # ---------------------------------------------------------------------------
  # Helper: safely pull column with fallback to N-length NA vector
  # ---------------------------------------------------------------------------
  .g <- function(df, col, n) {
    if (!is.null(df) && col %in% names(df)) df[[col]] else rep(NA_real_, n)
  }
  .gc <- function(df, col, n) {
    if (!is.null(df) && col %in% names(df)) as.character(df[[col]]) else rep(NA_character_, n)
  }

  # ---------------------------------------------------------------------------
  # Containers for the CORE columns (used in MTR computation)
  # ---------------------------------------------------------------------------
  u_g <- rep(NA_real_, N)
  teGroup_JLMS <- rep(NA_real_, N)
  teGroup_BC <- rep(NA_real_, N)

  # Group identifier column
  if (groupType == "sfalcmcross") {
    group_col <- rep(NA_character_, N)
  } else {
    group_col <- as.character(dataFull[[groupVar]])
  }

  # ---------------------------------------------------------------------------
  # Extra sfaR columns that vary by model type — stored as named lists of
  # N-length vectors, assembled into the result data frame at the end.
  # ---------------------------------------------------------------------------
  extra_cols <- list()

  # ---- sfacross / sfaselectioncross optional extras ----
  uLB_g <- rep(NA_real_, N)
  uUB_g <- rep(NA_real_, N)
  m_g <- rep(NA_real_, N)
  teMO_g <- rep(NA_real_, N)
  teBCLB_g <- rep(NA_real_, N)
  teBCUB_g <- rep(NA_real_, N)
  teBC_recip_g <- rep(NA_real_, N)

  # ---- sfalcmcross extras: will be built dynamically ----
  lcm_extra_list <- NULL # will become a data frame with all LCM columns

  # ===========================================================================
  # ---- LCM NO-GROUP path: single pooled sfalcmcross ----
  # ===========================================================================
  if (isTRUE(object$lcmNoGroup) && !is.null(object$lcmObj)) {
    effLcm <- efficiencies(object$lcmObj, level = level)
    group_col <- .gc(effLcm, "Group_c", N)
    u_g <- .g(effLcm, "u_c", N)
    teGroup_JLMS <- .g(effLcm, "teJLMS_c", N)
    teGroup_BC <- .g(effLcm, "teBC_c", N)
    teBC_recip_g <- .g(effLcm, "teBC_reciprocal_c", N)
    # Preserve ALL remaining LCM columns (class-specific, posterior probs, etc.)
    lcm_cols_to_keep <- setdiff(
      names(effLcm),
      c("Group_c", "u_c", "teJLMS_c", "teBC_c", "teBC_reciprocal_c")
    )
    if (length(lcm_cols_to_keep) > 0) {
      lcm_extra_list <- effLcm[, lcm_cols_to_keep, drop = FALSE]
    }
  } else {
    # ===========================================================================
    # ---- Normal grouped path ----
    # ===========================================================================
    # For sfalcmcross we aggregate the LCM extra columns across groups
    lcm_extra_all <- vector("list", length(groups))
    names(lcm_extra_all) <- groups

    for (g in groups) {
      idx <- which(dataFull[[groupVar]] == g)

      effG <- tryCatch(
        {
          m_g_obj <- object$groupModels[[g]]
          efficiencies(m_g_obj, level = level)
        },
        error = function(e) {
          warning(sprintf("Group '%s' efficiencies failed: %s", g, e$message), call. = FALSE)
          NULL
        }
      )

      if (is.null(effG)) next
      ng <- length(idx) # number of obs in this group

      if (groupType == "sfalcmcross") {
        # sfaR sfalcmcross columns:
        # Group_c, PosteriorProb_c, u_c, teJLMS_c, teBC_c, teBC_reciprocal_c,
        # PosteriorProb_c1, PriorProb_c1, u_c1, teBC_c1, teBC_reciprocal_c1, ...(per class)
        # ineff_c1, effBC_c1, ReffBC_c1, ...
        group_col[idx] <- .gc(effG, "Group_c", ng)
        u_g[idx] <- .g(effG, "u_c", ng)
        teGroup_JLMS[idx] <- .g(effG, "teJLMS_c", ng)
        teGroup_BC[idx] <- .g(effG, "teBC_c", ng)
        teBC_recip_g[idx] <- .g(effG, "teBC_reciprocal_c", ng)
        # Extra LCM-specific columns
        lcm_extra_cols <- setdiff(
          names(effG),
          c("Group_c", "u_c", "teJLMS_c", "teBC_c", "teBC_reciprocal_c")
        )
        if (length(lcm_extra_cols) > 0) {
          lcm_extra_all[[g]] <- list(idx = idx, df = effG[, lcm_extra_cols, drop = FALSE])
        }
      } else if (groupType == "sfaselectioncross") {
        # sfaR sfaselectioncross: u, teJLMS, teBC, teBC_reciprocal (NA for non-selected)
        stopifnot(nrow(effG) == ng)
        u_g[idx] <- .g(effG, "u", ng)
        teGroup_JLMS[idx] <- .g(effG, "teJLMS", ng)
        teGroup_BC[idx] <- .g(effG, "teBC", ng)
        teBC_recip_g[idx] <- .g(effG, "teBC_reciprocal", ng)
      } else {
        # sfacross: u, uLB, uUB, teJLMS, m, teMO, teBC, teBCLB, teBCUB, teBC_reciprocal
        u_g[idx] <- .g(effG, "u", ng)
        uLB_g[idx] <- .g(effG, "uLB", ng)
        uUB_g[idx] <- .g(effG, "uUB", ng)
        teGroup_JLMS[idx] <- .g(effG, "teJLMS", ng)
        m_g[idx] <- .g(effG, "m", ng)
        teMO_g[idx] <- .g(effG, "teMO", ng)
        teGroup_BC[idx] <- .g(effG, "teBC", ng)
        teBCLB_g[idx] <- .g(effG, "teBCLB", ng)
        teBCUB_g[idx] <- .g(effG, "teBCUB", ng)
        teBC_recip_g[idx] <- .g(effG, "teBC_reciprocal", ng)
      }
    } # end for g

    # For sfalcmcross: assemble extra columns across groups into one data frame
    if (groupType == "sfalcmcross") {
      all_lcm_cols <- unique(unlist(lapply(lcm_extra_all, function(x) {
        if (!is.null(x)) names(x$df) else NULL
      })))
      if (length(all_lcm_cols) > 0) {
        # Initialise with NAs
        lcm_extra_list <- setNames(
          as.data.frame(matrix(NA_real_, nrow = N, ncol = length(all_lcm_cols))),
          all_lcm_cols
        )
        for (g in groups) {
          x <- lcm_extra_all[[g]]
          if (!is.null(x)) {
            for (col in names(x$df)) {
              lcm_extra_list[[col]][x$idx] <- x$df[[col]]
            }
          }
        }
      }
    }
  } # end else (normal grouped path)

  # ===========================================================================
  # ---- Compute metafrontier efficiency and MTR ----
  # ===========================================================================
  yhat_group <- dataFull$.mf_yhat_group
  yhat_meta <- dataFull$.mf_yhat_meta
  metaMethod <- object$metaMethod
  sfaApproach <- if (!is.null(object$sfaApproach)) object$sfaApproach else "huang"

  if (metaMethod %in% c("lp", "qp")) {
    mtrRes <- compute_mtr(
      yhat_group   = yhat_group,
      yhat_meta    = yhat_meta,
      teGroup_BC   = teGroup_BC,
      teGroup_JLMS = teGroup_JLMS,
      uGroup       = u_g,
      S            = S,
      metaMethod   = metaMethod,
      sfaApproach  = sfaApproach
    )
    teMeta_BC <- mtrRes$teMeta_BC
    teMeta_JLMS <- mtrRes$teMeta_JLMS
    mtr_BC <- mtrRes$mtr_BC
    mtr_JLMS <- mtrRes$mtr_JLMS
    u_meta <- mtrRes$u_meta
  } else {
    # SFA metafrontier: pull ALL efficiency variables from second-stage SFA
    effMeta <- efficiencies(object$metaSfaObj, level = level)
    effMeta_sfa <- data.frame(
      teBC   = rep(NA_real_, N),
      teJLMS = rep(NA_real_, N),
      u      = rep(NA_real_, N)
    )
    metaRows <- which(!is.na(yhat_meta))
    n_meta <- min(length(metaRows), nrow(effMeta))
    indices <- metaRows[seq_len(n_meta)]
    if (!is.null(effMeta[["teBC"]])) effMeta_sfa$teBC[indices] <- effMeta[["teBC"]][seq_len(n_meta)]
    if (!is.null(effMeta[["teJLMS"]])) effMeta_sfa$teJLMS[indices] <- effMeta[["teJLMS"]][seq_len(n_meta)]
    if (!is.null(effMeta[["u"]])) effMeta_sfa$u[indices] <- effMeta[["u"]][seq_len(n_meta)]

    mtrRes <- compute_mtr(
      yhat_group = yhat_group,
      yhat_meta = yhat_meta,
      teGroup_BC = teGroup_BC,
      teGroup_JLMS = teGroup_JLMS,
      uGroup = u_g,
      S = S,
      metaMethod = metaMethod,
      sfaApproach = sfaApproach,
      effMeta_sfa = effMeta_sfa
    )
    teMeta_BC <- mtrRes$teMeta_BC
    teMeta_JLMS <- mtrRes$teMeta_JLMS
    mtr_BC <- mtrRes$mtr_BC
    mtr_JLMS <- mtrRes$mtr_JLMS
    u_meta <- mtrRes$u_meta
  }

  # ===========================================================================
  # ---- Assemble result data frame ----
  # ===========================================================================
  # 1. Group identifier
  if (groupType == "sfalcmcross") {
    res <- data.frame(Group_c = group_col, stringsAsFactors = FALSE)
  } else {
    res <- data.frame(group_col = group_col, stringsAsFactors = FALSE)
    names(res)[1] <- groupVar
  }

  # 2. Core group-level columns (always present)
  res$u_g <- u_g
  res$TE_group_JLMS <- teGroup_JLMS
  res$TE_group_BC <- teGroup_BC

  # 3. Optional sfacross/sfaselectioncross columns (present when model produces them)
  if (groupType %in% c("sfacross", "sfaselectioncross")) {
    res$TE_group_BC_reciprocal <- teBC_recip_g
  }
  if (groupType == "sfacross") {
    # Confidence bounds and mode-based estimates — only attach if non-trivial
    if (any(!is.na(uLB_g))) res$uLB_g <- uLB_g
    if (any(!is.na(uUB_g))) res$uUB_g <- uUB_g
    if (any(!is.na(m_g))) res$m_g <- m_g
    if (any(!is.na(teMO_g))) res$TE_group_mode <- teMO_g
    if (any(!is.na(teBCLB_g))) res$teBCLB_g <- teBCLB_g
    if (any(!is.na(teBCUB_g))) res$teBCUB_g <- teBCUB_g
  }

  # 4. sfalcmcross class-specific columns (passed through verbatim from sfaR)
  if (groupType == "sfalcmcross") {
    res$TE_group_BC_reciprocal <- teBC_recip_g
    if (!is.null(lcm_extra_list) && ncol(lcm_extra_list) > 0) {
      res <- cbind(res, lcm_extra_list)
    }
  }

  # 5. Metafrontier columns
  res$u_meta <- u_meta
  res$TE_meta_JLMS <- teMeta_JLMS
  res$TE_meta_BC <- teMeta_BC
  res$MTR_JLMS <- mtr_JLMS
  res$MTR_BC <- mtr_BC

  rownames(res) <- rownames(dataFull)
  return(res)
}
