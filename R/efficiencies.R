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

#' Compute conditional (in-)efficiency estimates of stochastic metafrontier models
#'
#' \code{\link{efficiencies}} returns (in-)efficiency estimates of models
#' estimated with \code{\link{sfametafrontier}}.
#'
#' @name efficiencies
#'
#' @param object A stochastic metafrontier model returned
#' by \code{\link{sfametafrontier}}.
#' @param level A number between between 0 and 0.9999 used for the computation
#' of (in-)efficiency confidence intervals (defaut = \code{0.95}).
#' @param newData Optional data frame that is used to calculate the efficiency
#' estimates. If NULL (the default), the efficiency estimates are calculated
#' for the observations that were used in the estimation.
#' @param ... Currently ignored.
#'
#' @return A data frame containing individual (in-)efficiency estimates, ordered corresponding to the original data used for estimation. The specific columns vary depending on the model and \code{sfaApproach}, but generally include:
#' \item{u_group}{Group-specific inefficiency estimates.}
#' \item{TE_group_JLMS}{Group-specific technical efficiency estimates using Jondrow et al. (1982).}
#' \item{TE_group_BC}{Group-specific technical efficiency estimates using Battese and Coelli (1988).}
#' \item{u_meta}{Metafrontier inefficiency estimates.}
#' \item{TE_meta_JLMS}{Metafrontier technical efficiency estimates using JLMS.}
#' \item{TE_meta_BC}{Metafrontier technical efficiency estimates using BC.}
#' \item{MTR_JLMS}{Metatechnology ratio based on JLMS estimates.}
#' \item{MTR_BC}{Metatechnology ratio based on BC estimates.}
#' @details
#' The metatechnology ratio (MTR) and metafrontier efficiencies are computed as follows:
#' \itemize{
#'   \item \strong{Group Efficiency:} \eqn{TE_{it}^g = \exp(-u_{it})}, evaluated natively
#'     by the group-specific frontier (e.g. using Jondrow et al., 1982 or Battese and Coelli, 1988).
#'   \item \strong{Metatechnology Ratio (MTR):} Evaluates the distance between the
#'     group-specific frontier and the global metafrontier.
#'     \itemize{
#'       \item For deterministic envelopes (\code{"lp"}, \code{"qp"}):
#'         \deqn{MTR_{it} = \exp\left( - \max\left\{S \times (\ln \hat{y}_{it}^* - \ln \hat{y}_{it}^g), 0\right\} \right)}
#'         where \eqn{S=1} for production/profit and \eqn{S=-1} for cost.
#'       \item For Huang et al. (2014) stochastic approach:
#'         \deqn{MTR_{it} = \exp(-U_{it})}
#'         directly estimated as the technical efficiency from the second-stage SFA regression
#'         where the dependent variable is the group-fitted values \eqn{\ln \hat{y}_{it}^g}.
#'       \item For O'Donnell et al. (2008) stochastic approach:
#'         \eqn{MTR_{it} = TE_{it}^* / TE_{it}^g}, potentially vulnerable to bounding issues if \eqn{TE_{it}^* > TE_{it}^g}.
#'     }
#'   \item \strong{Metafrontier Efficiency:} \eqn{TE_{it}^* = TE_{it}^g \times MTR_{it}}.
#' }
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
#' @seealso \code{\link{sfametafrontier}}, for the stochastic metafrontier analysis model
#' fitting function using cross-sectional or pooled data.
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

  # Collect group-level efficiency for each obs
  u_g <- rep(NA_real_, N)
  teGroup_JLMS <- rep(NA_real_, N)
  teGroup_BC <- rep(NA_real_, N)

  # ---- LCM no-group path: single pooled model, classes as groups ----
  if (isTRUE(object$lcmNoGroup) && !is.null(object$lcmObj)) {
    effLcm <- efficiencies(object$lcmObj, level = level)
    u_g <- effLcm$u_c
    teGroup_JLMS <- effLcm$teJLMS_c
    teGroup_BC <- effLcm$teBC_c
  } else {
    for (g in groups) {
      idx <- which(dataFull[[groupVar]] == g)
      effG <- efficiencies(object$groupModels[[g]], level = level)

      if (groupType == "sfalcmcross") {
        # cLCM*Chalfnormeff returns columns: Group_c, u_c, teJLMS_c, teBC_c
        u_g[idx] <- effG[["u_c"]]
        teGroup_JLMS[idx] <- effG[["teJLMS_c"]]
        teGroup_BC[idx] <- effG[["teBC_c"]]
      } else if (groupType == "sfaselectioncross") {
        # chalfnormeff_ss returns Ninit rows (all group obs) with NA for non-selected.
        # Row order matches the group's dataTable (same order as idx).
        # We map directly: effG row i corresponds to idx[i] in dataFull.
        stopifnot(nrow(effG) == length(idx))
        u_g[idx] <- effG[["u"]] # NA for non-selected
        teGroup_JLMS[idx] <- effG[["teJLMS"]] # NA for non-selected
        teGroup_BC[idx] <- effG[["teBC"]] # NA for non-selected
      } else {
        # sfacross: columns u, teJLMS, teBC
        u_g[idx] <- effG[["u"]]
        teGroup_JLMS[idx] <- effG[["teJLMS"]]
        teGroup_BC[idx] <- effG[["teBC"]]
      }
    }
  } # end else (normal grouped path)

  # Compute metafrontier efficiency and MTR
  yhat_group <- dataFull$.mf_yhat_group
  yhat_meta <- dataFull$.mf_yhat_meta
  metaMethod <- object$metaMethod
  sfaApproach <- if (!is.null(object$sfaApproach)) {
    object$sfaApproach
  } else {
    "huang"
  }

  if (metaMethod %in% c("lp", "qp")) {
    mtrRes <- compute_mtr(
      yhat_group = yhat_group,
      yhat_meta = yhat_meta,
      teGroup = teGroup_BC,
      S = S,
      metaMethod = metaMethod,
      sfaApproach = sfaApproach
    )
    teMeta_BC <- mtrRes$teMeta
    mtr <- mtrRes$mtr
  } else {
    # SFA metafrontier: pull BC efficiency from second-stage SFA, align to N rows
    effMeta <- efficiencies(object$metaSfaObj, level = level)
    teMeta_sfa <- rep(NA_real_, N)
    metaRows <- which(!is.na(yhat_meta))
    n_meta <- min(length(metaRows), nrow(effMeta))
    teMeta_sfa[metaRows[seq_len(n_meta)]] <- effMeta[["teBC"]][seq_len(n_meta)]
    mtrRes <- compute_mtr(
      yhat_group = yhat_group,
      yhat_meta = yhat_meta,
      teGroup = teGroup_BC,
      S = S,
      metaMethod = metaMethod,
      sfaApproach = sfaApproach,
      teMeta_sfa = teMeta_sfa
    )
    teMeta_BC <- mtrRes$teMeta
    mtr <- mtrRes$mtr
  }

  res <- data.frame(
    u_g = u_g,
    TE_group_JLMS = teGroup_JLMS,
    TE_group_BC = teGroup_BC,
    TE_meta_BC = teMeta_BC,
    MTR = mtr,
    row.names = rownames(dataFull)
  )
  return(res)
}
