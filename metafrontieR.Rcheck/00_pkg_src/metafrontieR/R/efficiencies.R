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
#' @return A data frame that contains individual (in-)efficiency estimates.
#' These are ordered in the same way as the corresponding observations in the
#' dataset used for the estimation.
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
    "ordonnell"
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
    teGroup_JLMS = teGroup_JLMS,
    teGroup_BC = teGroup_BC,
    teMeta_BC = teMeta_BC,
    mtr = mtr,
    row.names = rownames(dataFull)
  )
  return(res)
}
