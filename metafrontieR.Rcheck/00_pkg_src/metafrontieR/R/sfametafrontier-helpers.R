################################################################################
#                                                                              #
# R internal helper functions for stochastic metafrontier analysis             #
# sfaR package                                                                 #
#                                                                              #
################################################################################

#------------------------------------------------------------------------------#
# Metafrontier estimation helpers                                              #
#------------------------------------------------------------------------------#

# Pretty-print name for the metafrontier method -----------
#' @param metaMethod character string for metafrontier method
#' @param sfaApproach character string for SFA approach ("ordonnell" or "huang")
#' @noRd
mfauxdist <- function(metaMethod, sfaApproach = "ordonnell") {
  base <- switch(metaMethod,
    lp = "Linear Programming (LP) Metafrontier",
    qp = "Quadratic Programming (QP) Metafrontier",
    sfa = if (sfaApproach == "huang") {
      "SFA Metafrontier [Huang et al. (2014), two-stage]"
    } else {
      "SFA Metafrontier [O'Donnell et al. (2008), envelope]"
    }
  )
  base
}

# LP-based metafrontier: column-maximum of evaluated group frontier values -----
# The metafrontier predicted value for obs i: max over all group betas at X_i
#' @param groupFrontierMat N x G matrix of group frontier predicted values (all
#'   groups evaluated at each obs's X)
#' @noRd
mf_lp <- function(groupFrontierMat) {
  apply(groupFrontierMat, 1, max, na.rm = TRUE)
}

# QP-based metafrontier: OLS of envelope on X ----------------------------------
#' @param Xvar matrix of explanatory variables (N x K)
#' @param groupFrontierMat N x G matrix of group frontier predicted values
#' @noRd
mf_qp <- function(Xvar, groupFrontierMat) {
  yMeta <- apply(groupFrontierMat, 1, max, na.rm = TRUE)
  if (colnames(Xvar)[1] == "(Intercept)") {
    fit <- lm(yMeta ~ ., data = as.data.frame(Xvar[, -1, drop = FALSE]))
  } else {
    fit <- lm(yMeta ~ -1 + ., data = as.data.frame(Xvar))
  }
  list(
    beta  = coef(fit),
    vcov  = vcov(fit),
    yhat  = fitted(fit),
    sigma = summary(fit)$sigma,
    fit   = fit
  )
}

# O'Donnell SFA metafrontier: sfacross on envelope of group-evaluated values ---
#' @param Xvar matrix (N x K)
#' @param groupFrontierMat N x G matrix of group frontier values (all betas
#'   evaluated at all obs)
#' @param S 1 or -1
#' @param method optimization method
#' @param udist distribution string
#' @param ... passed to sfacross
#' @noRd
mf_sfa_ordonnell <- function(Xvar, groupFrontierMat, S, method, udist, ...) {
  yMeta <- apply(groupFrontierMat, 1, max, na.rm = TRUE)
  mf_sfa_fit(
    Xvar = Xvar, yMeta = yMeta, S = S, method = method,
    udist = udist, ...
  )
}

# Huang (2014) SFA metafrontier: sfacross on pooled own-group fitted values ----
# DV = yhat_{g(i)} for each obs, i.e. the fitted value from the obs's own group.
#' @param Xvar matrix (N x K) - same X used in group models
#' @param yhat_group N-vector of group fitted values (each obs from its own group)
#' @param S 1 or -1
#' @param method optimization method
#' @param udist distribution string
#' @param ... passed to sfacross
#' @noRd
mf_huang <- function(Xvar, yhat_group, S, method, udist, ...) {
  mf_sfa_fit(
    Xvar = Xvar, yMeta = yhat_group, S = S, method = method,
    udist = udist, ...
  )
}

# Shared second-stage sfacross fitting routine ---------------------------------
#' @noRd
mf_sfa_fit <- function(Xvar, yMeta, S, method, udist, ...) {
  # Use safe syntactic names internally to avoid issues when original Xvar
  # column names contain expressions like "log(df$var)", spaces, or operators.
  K <- ncol(Xvar)
  orig_names <- colnames(Xvar)
  safe_names <- paste0(".X", seq_len(K))

  hasIntercept <- orig_names[1L] == "(Intercept)"

  df_meta <- setNames(as.data.frame(Xvar), safe_names)
  df_meta[[".yMeta"]] <- yMeta

  if (hasIntercept) {
    rhs_parts <- safe_names[-1L]
    fStr <- if (length(rhs_parts) > 0L) {
      paste(".yMeta ~", paste(rhs_parts, collapse = " + "))
    } else {
      ".yMeta ~ 1"
    }
  } else {
    fStr <- paste(".yMeta ~ -1 +", paste(safe_names, collapse = " + "))
  }
  fml <- as.formula(fStr)

  sfaObj <- tryCatch(
    sfacross(
      formula = fml, udist = udist, S = S, data = df_meta,
      method = method, ...
    ),
    error = function(e) {
      stop("SFA metafrontier estimation failed: ", e$message, call. = FALSE)
    }
  )

  # Return coefficients indexed back to original names
  beta_idx <- seq_len(K)
  betas <- sfaObj$mlParam[beta_idx]
  names(betas) <- orig_names
  vcov_mat <- sfaObj$invHessian[beta_idx, beta_idx, drop = FALSE]
  dimnames(vcov_mat) <- list(orig_names, orig_names)

  list(
    beta   = betas,
    vcov   = vcov_mat,
    yhat   = sfaObj$dataTable$mlFitted,
    sfaObj = sfaObj
  )
}

# Extract best-class fitted value from sfalcmcross object ----------------------
# For each observation, selects the fitted value from the class with the highest
# posterior probability (using the same posterior logic as efficiencies.sfalcmcross).
#' @param lcmObj a sfalcmcross model object
#' @noRd
extract_lcm_fitted <- function(lcmObj) {
  dt <- lcmObj$dataTable
  nClass <- lcmObj$nClasses

  # efficiencies.sfalcmcross returns Group_c (best-posterior class index, 1-based)
  effRes <- efficiencies(lcmObj)

  if ("Group_c" %in% names(effRes)) {
    bestClass <- effRes$Group_c
    best_fitted <- vapply(seq_len(nrow(dt)), function(i) {
      colnm <- paste0("mlFitted_c", bestClass[i])
      if (colnm %in% names(dt)) dt[[colnm]][i] else NA_real_
    }, numeric(1))
  } else {
    # Fallback: class-1 fitted values
    best_fitted <- if ("mlFitted_c1" %in% names(dt)) dt[["mlFitted_c1"]] else NA_real_
  }
  best_fitted
}

# Compute metatechnology ratio (MTR) -------------------------------------------
#' @param yhat_group N-vector of group frontier fitted values
#' @param yhat_meta N-vector of metafrontier fitted values
#' @param teGroup N-vector of group efficiency (teBC from group SFA)
#' @param S integer 1 or -1
#' @param metaMethod character "lp", "qp", or "sfa"
#' @param sfaApproach character "ordonnell" or "huang" (used when metaMethod="sfa")
#' @param teMeta_sfa optional N-vector - BC efficiency from the second-stage SFA
#' @noRd
compute_mtr <- function(yhat_group, yhat_meta, teGroup, S,
                        metaMethod = "lp", sfaApproach = "ordonnell",
                        teMeta_sfa = NULL) {
  if (metaMethod %in% c("lp", "qp")) {
    # LP/QP: gap = S*(yhat_meta - yhat_group) >= 0 by construction of the envelope
    # MTR_i = exp(-gap_i); TE_meta_i = TE_group_i * MTR_i
    gap <- S * (yhat_meta - yhat_group)
    mtr <- exp(-pmax(gap, 0))
    teMeta <- teGroup * mtr
    list(teMeta = teMeta, mtr = mtr)
  } else if (sfaApproach == "huang") {
    # Huang (2014): second-stage SFA DV = group frontier fitted value.
    # u_meta captures how far the group frontier lies below the metafrontier.
    # MTR_i = exp(-u_meta_i) = teBC from the second-stage SFA (Huang Eq. 9).
    # TE_meta_i = TE_group_i * MTR_i  (Huang Eq. 8).
    mtr <- teMeta_sfa # exp(-u_meta) from second-stage SFA
    teMeta <- teGroup * mtr # obs-to-metafrontier TE
    list(teMeta = teMeta, mtr = mtr)
  } else {
    # O'Donnell (2008): second-stage SFA DV = LP envelope.
    # TE_meta is the BC efficiency from that SFA relative to the metafrontier.
    # MTR_i = TE_meta_i / TE_group_i.
    teMeta <- teMeta_sfa
    mtr <- teMeta / teGroup
    n_neg <- sum(!is.na(mtr) & mtr < 0)
    n_gt1 <- sum(!is.na(mtr) & mtr > 1)
    if (n_neg > 0 || n_gt1 > 0) {
      warning(
        sprintf(
          "%d MTR value(s) > 1 detected in O'Donnell SFA approach. ", n_gt1
        ),
        "This typically occurs when the second-stage SFA estimates near-zero ",
        "inefficiency (sigma_u -> 0), causing TE_meta ~= 1 and MTR = TE_meta/TE_group > 1. ",
        "Consider using metaMethod='lp' or sfaApproach='huang' instead.",
        call. = FALSE
      )
    }
    list(teMeta = teMeta, mtr = mtr)
  }
}
