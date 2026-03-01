################################################################################
#                                                                              #
# R internal helper functions for stochastic metafrontier analysis             #
# sfaR package                                                                 #
#                                                                              #
################################################################################

#------------------------------------------------------------------------------#
# Metafrontier estimation helpers                                              #
#------------------------------------------------------------------------------#
.pchibarsq <- function(p, df = 1, mix = 0.5, lower.tail = TRUE, log.p = FALSE) {
  df <- rep(df, length.out = length(p))
  mix <- rep(mix, length.out = length(p))
  c1 <- ifelse(df == 1, if (lower.tail) {
    1
  } else {
    0
  }, pchisq(p, df - 1, lower.tail = lower.tail))
  c2 <- pchisq(p, df, lower.tail = lower.tail)
  r <- mix * c1 + (1 - mix) * c2
  if (log.p) {
    log(r)
  } else {
    r
  }
}

#' @importFrom stats pchisq qchisq uniroot
#' @noRd
.qchibarsq <- function(q, df = 1, mix = 0.5) {
  n <- max(length(q), length(df), length(mix))
  df <- rep(df, length.out = n)
  mix <- rep(mix, length.out = n)
  q <- rep(q, length.out = n)
  tmpf2 <- function(q, df, mix) {
    if (df > 1) {
      tmpf <- function(x) {
        .pchibarsq(x, df, mix) - q
      }
      uniroot(tmpf, lower = qchisq(q, df - 1), upper = qchisq(
        q,
        df
      ))$root
    } else {
      newq <- (q - mix) / (1 - mix)
      ifelse(newq < 0, 0, qchisq(newq, df = 1))
    }
  }
  mapply(tmpf2, q, df, mix)
}


# Pretty-print name for the metafrontier method -----------
#' @param metaMethod character string for metafrontier method
#' @param sfaApproach character string for SFA approach ("ordonnell" or "huang")
#' @noRd
mfauxdist <- function(metaMethod, sfaApproach = "huang") {
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

# Compute metatechnology ratio (MTR) and metafrontier efficiencies ----------------
#' @param yhat_group N-vector of group frontier fitted values
#' @param yhat_meta N-vector of metafrontier fitted values
#' @param teGroup_BC N-vector of group efficiency (teBC from group SFA)
#' @param teGroup_JLMS N-vector of group efficiency (teJLMS from group SFA)
#' @param uGroup N-vector of group inefficiency
#' @param S integer 1 or -1
#' @param metaMethod character "lp", "qp", or "sfa"
#' @param sfaApproach character "ordonnell" or "huang" (used when metaMethod="sfa")
#' @param effMeta_sfa optional data.frame of efficiency estimates from second-stage SFA
#' @noRd
compute_mtr <- function(yhat_group, yhat_meta, teGroup_BC, teGroup_JLMS, uGroup, S,
                        metaMethod = "lp", sfaApproach = "huang",
                        effMeta_sfa = NULL) {
  if (metaMethod %in% c("lp", "qp")) {
    gap <- S * (yhat_meta - yhat_group)
    mtr <- exp(-pmax(gap, 0))
    teMeta_BC <- teGroup_BC * mtr
    teMeta_JLMS <- teGroup_JLMS * mtr
    u_meta <- uGroup - log(mtr)
    list(teMeta_BC = teMeta_BC, teMeta_JLMS = teMeta_JLMS, mtr_BC = mtr, mtr_JLMS = mtr, u_meta = u_meta)
  } else if (sfaApproach == "huang") {
    mtr_BC <- effMeta_sfa$teBC
    mtr_JLMS <- effMeta_sfa$teJLMS
    u_mtr <- effMeta_sfa$u
    teMeta_BC <- teGroup_BC * mtr_BC
    teMeta_JLMS <- teGroup_JLMS * mtr_JLMS
    u_meta <- uGroup + u_mtr
    list(teMeta_BC = teMeta_BC, teMeta_JLMS = teMeta_JLMS, mtr_BC = mtr_BC, mtr_JLMS = mtr_JLMS, u_meta = u_meta)
  } else {
    # ordonnell
    teMeta_BC <- effMeta_sfa$teBC
    teMeta_JLMS <- effMeta_sfa$teJLMS
    u_meta <- effMeta_sfa$u
    mtr_BC <- teMeta_BC / teGroup_BC
    mtr_JLMS <- teMeta_JLMS / teGroup_JLMS

    n_gt1 <- sum(!is.na(mtr_BC) & mtr_BC > 1)
    if (n_gt1 > 0) {
      warning(
        sprintf("%d MTR value(s) > 1 detected in O'Donnell SFA approach. ", n_gt1),
        "This typically occurs when the second-stage SFA estimates near-zero ",
        "inefficiency (sigma_u -> 0), causing TE_meta ~= 1 and MTR = TE_meta/TE_group > 1. ",
        "Consider using metaMethod='lp' or sfaApproach='huang' instead.",
        call. = FALSE
      )
    }
    list(teMeta_BC = teMeta_BC, teMeta_JLMS = teMeta_JLMS, mtr_BC = mtr_BC, mtr_JLMS = mtr_JLMS, u_meta = u_meta)
  }
}
