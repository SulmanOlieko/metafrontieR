################################################################################
#                                                                              #
# R functions for the metafrontieR package                                     #
#                                                                              #
################################################################################

#------------------------------------------------------------------------------#
# Summary of optimization objects                                              #
# Models:                                                                      #
#        -Stochastic metafrontier analysis and variants                        #
# Data:  Cross sectional & Pooled data                                         #
#------------------------------------------------------------------------------#

#' Summary of results for stochastic metafrontier models
#'
#' Create and print summary results for stochastic metafrontier models returned by
#' \code{\link{sfametafrontier}}.
#'
#' @param object An object of class \code{'sfametafrontier'} returned by the
#' function \code{\link{sfametafrontier}}.
#' @param ... Currently ignored.
#' @param x An object of class \code{'summary.sfametafrontier'}.
#' @param digits Numeric. Number of digits displayed in values.
#'
#' @name summary
#'
#' @return The \code{\link{summary}} method returns a list of class
#' \code{'summary.sfametafrontier'}
#' that contains the same elements as an object returned by \code{\link{sfametafrontier}}
#' with the following additional elements:
#'
#' \item{AIC}{Akaike information criterion.}
#'
#' \item{BIC}{Bayesian information criterion.}
#'
#' \item{HQIC}{Hannan-Quinn information criterion.}
#'
#' \item{metaRes}{Matrix of metafrontier estimates, their standard errors, z-values,
#' and asymptotic P-values.}
#'
#' \item{effStats}{A list of efficiency statistics including group means and
#' class membership probabilities.}
#'
#' \item{grpSummaries}{A list of summary objects for each group model.}
#'
#' @seealso \code{\link{sfametafrontier}}, for the stochastic metafrontier analysis model
#' fitting function for cross-sectional or pooled data.
#'
#' @keywords methods summary
#'
#' @aliases summary.sfametafrontier
#' @export

summary.sfametafrontier <- function(object, ...) {
  # Information criteria
  object$AIC <- -2 * object$mlLoglik + 2 * object$nParm
  object$BIC <- -2 * object$mlLoglik + log(object$Nobs) * object$nParm
  object$HQIC <- -2 * object$mlLoglik + 2 * log(log(object$Nobs)) * object$nParm

  # Per-group summary objects (typed: summary.sfacross / sfalcmcross / sfaselectioncross)
  # For lcmNoGroup: summarise lcmObj once (all entries are the same model)
  if (isTRUE(object$lcmNoGroup)) {
    lcm_sm <- summary(object$lcmObj)
    object$grpSummaries <- setNames(
      rep(list(lcm_sm), object$nGroups),
      object$groups
    )
  } else {
    object$grpSummaries <- lapply(object$groupModels, summary)
  }

  # Metafrontier coefficient table (QP and SFA methods)
  if (!is.null(object$metaFrontierParam)) {
    if (!is.null(object$metaFrontierVcov)) {
      se <- sqrt(diag(object$metaFrontierVcov))
      zval <- object$metaFrontierParam / se
      pval <- 2 * pnorm(-abs(zval))
      metaRes <- cbind(
        Estimate = object$metaFrontierParam,
        `Std. Error` = se,
        `z value` = zval,
        `Pr(>|z|)` = pval
      )
    } else {
      metaRes <- matrix(
        object$metaFrontierParam,
        ncol = 1,
        dimnames = list(names(object$metaFrontierParam), "Estimate")
      )
    }
    object$metaRes <- metaRes
  }

  # Efficiency statistics (computed once for the summary panel)
  effStats <- tryCatch(
    {
      eff <- efficiencies(object)
      grpVar <- object$dataTable$.mf_group

      buildRow <- function(g) {
        idx <- which(grpVar == g)
        nTE <- sum(!is.na(eff$TE_group_BC[idx]))
        nMTR <- sum(!is.na(eff$MTR_BC[idx]))
        c(
          N_group = length(idx),
          N_valid = nTE,
          TE_group_BC = if (nTE > 0) mean(eff$TE_group_BC[idx], na.rm = TRUE) else NA_real_,
          TE_group_JLMS = if (nTE > 0) mean(eff$TE_group_JLMS[idx], na.rm = TRUE) else NA_real_,
          TE_meta_BC = if (nMTR > 0) mean(eff$TE_meta_BC[idx], na.rm = TRUE) else NA_real_,
          TE_meta_JLMS = if (nMTR > 0) mean(eff$TE_meta_JLMS[idx], na.rm = TRUE) else NA_real_,
          MTR_BC = if (nMTR > 0) mean(eff$MTR_BC[idx], na.rm = TRUE) else NA_real_,
          MTR_JLMS = if (nMTR > 0) mean(eff$MTR_JLMS[idx], na.rm = TRUE) else NA_real_
        )
      }
      statMat <- do.call(rbind, lapply(object$groups, buildRow))
      rownames(statMat) <- object$groups

      # LCM: posterior class membership proportions
      postProb <- NULL
      if (object$groupType == "sfalcmcross") {
        # For lcmNoGroup: one pooled model -> compute once and show for all data
        # For grouped LCM: compute per group model
        if (isTRUE(object$lcmNoGroup)) {
          postProb <- list()
          lcm_m <- object$lcmObj
          nc <- lcm_m$nClasses
          eff_g <- tryCatch(efficiencies(lcm_m), error = function(e) NULL)
          if (!is.null(eff_g) && "Group_c" %in% names(eff_g)) {
            assign_tab <- table(eff_g$Group_c)
            assign_prop <- as.numeric(assign_tab) / sum(assign_tab)
            pp_cols <- grep(
              "^PosteriorProb_c[0-9]+$",
              names(eff_g),
              value = TRUE
            )
            mean_pp <- if (length(pp_cols) > 0) {
              colMeans(eff_g[, pp_cols, drop = FALSE], na.rm = TRUE)
            } else {
              rep(NA_real_, nc)
            }
            cls_labels <- paste0("Class ", seq_len(nc))
            pp_df <- data.frame(
              `% assigned` = formatC(
                assign_prop * 100,
                digits = 1,
                format = "f"
              ),
              `Mean post. prob.` = formatC(mean_pp, digits = 3, format = "f"),
              row.names = cls_labels,
              check.names = FALSE
            )
            # Store once - we'll display it once in the print method
            postProb[["pooled"]] <- pp_df
          }
        } else {
          postProb <- lapply(object$groups, function(g) {
            m <- object$groupModels[[g]]
            nc <- m$nClasses
            eff_g <- tryCatch(efficiencies(m), error = function(e) NULL)
            if (is.null(eff_g) || !("Group_c" %in% names(eff_g))) {
              return(NULL)
            }
            assign_tab <- table(eff_g$Group_c)
            assign_prop <- as.numeric(assign_tab) / sum(assign_tab)
            pp_cols <- grep(
              "^PosteriorProb_c[0-9]+$",
              names(eff_g),
              value = TRUE
            )
            mean_pp <- if (length(pp_cols) > 0) {
              colMeans(eff_g[, pp_cols, drop = FALSE], na.rm = TRUE)
            } else {
              rep(NA_real_, nc)
            }
            cls_labels <- paste0("Class ", seq_len(nc))
            data.frame(
              `% assigned` = formatC(
                assign_prop * 100,
                digits = 1,
                format = "f"
              ),
              `Mean post. prob.` = formatC(mean_pp, digits = 3, format = "f"),
              row.names = cls_labels,
              check.names = FALSE
            )
          })
          names(postProb) <- object$groups
        }
      }

      list(statMat = statMat, postProb = postProb)
    },
    error = function(e) NULL
  )

  object$effStats <- effStats
  class(object) <- "summary.sfametafrontier"
  return(object)
}

# ---- Helper: compute delta-method variance statistics table ----
# Returns a data.frame with Estimate / SE / z / p for:
#   sigma_u, sigma_v, sigma, gamma, lambda (for hnormal/tnormal)
# Falls back to NA-filled table if hessian not available.
.mf_var_stats <- function(sm, digits = 3) {
  # Retrieve fields stored by summary.sfacross / summary.sfaselectioncross
  # (sfalcmcross summary doesn't carry these; handled separately)
  su2 <- sm$sigmauSq
  sv2 <- sm$sigmavSq
  Eu <- sm$Eu
  Exp <- sm$Expu

  if (is.null(su2) || is.null(sv2) || is.na(su2) || is.na(sv2)) {
    return(NULL)
  }

  su <- sqrt(su2)
  sv <- sqrt(sv2)
  sig <- sqrt(su2 + sv2)
  gam <- su2 / (su2 + sv2)
  lam <- su / sv

  vals <- c(
    `Sigma-squared(u)` = su2,
    `Sigma(u)` = su,
    `Sigma-squared(v)` = sv2,
    `Sigma(v)` = sv,
    `Sigma = Sqrt[(s^2(u)+s^2(v))]` = sig,
    `Gamma = sigma(u)^2/sigma^2` = gam,
    `Lambda = sigma(u)/sigma(v)` = lam
  )

  # Try delta-method SEs if invHessian is available
  ih <- sm$invHessian
  if (!is.null(ih) && is.matrix(ih)) {
    # Identify parameter positions from sm fields
    nX <- sm$nXvar
    nU <- sm$nuZUvar
    nV <- sm$nvZVvar
    np <- nrow(ih)

    # log sigma_u^2 = Zu_intercept (position nX+1 assuming single intercept)
    # log sigma_v^2 = Zv_intercept (position nX+nU+1)
    pos_lsu2 <- nX + 1L # log(sigma_u^2) param
    pos_lsv2 <- nX + nU + 1L # log(sigma_v^2) param

    if (pos_lsu2 <= np && pos_lsv2 <= np) {
      grad <- function(nm) {
        g <- numeric(np)
        if (nm == "Sigma(u)") {
          g[pos_lsu2] <- su / 2 # d(su)/d(log su2) = su/2
        } else if (nm == "Sigma(v)") {
          g[pos_lsv2] <- sv / 2
        } else if (nm == "Sigma = Sqrt[(s^2(u)+s^2(v))]") {
          g[pos_lsu2] <- su2 / (2 * sig) # d(sig)/d(log su2) = su2/(2*sig)
          g[pos_lsv2] <- sv2 / (2 * sig)
        } else if (nm == "Sigma-squared(u)") {
          g[pos_lsu2] <- su2
        } else if (nm == "Sigma-squared(v)") {
          g[pos_lsv2] <- sv2
        } else if (nm == "Gamma = sigma(u)^2/sigma^2") {
          # gamma = su2/(su2+sv2)
          # d(gamma)/d(log su2) = su2 * sv2/(su2+sv2)^2 = gamma*(1-gamma)
          g[pos_lsu2] <- gam * (1 - gam)
          g[pos_lsv2] <- -gam * (1 - gam)
        } else if (nm == "Lambda = sigma(u)/sigma(v)") {
          # lambda = su/sv => d/d(log su2) = lambda/2; d/d(log sv2) = -lambda/2
          g[pos_lsu2] <- lam / 2
          g[pos_lsv2] <- -lam / 2
        }
        g
      }
      se_vec <- setNames(
        sapply(names(vals), function(nm) {
          g <- grad(nm)
          sqrt(max(0, t(g) %*% ih %*% g))
        }),
        names(vals)
      )
    } else {
      se_vec <- rep(NA_real_, length(vals))
    }
  } else {
    se_vec <- rep(NA_real_, length(vals))
  }
  names(se_vec) <- names(vals)

  zv <- ifelse(se_vec > 0, vals / se_vec, NA_real_)
  pv <- ifelse(!is.na(zv), 2 * pnorm(-abs(zv)), NA_real_)

  tab <- cbind(
    Estimate = vals,
    `Std. Error` = se_vec,
    `z value` = zv,
    `Pr(>|z|)` = pv
  )

  # Append E[u] and E[exp(-u)] as Estimates only (no SE available from stored fields)
  extras <- c()
  if (!is.null(Eu) && !is.na(Eu)) {
    extras <- c(extras, `E[u]` = Eu)
  }
  if (!is.null(Exp) && !is.na(Exp)) {
    extras <- c(extras, `E[exp(-u)]` = Exp)
  }
  if (length(extras) > 0) {
    extra_tab <- cbind(
      Estimate = extras,
      `Std. Error` = NA_real_,
      `z value` = NA_real_,
      `Pr(>|z|)` = NA_real_
    )
    tab <- rbind(tab, extra_tab)
  }

  tab
}

# print for summary.sfametafrontier ----------
#' @rdname summary
#' @aliases print.summary.sfametafrontier
#' @export
print.summary.sfametafrontier <- function(
  x,
  digits = max(3, getOption("digits") - 2),
  ...
) {
  lengthSum <- 60
  sep <- paste0(rep("-", lengthSum), collapse = "")
  sep2 <- paste0(rep("=", lengthSum), collapse = "")

  # Helper to print the tests
  .print_tests <- function(m) {
    if (is.null(m$df) || is.null(m$olsLoglik) || is.null(m$chisq) || is.null(m$CoelliM3Test)) {
      return()
    }
    cat("-----[ Tests vs. No Inefficiency ]-----\n")
    cat("Likelihood Ratio Test of Inefficiency\n")
    cat(
      "Deg. freedom for inefficiency model",
      paste0(
        rep(" ", lengthSum - 2 - nchar("Deg. freedom for inefficiency model") - nchar(formatC(m$df, digits = digits, format = "d"))),
        collapse = ""
      ),
      formatC(m$df, digits = digits, format = "d"),
      "\n"
    )
    cat(
      "Log Likelihood for OLS Log(H0) = ",
      paste0(
        rep(" ", lengthSum - 2 - nchar("Log Likelihood for OLS Log(H0) = ") - nchar(formatC(m$olsLoglik, digits = digits, format = "f"))),
        collapse = ""
      ),
      formatC(m$olsLoglik, digits = digits, format = "f"), "\n"
    )
    cat("LR statistic: \n")
    cat(
      "Chisq = 2*[LogL(H0)-LogL(H1)]  = ",
      paste0(
        rep(" ", lengthSum - 2 - nchar("Chisq = 2*[LogL(H0)-LogL(H1)]  = ") - nchar(formatC(m$chisq, digits = digits, format = "f"))),
        collapse = ""
      ),
      formatC(m$chisq, digits = digits, format = "f"), "\n"
    )
    cat(
      "Kodde-Palm C*:       95%:", formatC(.qchibarsq(0.95, df = m$df), digits = digits, format = "f"),
      paste0(
        rep(" ", lengthSum - 2 - nchar("Kodde-Palm C*:       95%:") - nchar(formatC(.qchibarsq(0.95, df = m$df), digits = digits, format = "f")) - nchar(formatC(.qchibarsq(0.99, df = m$df), digits = digits, format = "f")) - nchar("99%") - 3),
        collapse = ""
      ),
      "99%:", formatC(.qchibarsq(0.99, df = m$df), digits = digits, format = "f"), "\n"
    )
    cat("Coelli (1995) skewness test on OLS residuals\n")
    cat(
      "M3T: z                         = ",
      paste0(
        rep(" ", lengthSum - 2 - nchar("M3T: z                         = ") - nchar(formatC(m$CoelliM3Test[1], digits = digits, format = "f"))),
        collapse = ""
      ),
      formatC(m$CoelliM3Test[1], digits = digits, format = "f"), "\n"
    )
    cat(
      "M3T: p.value                   = ",
      paste0(
        rep(" ", lengthSum - 2 - nchar("M3T: p.value                   = ") - nchar(formatC(m$CoelliM3Test[2], digits = digits, format = "f"))),
        collapse = ""
      ),
      formatC(m$CoelliM3Test[2], digits = digits, format = "f"), "\n"
    )
  }

  # ---- Header ----
  cat(sep2, "\n")
  cat("Stochastic Metafrontier Analysis\n")
  cat("Metafrontier method:", mfauxdist(x$metaMethod, x$sfaApproach), "\n")
  cat(x$typeSfa, "\n")
  if (!is.null(x$sfaApproach) && x$metaMethod == "sfa") {
    cat("SFA approach       :", x$sfaApproach, "\n")
  }
  grp_appr <- switch(x$groupType,
    sfacross = "Stochastic Frontier Analysis",
    sfaselectioncross = "Sample Selection Stochastic Frontier Analysis",
    sfalcmcross = "Latent Class Stochastic Frontier Analysis",
    x$groupType
  )
  cat("Group approach     :", grp_appr, "\n")
  cat("Group estimator    :", x$groupType, "\n")
  if (!is.null(x$groupModels[[1]]$optType)) {
    cat("Group optim solver :", x$groupModels[[1]]$optType[[1]], "\n")
  }
  if (isTRUE(x$lcmNoGroup)) {
    cat("  (Pooled LCM - latent classes used as groups)\n")
  }
  cat(
    "Groups (",
    length(x$groups),
    "):",
    paste(x$groups, collapse = ", "),
    "\n"
  )
  cat("Total observations :", x$Nobs, "\n")
  cat("Distribution       :", x$udist, "\n")
  cat(sep2, "\n\n")

  # ---- Per-group sections ----
  # For lcmNoGroup: print the SINGLE pooled LCM model only once, then efficiency by class
  if (isTRUE(x$lcmNoGroup)) {
    lcm_m <- x$lcmObj
    lcm_sm <- x$grpSummaries[[1]] # same object for all classes
    mlR <- lcm_sm$mlRes
    nc <- lcm_m$nClasses
    nX_lcm <- lcm_m$nXvar
    nU_lcm <- lcm_m$nuZUvar
    nV_lcm <- lcm_m$nvZVvar
    nZH <- lcm_m$nZHvar

    cat(sep, "\n")
    cat(sprintf(
      "Pooled LCM (%d classes) on all data (N = %d)  Log-likelihood: %s\n",
      nc,
      nrow(lcm_m$dataTable),
      formatC(lcm_m$mlLoglik, digits = digits, format = "f")
    ))
    cat(sep, "\n")

    for (k in seq_len(nc)) {
      cat(sprintf("\n  -- Latent Class %d --\n", k))
      s_b <- (k - 1) * (nX_lcm + nU_lcm + nV_lcm) + 1
      e_b <- (k - 1) * (nX_lcm + nU_lcm + nV_lcm) + nX_lcm
      s_u <- (k - 1) * (nX_lcm + nU_lcm + nV_lcm) + nX_lcm + 1
      e_u <- (k - 1) * (nX_lcm + nU_lcm + nV_lcm) + nX_lcm + nU_lcm
      s_v <- (k - 1) * (nX_lcm + nU_lcm + nV_lcm) + nX_lcm + nU_lcm + 1
      e_v <- (k - 1) * (nX_lcm + nU_lcm + nV_lcm) + nX_lcm + nU_lcm + nV_lcm
      cat("  Frontier:\n")
      printCoefmat(
        mlR[s_b:e_b, , drop = FALSE],
        P.values = TRUE,
        digits = digits,
        signif.legend = FALSE,
        na.print = "NA"
      )
      cat("  Var(u):\n")
      printCoefmat(
        mlR[s_u:e_u, , drop = FALSE],
        P.values = TRUE,
        digits = digits,
        signif.legend = FALSE,
        na.print = "NA"
      )
      cat("  Var(v):\n")
      printCoefmat(
        mlR[s_v:e_v, , drop = FALSE],
        P.values = TRUE,
        digits = digits,
        signif.legend = FALSE,
        na.print = "NA"
      )
      # Per-class variance derived stats
      vs <- tryCatch(
        {
          # Extract class-k sigma_u^2 from Zu intercept
          # Interleaved position
          Zu_int <- lcm_m$mlParam[
            (k - 1) * (nX_lcm + nU_lcm + nV_lcm) + nX_lcm + 1
          ]
          Zv_int <- lcm_m$mlParam[
            (k - 1) * (nX_lcm + nU_lcm + nV_lcm) + nX_lcm + nU_lcm + 1
          ]
          su2_k <- exp(Zu_int)
          sv2_k <- exp(Zv_int)
          su_k <- sqrt(su2_k)
          sv_k <- sqrt(sv2_k)
          sig_k <- sqrt(su2_k + sv2_k)
          cat(sprintf(
            "  Sigma_u=%.4f  Sigma_v=%.4f  Sigma=%.4f  Gamma=%.4f  Lambda=%.4f\n",
            su_k,
            sv_k,
            sig_k,
            su2_k / (su2_k + sv2_k),
            su_k / sv_k
          ))
          invisible(NULL)
        },
        error = function(e) NULL
      )
    }
    # Class membership block
    s_th <- nc * (nX_lcm + nU_lcm + nV_lcm) + 1
    e_th <- nc * (nX_lcm + nU_lcm + nV_lcm) + (nc - 1) * nZH
    if (e_th >= s_th && nrow(mlR) >= e_th) {
      cat("\n  -- Class Membership (logit) --\n")
      printCoefmat(
        mlR[s_th:e_th, , drop = FALSE],
        P.values = TRUE,
        digits = digits,
        signif.legend = TRUE,
        na.print = "NA"
      )
    }
    .print_tests(lcm_sm)
    if (!is.null(lcm_m$optStatus)) {
      cat("Log likelihood status:", lcm_m$optStatus, "\n")
    }
    cat("\n")
  } else {
    # Normal per-group loop
    for (g in x$groups) {
      m <- x$groupModels[[g]]
      sm <- x$grpSummaries[[g]]
      Ng <- nrow(m$dataTable)
      mlR <- sm$mlRes
      cat(sep, "\n")
      cat(
        "Group:",
        g,
        sprintf(
          "(N = %d)  Log-likelihood: %s\n",
          Ng,
          formatC(m$mlLoglik, digits = digits, format = "f")
        )
      )
      cat(sep, "\n")

      if (x$groupType == "sfacross") {
        # ------- standard sfacross: flat table -------
        if (!is.null(mlR)) {
          printCoefmat(
            mlR,
            P.values = TRUE,
            digits = digits,
            signif.legend = TRUE,
            na.print = "NA"
          )
        }
        # Variance derived stats with delta-method SEs
        vs <- tryCatch(.mf_var_stats(sm, digits), error = function(e) NULL)
        if (!is.null(vs)) {
          cat("\n  Variance & Efficiency Statistics (delta-method SEs):\n")
          printCoefmat(
            vs,
            P.values = TRUE,
            digits = digits,
            signif.legend = FALSE,
            na.print = "-"
          )
        }
      } else if (x$groupType == "sfalcmcross") {
        # ------- LCM: one block per class -------
        nc <- m$nClasses
        nX <- m$nXvar
        nU <- m$nuZUvar
        nV <- m$nvZVvar
        nZH <- m$nZHvar
        for (k in seq_len(nc)) {
          cat(sprintf("\n  -- Latent Class %d --\n", k))
          s_b <- (k - 1) * (nX + nU + nV) + 1
          e_b <- (k - 1) * (nX + nU + nV) + nX
          s_u <- (k - 1) * (nX + nU + nV) + nX + 1
          e_u <- (k - 1) * (nX + nU + nV) + nX + nU
          s_v <- (k - 1) * (nX + nU + nV) + nX + nU + 1
          e_v <- (k - 1) * (nX + nU + nV) + nX + nU + nV
          cat("  Frontier:\n")
          printCoefmat(
            mlR[s_b:e_b, , drop = FALSE],
            P.values = TRUE,
            digits = digits,
            signif.legend = FALSE,
            na.print = "NA"
          )
          cat("  Var(u):\n")
          printCoefmat(
            mlR[s_u:e_u, , drop = FALSE],
            P.values = TRUE,
            digits = digits,
            signif.legend = FALSE,
            na.print = "NA"
          )
          cat("  Var(v):\n")
          printCoefmat(
            mlR[s_v:e_v, , drop = FALSE],
            P.values = TRUE,
            digits = digits,
            signif.legend = FALSE,
            na.print = "NA"
          )
          # Per-class derived variance stats (point estimates only from mlParam)
          tryCatch(
            {
              Zu_int <- m$mlParam[(k - 1) * (nX + nU + nV) + nX + 1]
              Zv_int <- m$mlParam[(k - 1) * (nX + nU + nV) + nX + nU + 1]
              su2_k <- exp(Zu_int)
              sv2_k <- exp(Zv_int)
              su_k <- sqrt(su2_k)
              sv_k <- sqrt(sv2_k)
              sig_k <- sqrt(su2_k + sv2_k)
              cat(sprintf(
                "  Sigma_u=%.4f  Sigma_v=%.4f  Sigma=%.4f  Gamma=%.4f  Lambda=%.4f\n",
                su_k,
                sv_k,
                sig_k,
                su2_k / (su2_k + sv2_k),
                su_k / sv_k
              ))
            },
            error = function(e) NULL
          )
        }
        s_th <- nc * (nX + nU + nV) + 1
        e_th <- nc * (nX + nU + nV) + (nc - 1) * nZH
        if (e_th >= s_th && nrow(mlR) >= e_th) {
          cat("\n  -- Class Membership (logit) --\n")
          printCoefmat(
            mlR[s_th:e_th, , drop = FALSE],
            P.values = TRUE,
            digits = digits,
            signif.legend = TRUE,
            na.print = "NA"
          )
        }
      } else if (x$groupType == "sfaselectioncross") {
        nX <- m$nXvar
        nU <- m$nuZUvar
        nV <- m$nvZVvar
        e_b <- nX
        e_u <- nX + nU
        e_v <- nX + nU + nV
        nr <- nrow(mlR)
        cat("  Frontier equation:\n")
        printCoefmat(
          mlR[1:e_b, , drop = FALSE],
          P.values = TRUE,
          digits = digits,
          signif.legend = FALSE,
          na.print = "NA"
        )
        cat("  Var(u) parameters:\n")
        printCoefmat(
          mlR[(e_b + 1):e_u, , drop = FALSE],
          P.values = TRUE,
          digits = digits,
          signif.legend = FALSE,
          na.print = "NA"
        )
        cat("  Var(v) parameters:\n")
        printCoefmat(
          mlR[(e_u + 1):e_v, , drop = FALSE],
          P.values = TRUE,
          digits = digits,
          signif.legend = FALSE,
          na.print = "NA"
        )
        if (nr > e_v) {
          cat("  Selection bias parameter (rho):\n")
          printCoefmat(
            mlR[(e_v + 1):nr, , drop = FALSE],
            P.values = TRUE,
            digits = digits,
            signif.legend = TRUE,
            na.print = "NA"
          )
        }
        # Variance derived stats with delta-method SEs
        vs <- tryCatch(.mf_var_stats(sm, digits), error = function(e) NULL)
        if (!is.null(vs)) {
          cat("\n  Variance & Efficiency Statistics (delta-method SEs):\n")
          printCoefmat(
            vs,
            P.values = TRUE,
            digits = digits,
            signif.legend = FALSE,
            na.print = "-"
          )
        }
      }
      .print_tests(sm)
      if (!is.null(m$optStatus)) {
        cat("Log likelihood status:", m$optStatus, "\n")
      }
      cat("\n")
    }
  } # end if/else lcmNoGroup

  # ---- Metafrontier coefficients ----
  cat(sep, "\n")
  cat("Metafrontier Coefficients (", x$metaMethod, "):\n", sep = "")
  if (!is.null(x$metaSfaObj) && !is.null(x$metaSfaObj$optType)) {
    cat("Meta-optim solver  :", x$metaSfaObj$optType[[1]], "\n")
  }
  if (!is.null(x$metaRes)) {
    printCoefmat(
      x$metaRes,
      P.values = ncol(x$metaRes) == 4,
      digits = digits,
      signif.legend = TRUE,
      na.print = "NA"
    )
    # Metafrontier variance stats (SFA method only)
    if (x$metaMethod == "sfa" && !is.null(x$metaSfaObj)) {
      vs_meta <- tryCatch(
        .mf_var_stats(summary(x$metaSfaObj), digits),
        error = function(e) NULL
      )
      if (!is.null(vs_meta)) {
        cat("\n  Meta-frontier Variance & Efficiency Statistics:\n")
        printCoefmat(
          vs_meta,
          P.values = TRUE,
          digits = digits,
          signif.legend = FALSE,
          na.print = "-"
        )
      }
      meta_sm <- suppressWarnings(tryCatch(summary(x$metaSfaObj), error = function(e) NULL))
      if (!is.null(meta_sm)) .print_tests(meta_sm)
      if (!is.null(x$metaSfaObj$optStatus)) {
        cat("Log likelihood status:", x$metaSfaObj$optStatus, "\n")
      }
    }
  } else {
    cat("  (LP: deterministic envelope - no estimated parameters)\n")
  }
  cat("\n")

  # ---- Efficiency statistics panel ----
  if (!is.null(x$effStats)) {
    cat(sep, "\n")
    cat("Efficiency Statistics (group means):\n")
    cat(sep, "\n")
    sm_eff <- x$effStats$statMat
    out <- data.frame(
      N_obs = formatC(sm_eff[, "N_group"], format = "d"),
      N_valid = formatC(sm_eff[, "N_valid"], format = "d"),
      TE_group_BC = formatC(sm_eff[, "TE_group_BC"], digits = digits, format = "f"),
      TE_group_JLMS = formatC(sm_eff[, "TE_group_JLMS"], digits = digits, format = "f"),
      TE_meta_BC = formatC(sm_eff[, "TE_meta_BC"], digits = digits, format = "f"),
      TE_meta_JLMS = formatC(sm_eff[, "TE_meta_JLMS"], digits = digits, format = "f"),
      MTR_BC = formatC(sm_eff[, "MTR_BC"], digits = digits, format = "f"),
      MTR_JLMS = formatC(sm_eff[, "MTR_JLMS"], digits = digits, format = "f"),
      row.names = rownames(sm_eff),
      stringsAsFactors = FALSE
    )
    print(out)
    ov <- colMeans(
      sm_eff[, c("TE_group_BC", "TE_group_JLMS", "TE_meta_BC", "TE_meta_JLMS", "MTR_BC", "MTR_JLMS"), drop = FALSE],
      na.rm = TRUE
    )
    cat(sprintf(
      "\nOverall:\nTE_group_BC=%.4f  TE_group_JLMS=%.4f\nTE_meta_BC=%.4f   TE_meta_JLMS=%.4f\nMTR_BC=%.4f     MTR_JLMS=%.4f\n",
      ov["TE_group_BC"], ov["TE_group_JLMS"],
      ov["TE_meta_BC"], ov["TE_meta_JLMS"],
      ov["MTR_BC"], ov["MTR_JLMS"]
    ))

    # LCM: posterior class proportions
    pp_list <- x$effStats$postProb
    if (!is.null(pp_list) && any(!sapply(pp_list, is.null))) {
      cat("\n")
      cat(sep, "\n")
      if (isTRUE(x$lcmNoGroup)) {
        cat("Posterior Class Membership (pooled LCM):\n")
        cat(sep, "\n")
        pp <- pp_list[["pooled"]]
        if (!is.null(pp) && is.data.frame(pp)) print(pp)
      } else {
        cat("Posterior Class Membership (by group):\n")
        cat(sep, "\n")
        for (g in x$groups) {
          pp <- pp_list[[g]]
          if (!is.null(pp) && is.data.frame(pp)) {
            cat("  Group:", g, "\n")
            print(pp)
            cat("\n")
          }
        }
      }
    }
  }

  # ---- Information criteria ----
  cat(sep, "\n")
  cat("Total Log-likelihood:", round(x$mlLoglik, digits), "\n")
  cat(
    "AIC:",
    round(x$AIC, digits),
    "  BIC:",
    round(x$BIC, digits),
    "  HQIC:",
    round(x$HQIC, digits),
    "\n"
  )
  if (!is.null(x$tests)) {
    cat(sep, "\n")
    cat("Tests vs. No Inefficiency:\n")
    printCoefmat(
      x$tests,
      P.values = TRUE,
      has.Pvalue = TRUE,
      digits = digits,
      signif.legend = TRUE,
      na.print = "NA"
    )
  }
  cat(sep, "\n")
  if (!is.null(x$optStatus)) {
    cat("Log likelihood status:", x$optStatus, "\n")
  }
  cat(x$mlDate, "\n")
  invisible(x)
}
