################################################################################
#                                                                              #
# R functions for the metafrontieR package                                             #
#                                                                              #
################################################################################

#------------------------------------------------------------------------------#
# Information Criteria extraction                                              #
# Models:                                                                      #
#           -Stochastic metafrontier analysis                                  #
#           -Latent class stochastic metafrontier analysis                     #
#           -Sample selection correction for stochastic metafrontier model     #
# Data: Cross sectional data & Pooled data                                     #
#------------------------------------------------------------------------------#

#' Extract information criteria of stochastic metafrontier models
#'
#' \code{\link{ic}} returns information criterion from stochastic
#' metafrontier models estimated with \code{\link{sfametafrontier}}.
#'
#' The different information criteria are computed as follows: \itemize{ \item
#' AIC: \eqn{-2 \log{LL} + 2 * K} \item BIC: \eqn{-2 \log{LL} + \log{N} * K}
#' \item HQIC: \eqn{-2 \log{LL} + 2 \log{\left[\log{N}\right]} * K} } where
#' \eqn{LL} is the maximum likelihood value, \eqn{K} the number of parameters
#' estimated and \eqn{N} the number of observations.
#'
#' @name ic
#'
#' @param object A stochastic metafrontier model returned
#' by \code{\link{sfametafrontier}}.
#' @param IC Character string. Information criterion measure. Three criteria
#' are available: \itemize{ \item \code{'AIC'} for Akaike information criterion
#' (default) \item \code{'BIC'} for Bayesian information criterion \item
#' \code{'HQIC'} for Hannan-Quinn information criterion }.
#' @param ... Currently ignored.
#'
#' @return \code{\link{ic}} returns the value of the information criterion
#' (AIC, BIC or HQIC) of the maximum likelihood coefficients.
#'
#' @seealso \code{\link{sfametafrontier}}, for the stochastic metafrontier analysis model
#' fitting function using cross-sectional or pooled data.
#'
#' @keywords methods AIC BIC HQIC
#'
# information criteria for sfametafrontier ----------
#' @rdname ic
#' @aliases ic.sfametafrontier
#' @importFrom sfaR ic
#' @export
ic.sfametafrontier <- function(object, IC = "AIC", ...) {
  if (!(IC %in% c("AIC", "BIC", "HQIC"))) {
    stop("Unknown information criteria: ", paste(IC), call. = FALSE)
  }
  if (IC == "AIC") {
    obj <- -2 * object$mlLoglik + 2 * object$nParm
  } else {
    if (IC == "BIC") {
      obj <- -2 * object$mlLoglik + log(object$Nobs) * object$nParm
    } else {
      if (IC == "HQIC") {
        obj <- -2 * object$mlLoglik + 2 * log(log(object$Nobs)) * object$nParm
      }
    }
  }
  message(IC, ": ", prettyNum(obj), sep = "")
}
