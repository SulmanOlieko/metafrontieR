################################################################################
#                                                                              #
# R functions for the metafrontieR package                                     #
#                                                                              #
################################################################################

#------------------------------------------------------------------------------#
# Log-likelihood extraction                                                    #
# Models:                                                                      #
#           -Stochastic Metafrontier Analysis                                  #
# Data:     Cross sectional & Pooled data                                      #
#------------------------------------------------------------------------------#

#' Extract log-likelihood value of stochastic metafrontier models
#'
#' \code{\link{logLik}} extracts the log-likelihood value(s) from stochastic
#' metafrontier models estimated with \code{\link{sfametafrontier}}.
#'
#' @param object A stochastic metafrontier model returned
#' by \code{\link{sfametafrontier}}.
#' @param individual Logical. If \code{FALSE} (default), the sum of all
#' observations' log-likelihood values is returned. If \code{TRUE}, a vector of
#' each observation's log-likelihood value is returned.
#' @param ... Currently ignored.
#'
#' @name logLik
#'
#' @return \code{\link{logLik}} returns either an object of class
#' \code{'logLik'}, which is the log-likelihood value with the total number of
#' observations (\code{nobs}) and the number of free parameters (\code{df}) as
#' attributes, when \code{individual = FALSE}, or a list of elements, containing
#' the log-likelihood of each observation (\code{logLik}), the total number of
#' observations (\code{Nobs}) and the number of free parameters (\code{df}),
#' when \code{individual = TRUE}.
#'
#' @seealso \code{\link{sfametafrontier}}, for the stochastic metafrontier analysis model
#' fitting function using cross-sectional or pooled data.
#'
#' @keywords methods likelihood
#'
#' @aliases logLik.sfametafrontier
#' @importFrom stats logLik
#' @export
logLik.sfametafrontier <- function(object, individual = FALSE, ...) {
  if (length(individual) != 1 || !is.logical(individual[1])) {
    stop("argument 'individual' must be a single logical value", call. = FALSE)
  }
  if (individual) {
    LL <- list()
    LL[["logLik"]] <- object$dataTable$.mf_logL_OBS
    LL[["Nobs"]] <- object$Nobs
    LL[["df"]] <- object$nParm
    return(LL)
  } else {
    LL <- object$mlLoglik
    attributes(LL)$nobs <- object$Nobs
    attributes(LL)$df <- object$nParm
    class(LL) <- "logLik"
    return(LL)
  }
}
