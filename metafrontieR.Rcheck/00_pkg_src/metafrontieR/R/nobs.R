################################################################################
#                                                                              #
# R functions for the metafrontieR package                                     #
#                                                                              #
################################################################################

#------------------------------------------------------------------------------#
# Variance - Covariance Matrix of estimates                                    #
# Models:                                                                      #
#           -Stochastic Frontier Analysis                                      #
#           -Latent Class Stochastic Frontier Analysis                         #
#           -Sample selection correction for Stochastic Frontier Model         #
# Data: Cross sectional data & Pooled data                                     #
#------------------------------------------------------------------------------#

#' Extract total number of observations used in frontier models
#'
#' This function extracts the total number of 'observations' from a
#' fitted point frontier model.
#'
#' `nobs` gives the number of observations actually
#' used by the estimation procedure.
#'
#' @name nobs
#'
#' @param object a `sfametafrontier`
#' object for which the number of total observations is to be extracted.
#' @param \dots Currently ignored.
#'
#' @return A single number, normally an integer.
#'
#' @seealso \code{\link{sfametafrontier}}, for the stochastic metafrontier analysis model
#' fitting function using cross-sectional or pooled data
#' @keywords attribute
#'
# Extract number of observations for sfametafrontier ----------
#' @rdname nobs
#' @aliases nobs.sfametafrontier
#' @importFrom stats nobs
#' @export
nobs.sfametafrontier <- function(object, ...) {
  return(object$Nobs)
}
