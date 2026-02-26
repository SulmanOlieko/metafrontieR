################################################################################
#                                                                              #
# R functions for the metafrontieR package                                     #
#                                                                              #
################################################################################

#------------------------------------------------------------------------------#
# Fitted values of models                                                      #
# Models:                                                                      #
#           -Stochastic metafrontier analysis                                  #
#           -Latent class stochastic metafrontier analysis                     #
#           -Sample selection correction for stochastic metafrontier model     #
# Data: Cross sectional data & Pooled data                                     #
#------------------------------------------------------------------------------#

#' Extract fitted values of stochastic metafrontier models
#'
#' \code{\link{fitted}} returns the fitted frontier values from stochastic
#' metafrontier models estimated with \code{\link{sfametafrontier}}.
#'
#' @param object A stochastic metafrontier model returned
#' by \code{\link{sfametafrontier}}.
#' @param ... Currently ignored.
#'
#' @name fitted
#'
#' @return A vector of fitted values is returned.
#'
#' @note The fitted values are ordered in the same way as the corresponding
#' observations in the dataset used for the estimation.
#'
#' @seealso \code{\link{sfametafrontier}}, for the stochastic metafrontier analysis model
#' fitting function using cross-sectional or pooled data.
#'
#' @keywords methods fitted
#'
# fitted values for sfametafrontier (returns metafrontier fitted values) ----------
#' @rdname fitted
#' @aliases fitted.sfametafrontier
#' @importFrom stats fitted
#' @export
fitted.sfametafrontier <- function(object, ...) {
  object$dataTable$.mf_yhat_meta
}
