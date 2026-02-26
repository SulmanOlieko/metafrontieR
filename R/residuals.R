################################################################################
#                                                                              #
# R functions for the metafrontieR package                                     #
#                                                                              #
################################################################################

#------------------------------------------------------------------------------#
# Residuals of model (v - S * u)                                               #
# Models:                                                                      #
#           -Stochastic metafrontier analysis                                  #
#           -Latent class stochastic metafrontier analysis                     #
#           -Sample selection correction for stochastic metafrontier model     #
# Data: Cross sectional data & Pooled data                                     #
#------------------------------------------------------------------------------#

#' Extract residuals of stochastic metafrontier models
#'
#' This function returns the residuals' values from stochastic metafrontier models
#' estimated with \code{\link{sfametafrontier}}.
#'
#' @param object A stochastic metafrontier model returned
#' by \code{\link{sfametafrontier}}.
#' @param \dots Currently ignored.
#'
#' @name residuals
#'
#' @return \code{\link{residuals}} returns a vector of residuals values.
#'
#' @note The residuals values are ordered in the same way as the corresponding
#' observations in the dataset used for the estimation.
#'
#' @seealso \code{\link{sfametafrontier}}, for the stochastic metafrontier analysis model
#' fitting function using cross-sectional or pooled data.
#'
#' @keywords methods residuals
#'
# residuals for sfametafrontier (residuals vs. metafrontier) ----------
#' @rdname residuals
#' @aliases residuals.sfametafrontier
#' @importFrom stats residuals
#' @export
residuals.sfametafrontier <- function(object, ...) {
  # Residual = observed Y - metafrontier fitted value
  Yvar <- object$dataTable[[all.vars(object$formula)[1]]]
  if (is.null(Yvar)) {
    # Reconstruct Y from formula and data
    Yvar <- model.response(model.frame(
      object$formula,
      data = object$dataTable,
      na.action = na.pass
    ))
  }
  as.numeric(Yvar) - object$dataTable$.mf_yhat_meta
}
