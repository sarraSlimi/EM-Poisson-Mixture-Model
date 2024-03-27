#' Calculate Bayesian Information Criterion (BIC) for Poisson Mixture Model
#'
#' This function calculates the Bayesian Information Criterion (BIC) for evaluating
#' the fit of a Poisson mixture model.
#'
#' @param model A list containing the current model parameters.
#' @param X The data matrix.
#' @return BIC value.
#' @export
calculate_bic <- function(model, X) {
  n <- nrow(X)
  k <- model$num_clusters * model$num_features
  log_likelihood <- log_likelihood(model, X)
  bic <- -2 * log_likelihood + k * log(n)
  return(bic)
}
