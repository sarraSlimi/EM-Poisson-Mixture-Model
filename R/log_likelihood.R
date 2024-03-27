#' Calculate Log-Likelihood for Poisson Mixture Model
#'
#' This function calculates the log-likelihood of the data given the parameters
#' of a Poisson mixture model.
#'
#' @param model A list containing the current model parameters.
#' @param X The data matrix.
#' @return Log-likelihood value.
#' @export
log_likelihood <- function(model, X) {
  poissons <- array(dim = c(dim(X), model$num_clusters))
  for (cluster_idx in 1:model$num_clusters) {
    poissons[, , cluster_idx] <- dpois(X, model$cluster_lambdas[cluster_idx])
  }
  weighted_poissons <- poissons * model$cluster_probs
  likelihoods <- rowSums(weighted_poissons)
  log_likelihood <- sum(log(likelihoods))
  return(log_likelihood)
}
