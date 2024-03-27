#' M step of the EM algorithm for Poisson Mixture Model
#'
#' This function performs the maximization (M) step of the EM algorithm for
#' fitting a Poisson mixture model.
#'
#' @param model A list containing the current model parameters.
#' @param X The data matrix.
#' @param responsibilities A matrix of responsibilities.
#' @return Updated model with new parameters after the M step.
#' @export
m_step <- function(model, X, responsibilities) {
  Nk <- colSums(responsibilities)
  model$cluster_probs <- Nk / sum(Nk)
  new_cluster_centers <- matrix(0, nrow = model$num_clusters, ncol = model$num_features)
  for (i in 1:model$num_clusters) {
    new_cluster_centers[i, ] <- colSums(X * responsibilities[, i]) / Nk[i]
  }
  model$cluster_lambdas <- new_cluster_centers
  return(model)
}

