#' Expectation step of the EM algorithm for Poisson Mixture Model
#'
#' This function performs the expectation (E) step of the EM algorithm for
#' fitting a Poisson mixture model.
#'
#' @param model A list containing the current model parameters.
#' @param X The data matrix.
#' @return A matrix of responsibilities.
#' @export
e_step <- function(model, X) {
  poissons <- sapply(seq_len(model$num_clusters), function(cluster_idx) {
    apply(X, 1, function(x) prod(dpois(x, model$cluster_lambdas[cluster_idx, ])))
  })
  if (ncol(poissons) != model$num_clusters) {
    stop("The number of columns in the Poisson matrix does not match the number of clusters in the model.")
  }
  weighted_poissons <- model$cluster_probs * poissons
  responsibilities <- weighted_poissons / rowSums(weighted_poissons)
  return(responsibilities)
}
