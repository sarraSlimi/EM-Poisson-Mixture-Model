#' Fit Poisson Mixture Model
#'
#' This function fits a Poisson mixture model to the given data using the EM algorithm.
#'
#' @param X The data matrix.
#' @param num_clusters_range A vector specifying the range of number of clusters to try.
#' @param max_iter Maximum number of iterations for the EM algorithm.
#' @param tol Tolerance for convergence of the EM algorithm.
#' @return A list containing the best number of clusters, the best model, and BIC values for different number of clusters.
#' @export
fit_poisson_mixture <- function(X, num_clusters_range, max_iter = 100, tol = 1e-6) {
  return(fit(X, num_clusters_range, max_iter, tol))
}
