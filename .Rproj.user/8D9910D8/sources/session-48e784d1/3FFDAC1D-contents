#' Fit Poisson Mixture Model
#'
#' This function fits a Poisson mixture model to the given data using the EM algorithm.
#'
#' @param X The data matrix.
#' @param num_clusters_range A vector specifying the range of number of clusters to try.
#' @param max_iter Maximum number of iterations for the EM algorithm. Default is 100.
#' @param tol Tolerance for convergence of the EM algorithm. Default is 1e-6.
#' @return A list containing the best number of clusters, the best model, and BIC values for different number of clusters.
#' @export
fit <- function(X, num_clusters_range, max_iter = 100, tol = 1e-6) {
  bics <- numeric(length(num_clusters_range))
  models <- list()

  for (i in seq_along(num_clusters_range)) {
    # Initialize model for current number of clusters
    model <- initialize_model(num_clusters_range[i], ncol(X))
    model_result <- fit_model(model, X, max_iter, tol)
    models[[i]] <- model_result$model
    bics[i] <- model_result$bic
  }

  best_num_clusters <- which.min(bics)
  best_model <- models[[best_num_clusters]]

  return(list(best_num_clusters = best_num_clusters, best_model = best_model, bics = bics))
}
