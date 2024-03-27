#' Fit Poisson Mixture Model with EM Algorithm
#'
#' This function fits a Poisson mixture model to the given data using the EM algorithm.
#'
#' @param model A list containing the current model parameters.
#' @param X The data matrix.
#' @param max_iter Maximum number of iterations for the EM algorithm.
#' @param tol Tolerance for convergence of the EM algorithm.
#' @return A list containing the fitted model and the Bayesian Information Criterion (BIC) value.
#' @export
fit_model <- function(model, X, max_iter, tol) {
  # Initialization of parameters
  model$cluster_probs <- rep(1 / model$num_clusters, model$num_clusters)
  model$cluster_lambdas <- matrix(
    runif(model$num_clusters * model$num_features,
          min = apply(X, 2, min),
          max = apply(X, 2, max)),
    nrow = model$num_clusters,
    ncol = model$num_features
  )

  # Vector to store log-likelihoods
  log_likelihoods <- numeric(max_iter)

  prev_log_likelihood <- -Inf
  for (iter in 1:max_iter) {
    responsibilities <- e_step(model, X)
    model <- m_step(model, X, responsibilities)
    log_likelihood <- log_likelihood(model, X)
    log_likelihoods[iter] <- log_likelihood
    if (abs(log_likelihood - prev_log_likelihood) < tol) {
      break
    }
    prev_log_likelihood <- log_likelihood
  }

  # Calculate BIC after fitting
  bic <- calculate_bic(model, X)

  # Attach log-likelihoods to the model
  model$log_likelihoods <- log_likelihoods[1:iter]

  # Return the model along with the BIC value
  return(list(model = model, bic = bic))
}
