\name{calculate_bic}
\alias{calculate_bic}
\title{
  Calculate Bayesian Information Criterion (BIC) for Poisson Mixture Model
}
\usage{
  \code{calculate_bic(model, X)}
}
\arguments{
  \item{model}{A list containing the parameters of the Poisson mixture model.}

  \item{X}{Data matrix.}
}
\description{
  Calculates the Bayesian Information Criterion (BIC) for a fitted Poisson mixture model.
}
\examples{
  set.seed(500)
  n_samples <- 1000
  n_clusters <- 4
  cluster_centers <- matrix(c(1, 1, 50, 50, 100, 100, 200, 200), ncol = 2, byrow = TRUE)
  X <- rbind(mapply(rpois, n_samples/n_clusters, as.data.frame(cluster_centers)))

  num_clusters_range <- 1:10
  fit_result <- fit_poisson_mixture(X, num_clusters_range)
  best_model <- fit_result$best_model

  calculate_bic(best_model, X)
}
