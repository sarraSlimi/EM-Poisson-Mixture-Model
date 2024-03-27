\name{fit_poisson_mixture}
\alias{fit_poisson_mixture}
\title{
  Fit Poisson Mixture Model to Data
}
\usage{
  \code{fit_poisson_mixture(X, num_clusters_range, max_iter = 100, tol = 1e-6)}
}
\arguments{
  \item{X}{Data matrix.}

  \item{num_clusters_range}{Range of the number of clusters to consider.}

  \item{max_iter}{Maximum number of iterations for fitting the model. Default is 100.}

  \item{tol}{Tolerance value for convergence criterion. Default is 1e-6.}
}
\description{
  Fits a Poisson mixture model to clustered data.
}
