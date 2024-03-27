\name{fit_model}
\alias{fit_model}
\title{
  Fit Poisson Mixture Model to Data (Internal Function)
}
\usage{
  \code{fit_model(model, X, max_iter, tol)}
}
\arguments{
  \item{model}{A list containing the parameters of the Poisson mixture model.}

  \item{X}{Data matrix.}

  \item{max_iter}{Maximum number of iterations for fitting the model.}

  \item{tol}{Tolerance value for convergence criterion.}
}
\description{
  Fits a Poisson mixture model to clustered data (internal function used by \code{fit}).
}
