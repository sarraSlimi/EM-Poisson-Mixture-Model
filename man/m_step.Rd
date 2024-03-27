\name{m_step}
\alias{m_step}
\title{
  Perform the Maximization (M) Step in Poisson Mixture Model
}
\usage{
  \code{m_step(model, X, responsibilities)}
}
\arguments{
  \item{model}{A list containing the parameters of the Poisson mixture model.}

  \item{X}{Data matrix.}

  \item{responsibilities}{Responsibility matrix obtained from the E step.}
}
\description{
  Performs the Maximization (M) step in the fitting of a Poisson mixture model.
}
