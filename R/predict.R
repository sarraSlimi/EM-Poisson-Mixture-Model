#' Predict Cluster Memberships
#'
#' This function predicts the cluster memberships for the given data using the fitted model.
#'
#' @param model A list containing the fitted model parameters.
#' @param X The data matrix.
#' @return A vector indicating the predicted cluster memberships for each observation in the data.
#' @export
predict <- function(model, X) {
  responsibilities <- e_step(model, X)
  return(apply(responsibilities, 1, which.max))
}
