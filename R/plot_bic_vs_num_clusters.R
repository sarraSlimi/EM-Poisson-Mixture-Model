#' Plot BIC vs. Number of Clusters
#'
#' This function creates a plot to visualize the Bayesian Information Criterion (BIC) values
#' against the number of clusters.
#'
#' @param num_clusters_range A vector specifying the range of number of clusters.
#' @param bics A vector of BIC values corresponding to each number of clusters.
#' @return NULL
#' @export
plot_bic_vs_num_clusters <- function(num_clusters_range, bics) {
  plot(num_clusters_range, bics, type = "b", xlab = "Number of Clusters", ylab = "BIC", main = "BIC vs. Number of Clusters")
}
