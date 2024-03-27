#' Plot Clustered Data
#'
#' This function creates a scatter plot to visualize the clustered data.
#'
#' @param X The data matrix.
#' @param model A list containing the fitted model parameters.
#' @return NULL
#' @export
plot_clustered_data <- function(X, model) {
  predicted_clusters <- predict(model, X)
  if (ncol(X) != 2) {
    stop("plot_clustered_data only supports 2-dimensional data.")
  }
  num_clusters <- length(unique(predicted_clusters))
  colors <- rainbow(num_clusters)
  plot(X, col = colors[predicted_clusters], pch = 16, main = "Clustered Data", xlab = "Feature 1", ylab = "Feature 2")
  legend("topright", legend = 1:num_clusters, col = colors, pch = 16, title = "Cluster")
}
