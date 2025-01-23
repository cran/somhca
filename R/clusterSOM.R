#' Perform Clustering on SOM Nodes
#'
#' Groups similar nodes of the SOM using hierarchical clustering and the KGS
#' penalty function to determine the optimal number of clusters.
#'
#' @import RColorBrewer aweSOM dplyr kohonen maptree
#' @importFrom grDevices colorRampPalette
#' @importFrom stats cutree dist hclust na.omit
#' @importFrom utils read.csv setTxtProgressBar txtProgressBar
#' @param model The trained SOM model object.
#' @param plot_result A logical value indicating whether to plot the clustering result. Default is `TRUE`.
#' @param file_path An optional string specifying the path to a CSV file. If provided, clusters are assigned to the observations in the original dataset, and the updated data is stored in a package environment as 'DataAndClusters'.
#' @return A plot of the clusters on the SOM grid (if `plot_result = TRUE`). If `file_path` is specified, the clustered dataset is stored in a package environment for retrieval.
#' @examples
#' # Create a toy matrix with 9 columns and 100 rows
#' data <- matrix(rnorm(900), ncol = 9, nrow = 100)  # 900 random numbers, 100 rows, 9 columns
#'
#' # Run the finalSOM function with the mock data
#' model <- finalSOM(data, dimension = 6, iterations = 700)
#'
#' # Perform clustering using the mock model
#' clusterSOM(model, plot_result = TRUE)
#'
#' # Load the toy data from the package's inst/extdata/ directory, perform
#' # clustering and retrieve the clustered dataset
#' file_path <- system.file("extdata", "toy_data.csv", package = "somhca")
#' clusterSOM(model, plot_result = FALSE, file_path)
#' getClusterData()
#' @export

clusterSOM <- function(model, plot_result = TRUE, file_path = NULL) {
  # Validate inputs
  if (!inherits(model, "kohonen")) {
    stop("The input model must be a trained SOM object (of class 'kohonen').")
  }
  if (!is.null(file_path) && !file.exists(file_path)) {
    stop("The specified file path does not exist or cannot be read.")
  }

  # Perform hierarchical clustering
  distance <- dist(getCodes(model))
  clustering <- hclust(distance)

  # Determine optimal number of clusters using the KGS penalty function
  optimal_k <- kgs(clustering, distance, maxclust = 20)
  clusters <- as.integer(names(optimal_k[which(optimal_k == min(optimal_k))]))
  message(clusters, " clusters were determined.\n")

  # Assign clusters to SOM units
  som_cluster <- cutree(clustering, clusters)

  # Create a color palette
  max_colors <- max(20, clusters)  # Ensure enough colors
  pretty_palette <- colorRampPalette(brewer.pal(8, "Set1"))(max_colors)

  # Plot the result if requested
  if (plot_result) {
    plot(model, type = "mapping", bgcol = pretty_palette[som_cluster], main = "Clusters")
    add.cluster.boundaries(model, som_cluster)
  }

  # If file_path is provided, process and store the data
  if (!is.null(file_path)) {
    # Map clusters to original observations
    cluster_assignment <- som_cluster[model$unit.classif]

    # Read and modify the dataset
    data <- read.csv(file_path)
    data$Cluster <- cluster_assignment
    data <- data[, c("Cluster", setdiff(names(data), "Cluster"))]

    # Store the data in the package environment
    somhca_env$DataAndClusters <- data

    # Notify the user
    message("The clustered dataset is stored in the package environment as 'DataAndClusters'. Use `getClusterData()` to retrieve it.\n")
  }
}

#' Retrieve Clustered Data
#'
#' Access the dataset with cluster assignments stored by `clusterSOM`.
#' @return A data frame with the clustered dataset.
#' @export
getClusterData <- function() {
  if (!exists("DataAndClusters", envir = somhca_env)) {
    stop("No clustered data found. Run `clusterSOM` with a valid `file_path` first.")
  }
  return(somhca_env$DataAndClusters)
}
