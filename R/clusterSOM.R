#' Perform Clustering on SOM Nodes
#'
#' Groups similar nodes of the SOM using hierarchical clustering and the KGS
#' penalty function to determine the optimal number of clusters.
#'
#' @import RColorBrewer aweSOM dplyr kohonen maptree
#' @importFrom grDevices colorRampPalette
#' @importFrom stats cutree dist hclust na.omit
#' @importFrom utils read.csv setTxtProgressBar txtProgressBar
#' @param model A trained SOM model object.
#' @param plot_result A logical value indicating whether to plot the clustering result. Default is `TRUE`.
#' @param input An optional input specifying either:
#'   \describe{
#'     \item{File Path}{A string specifying the path to a CSV file.}
#'     \item{In-Memory Data}{A data frame or matrix containing numeric data.}
#'   }
#'   If provided, clusters are assigned to the observations in the original dataset, and the updated data is stored in a package environment as 'DataAndClusters'.
#' @return A plot of the clusters on the SOM grid (if `plot_result = TRUE`). If `input` is provided, the clustered dataset is stored in a package environment for retrieval.
#' @examples
#' # Create a toy matrix with 9 columns and 100 rows
#' data <- matrix(rnorm(900), ncol = 9, nrow = 100)  # 900 random numbers, 100 rows, 9 columns
#'
#' # Run the finalSOM function with the mock data
#' model <- finalSOM(data, dimension = 6, iterations = 700)
#'
#' # Example 1: Perform clustering using the mock model
#' clusterSOM(model, plot_result = TRUE)
#'
#' # Example 2: Cluster with an in-memory toy data frame
#' df <- data.frame(
#'   ID = paste0("Sample", 1:100), # Character column for row headings
#'   matrix(rnorm(900), ncol = 9, nrow = 100) # Numeric data
#' )
#' clusterSOM(model, plot_result = FALSE, input = df)
#' getClusterData()
#'
#' # Example 3: Load toy data from a CSV file, perform clustering, and retrieve the clustered dataset
#' file_path <- system.file("extdata", "toy_data.csv", package = "somhca")
#' clusterSOM(model, plot_result = FALSE, input = file_path)
#' getClusterData()
#' @export

clusterSOM <- function(model, plot_result = TRUE, input = NULL) {
  # Validate model input
  if (!inherits(model, "kohonen")) {
    stop("The input model must be a trained SOM object (of class 'kohonen').")
  }

  # Validate and load the input data
  if (is.null(input)) {
    data <- NULL
  } else if (is.character(input)) {
    if (!file.exists(input)) {
      stop("The specified file path does not exist or cannot be read.")
    }
    data <- read.csv(input)
  } else if (is.matrix(input) || is.data.frame(input)) {
    data <- as.data.frame(input)  # Convert matrix to data frame if necessary
  } else {
    stop("Input must be a file path (character), data frame, or matrix.")
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

  # Process and store the data if input is provided
  if (!is.null(data)) {
    # Map clusters to original observations
    cluster_assignment <- som_cluster[model$unit.classif]

    # Add cluster assignments to the dataset
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
