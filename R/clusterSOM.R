#' Perform Clustering on SOM Nodes
#'
#' Groups similar nodes of a SOM using hierarchical clustering.
#' By default, the optimal number of clusters is determined automatically using the KGS penalty function,
#' but the user can also specify a fixed number of clusters.
#'
#' @import kohonen
#' @importFrom maptree kgs
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats dist hclust cutree
#' @importFrom utils read.csv
#' @importFrom fpc cluster.stats
#' @param model In-memory SOM model object. A trained SOM model. Argument is required.
#' @param n_clusters Integer.
#'   If provided, specifies the number of clusters to cut the SOM dendrogram into.
#'   If NULL (default), the optimal number of clusters is determined automatically
#'   using the KGS penalty function.
#' @param validity_indices Logical. Indicates whether to compute and print popular clustering validity indices (Silhouette, Dunn, Calinski-Harabasz, Pearson Gamma). Default is `TRUE`.
#' @param plot_result Logical. Indicates whether to plot the clustering result. Default is `TRUE`.
#' @param input Character or in-memory dataset. A string specifying the path to a CSV file, or an in-memory
#'   object (data frame or matrix). Default is `NULL`.
#'   If provided, cluster assignments are appended to the observations in the original dataset, and the updated data is stored in a package environment as 'DataAndClusters'.
#' @return
#'   Invisibly returns `NULL`. If `plot_result = TRUE`, a plot of the clusters on the SOM grid is produced.
#'   If `input` is provided, the clustered dataset is stored in the package
#'   environment and can be retrieved with `getClusterData()`.
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
#' # Example 2: Assign SOM-based clusters to an in-memory data frame
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

clusterSOM <- function(model, n_clusters = NULL, validity_indices = TRUE, plot_result = TRUE, input = NULL) {
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
  codes <- getCodes(model)
  distance <- dist(codes, method = "euclidean")
  clustering <- hclust(distance, method = "ward.D2")

  # Determine optimal number of clusters using the KGS penalty function
  if (is.null(n_clusters)) {
    optimal_k <- kgs(clustering, distance, maxclust = 20)
    clusters <- as.integer(names(optimal_k[which.min(optimal_k)]))
    message(clusters, " clusters were determined using KGS.\n")
  } else {
    if (!is.numeric(n_clusters) ||
        length(n_clusters) != 1 ||
        n_clusters < 1) {
      stop("`n_clusters` must be a single positive integer.")
    }
    clusters <- as.integer(n_clusters)
    message(clusters, " clusters were specified by the user.\n")
  }

  n_units <- nrow(codes)

  if (clusters > n_units) {
    stop(
      "`n_clusters` (", clusters,
      ") cannot exceed the number of SOM units (", n_units, ")."
    )
  }

  # Assign clusters to SOM units
  som_cluster <- cutree(clustering, clusters)

  # Cluster validity indices (informative message)
  if (isTRUE(validity_indices)) {
    if (requireNamespace("fpc", quietly = TRUE)) {
      stats <- fpc::cluster.stats(distance, som_cluster)

      message(
        "Cluster validity indices:\n",
        sprintf("  %-25s %6.3f (higher is better; >0.25 reasonable, >0.5 strong)\n",
                "Average Silhouette Width:", stats$avg.silwidth),
        sprintf("  %-25s %6.3f (higher is better)\n",
                "Dunn Index:", stats$dunn),
        sprintf("  %-25s %6.1f (higher is better)\n",
                "Calinski-Harabasz Index:", stats$ch),
        sprintf("  %-25s %6.3f (closer to 1 is better)",
                "Pearson Gamma Coefficient:", stats$pearsongamma)
      )
    } else {
      message("Package 'fpc' not installed; cluster validity indices skipped.")
    }
  }

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

    # Validate that input matches SOM training data
    if (nrow(data) != length(model$unit.classif)) {
      stop(
        "The number of rows in `input` (", nrow(data),
        ") does not match the number of observations used to train the SOM (",
        length(model$unit.classif), ")."
      )
    }

    # Map clusters to original observations
    cluster_assignment <- som_cluster[model$unit.classif]

    # Add cluster assignments to the dataset
    data$Cluster <- factor(cluster_assignment)
    data <- data[, c("Cluster", setdiff(names(data), "Cluster"))]

    # Store the data in the package environment
    somhca_env$DataAndClusters <- data

    # Notify the user
    message("The clustered dataset is stored in the package environment as 'DataAndClusters'. Use `getClusterData()` to retrieve it.\n")
  }

  # Return invisibly
  invisible(NULL)
}

#' Retrieve Clustered Data
#'
#' Access the dataset with cluster assignments stored by `clusterSOM` or `ClusterX`.
#' @return A data frame with the clustered dataset.
#' @export
getClusterData <- function() {
  if (!exists("DataAndClusters", envir = somhca_env)) {
    stop("No clustered data found. Run `clusterSOM` with a valid `input` first.")
  }
  return(somhca_env$DataAndClusters)
}
