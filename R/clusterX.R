#' Perform Hierarchical Clustering on Data Objects
#'
#' Groups similar observations using hierarchical clustering
#' This function is designed for clustering data types other than trained SOM models.
#' By default, the optimal number of clusters is determined automatically using the KGS penalty function,
#' but the user can also specify a fixed number of clusters.
#'
#' @importFrom maptree kgs
#' @importFrom stats dist hclust cutree
#' @importFrom stats rect.hclust
#' @importFrom utils read.csv
#' @importFrom fpc cluster.stats
#' @param x Data frame or matrix containing numeric data to be clustered (non-numeric columns are automatically ignored). Argument is required.
#' @param n_clusters Integer.
#'   If provided, specifies the number of clusters to cut the dendrogram into.
#'   If NULL (default), the optimal number of clusters is determined automatically
#'   using the KGS penalty function.
#' @param validity_indices Logical. Indicates whether to compute and print popular clustering validity indices (Silhouette, Dunn, Calinski-Harabasz, Pearson Gamma). Default is `TRUE`.
#' @param plot_result Logical. Indicates whether to plot the clustering result. Default is `TRUE`.
#' @param input Character or in-memory dataset. A string specifying the path to a CSV file, or an in-memory
#'   object (data frame or matrix). Default is `NULL`.
#'   If provided, cluster assignments are appended to the observations in the original dataset,
#'   and the updated data is stored in a package environment as 'DataAndClusters'.
#' @return
#'   Invisibly returns `NULL`. If `plot_result = TRUE`, a plot of the clustering result is produced.
#'   If `input` is provided, the clustered dataset is stored in the package
#'   environment and can be retrieved with `getClusterData()`.
#' @examples
#' # Create a toy matrix with 9 columns and 100 rows
#' data <- matrix(rnorm(900), ncol = 9, nrow = 100)
#'
#' # Example 1: Perform clustering directly on the matrix
#' clusterX(data, plot_result = TRUE)
#'
#' # Example 2: Assign clusters to an in-memory data frame
#' df <- data.frame(
#'   ID = paste0("Sample", 1:100), # Character column for row headings
#'   matrix(rnorm(900), ncol = 9, nrow = 100) # Numeric data
#' )
#' clusterX(data, plot_result = FALSE, input = df)
#' getClusterData()
#' @export

clusterX <- function(x,
                     n_clusters = NULL,
                     validity_indices = TRUE,
                     plot_result = TRUE,
                     input = NULL) {

  # -----------------------------
  # Validate data
  # -----------------------------
  if (is.null(x)) {
    stop("`x` must be provided as a matrix or data frame.")
  }

  if (!(is.matrix(x) || is.data.frame(x))) {
    stop("`x` must be a data frame or matrix.")
  }

  # -----------------------------
  # Keep only numeric columns
  # -----------------------------
  if (is.data.frame(x)) {

    numeric_cols <- vapply(x, is.numeric, logical(1))

    if (!any(numeric_cols)) {
      stop("`x` must contain at least one numeric column.")
    }

    removed <- names(x)[!numeric_cols]

    if (length(removed) > 0) {
      message(
        "Ignoring non-numeric columns: ",
        paste(removed, collapse = ", ")
      )
    }

    x <- x[, numeric_cols, drop = FALSE]
  }

  x <- as.matrix(x)

  # -----------------------------
  # Hierarchical clustering
  # -----------------------------
  distance <- dist(x, method = "euclidean")
  clustering <- hclust(distance, method = "ward.D2")

  # -----------------------------
  # Determine number of clusters
  # -----------------------------
  if (is.null(n_clusters)) {

    optimal_k <- kgs(clustering, distance, maxclust = 20)
    clusters <- as.integer(names(optimal_k[which.min(optimal_k)]))

    message(clusters, " clusters were determined using KGS.\n")

  } else {

    if (!is.numeric(n_clusters) || length(n_clusters) != 1 || n_clusters < 1) {
      stop("`n_clusters` must be a single positive integer.")
    }

    clusters <- as.integer(n_clusters)
    message(clusters, " clusters were specified by the user.\n")
  }

  if (clusters > nrow(x)) {
    stop("`n_clusters` cannot exceed number of observations.")
  }

  som_cluster <- cutree(clustering, clusters)

  # -----------------------------
  # Validity indices
  # -----------------------------
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
      message("Package 'fpc' not installed; skipping validity indices.")
    }
  }

  # -----------------------------
  # Plot
  # -----------------------------
  if (plot_result) {

    max_colors <- max(20, clusters)
    palette <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set1"))(max_colors)

    plot(clustering,
         main = "Hierarchical Clustering Dendrogram",
         xlab = "Observations",
         ylab = "Dissimilarity",
         sub = ""
    )

    rect.hclust(clustering,
                k = clusters,
                border = palette[1:clusters])
  }

  # -----------------------------
  # Attach clusters to external data (optional)
  # -----------------------------
  if (!is.null(input)) {

    if (!is.data.frame(input)) {
      input <- as.data.frame(input)
    }

    if (nrow(input) != nrow(x)) {
      stop("`input` must have same number of rows as `x`.")
    }

    input$Cluster <- factor(som_cluster)
    input <- input[, c("Cluster", setdiff(names(input), "Cluster"))]

    somhca_env$DataAndClusters <- input

    message("The clustered dataset is stored in the package environment as 'DataAndClusters'. Use `getClusterData()` to retrieve it.")
  }

  invisible(som_cluster)
}
