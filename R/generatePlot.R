#' Generate SOM Visualization Plots
#'
#' Creates various types of plots to visualize and evaluate the trained SOM model.
#'
#' @import kohonen
#' @param model In-memory SOM model object. A trained SOM model. Argument is required.
#' @param plot_type Integer. Specifies the type of plot to generate. One of:
#'   \describe{
#'     \item{1}{Training progress plot (changes during training).}
#'     \item{2}{Node count plot (number of samples mapped to each node) for assessing map quality.}
#'     \item{3}{U-matrix plot (similarities between neighboring nodes).}
#'     \item{4}{Weight vector plot (patterns in the distributions of variables).}
#'     \item{5}{Kohonen heatmaps for all variables in the dataset (distribution of single variables across the map).}
#'   }
#'   Argument is required.
#' @param data Matrix containing numeric data. A preprocessed data matrix containing the input data for SOM training. Default is `NULL` (required only for `plot_type = 5`).
#' @return A plot or a series of plots is generated and displayed based on the specified type.
#' @examples
#' # Create a toy matrix with 9 columns and 100 rows
#' data <- matrix(rnorm(900), ncol = 9, nrow = 100)  # 900 random numbers, 100 rows, 9 columns
#'
#' # Assign column names to the data matrix
#' colnames(data) <- paste("Var", 1:ncol(data), sep = "_")
#'
#' # Run the finalSOM function with the mock data
#' model <- finalSOM(data, dimension = 6, iterations = 700)
#'
#' # Generate plots using the mock model
#' generatePlot(model, plot_type = 2)
#' generatePlot(model, plot_type = 5, data)
#' @export

generatePlot <- function(model, plot_type, data = NULL) {

  if (!inherits(model, "kohonen")) {
    stop("`model` must be a trained SOM object from the kohonen package.")
  }

  # Select plot based on plot_type
  if (plot_type == 1) {
    plot(model, type = "changes")

  } else if (plot_type == 2) {
    plot(model, type = "count", main = "Node Counts")

  } else if (plot_type == 3) {
    plot(model, type = "dist.neighbours", main = "SOM Neighbour Distances")

  } else if (plot_type == 4) {
    plot(model, type = "codes", main = "Weight Vectors")

  } else if (plot_type == 5) {

    if (is.null(data)) {
      stop("For plot_type = 5, `data` must be provided.")
    }

    codes <- getCodes(model)

    if (ncol(data) != ncol(codes)) {
      stop("Number of columns in `data` must match the SOM model variables.")
    }

    var_names <- colnames(codes)
    if (is.null(var_names)) {
      var_names <- paste0("Var_", seq_len(ncol(codes)))
    }

    for (i in seq_len(ncol(codes))) {
      plot(model, type = "property", property = codes[, i], main = var_names[i])
    }

  } else {
    stop("Invalid `plot_type`. Must be an integer between 1 and 5.")
  }
}
