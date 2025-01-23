## Define the function generatePlot() to produce different types of plots.

#' Generate SOM Visualization Plots
#'
#' Creates various types of plots to visualize and evaluate the trained SOM model.
#'
#' @import RColorBrewer aweSOM dplyr kohonen maptree
#' @importFrom grDevices colorRampPalette
#' @importFrom stats cutree dist hclust na.omit
#' @importFrom utils read.csv setTxtProgressBar txtProgressBar
#' @param model The trained SOM model object.
#' @param plot_type An integer specifying the type of plot to generate. Options are:
#'   \describe{
#'     \item{1}{Training progress plot (changes during training).}
#'     \item{2}{Node count plot (number of samples mapped to each node) for assessing map quality.}
#'     \item{3}{U-matrix plot (visualizing similarities between neighboring nodes).}
#'     \item{4}{Weight vector plot (patterns in the distributions of variables).}
#'     \item{5}{Kohonen heatmaps for all variables in the dataset (distribution of single variables across the map).}
#'   }
#' @param data The preprocessed data matrix containing the input data. Required only for `plot_type = 5`.
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

  # Select plot based on plot_type
  if (plot_type == 1) {
    # Training progress for SOM.
    plot(model, type="changes")

  } else if (plot_type == 2) {
    # Node count plot (for map quality).
    plot(model, type="count", main="Node Counts")

  } else if (plot_type == 3) {
    # U-matrix visualization (similarity between each node and its neighbors).
    plot(model, type="dist.neighbours", main = "SOM neighbour distances")

  } else if (plot_type == 4) {
    # Weight vector view (patterns in the distribution of samples and variables).
    plot(model, type="codes")

  } else if (plot_type == 5) {
    # Kohonen heatmap creation (distribution of single variables across the map);
    # if there are improper column names, it could potentially result in an error.
    for (i in 1:ncol(data)){
      plot(model, type = "property", property = getCodes(model)[,i], main=colnames(getCodes(model))[i])
    }
  } else {
    stop("Invalid plot_type.")
  }

  # Print the plot
  print(plot)
}
