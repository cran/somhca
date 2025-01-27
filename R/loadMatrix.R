## Define the function loadMatrix() to load the data.

#' Load Data and Convert to a Matrix
#'
#' Loads data from a CSV file or an in-memory object (data frame or matrix),
#' optionally removes row headings, and applies specified normalization methods
#' before converting the data to a matrix. In the original dataset, rows represent
#' observations (e.g., samples), columns represent variables (e.g., features),
#' and all cells (except for column headers and, if applicable, row headers)
#' must only contain numeric values.
#'
#' @import RColorBrewer aweSOM dplyr kohonen maptree
#' @importFrom grDevices colorRampPalette
#' @importFrom stats cutree dist hclust na.omit
#' @importFrom utils read.csv setTxtProgressBar txtProgressBar
#' @param input A string specifying the path to the CSV file, or an in-memory
#'   object (data frame or matrix).
#' @param remove_row_headings A logical value. If `TRUE`, removes the first column of the dataset. This is useful when the first column contains non-numeric identifiers (e.g., sample names) that should be excluded from the analysis. Default is `FALSE`.
#' @param scaling A string specifying the scaling method. Options are:
#'   \describe{
#'     \item{"no"}{No scaling is applied (default).}
#'     \item{"simpleFeature"}{Each column is divided by its maximum value.}
#'     \item{"minMax"}{Each column is scaled to range [0, 1].}
#'     \item{"zScore"}{Each column is Z-score standardized.}
#'   }
#' @return A matrix with the processed data.
#' @examples
#' # Example 1: Load toy data from a CSV file
#' file_path <- system.file("extdata", "toy_data.csv", package = "somhca")
#'
#' # Run the loadMatrix function with the mock data
#' myMatrix <- loadMatrix(file_path, TRUE, "minMax")
#'
#' # Example 2: Load from a toy data frame
#' df <- data.frame(
#'   ID = paste0("Sample", 1:100), # Character column for row headings
#'   matrix(rnorm(900), nrow = 100, ncol = 9) # Numeric data
#' )
#'
#' # Run the loadMatrix function with the mock data
#' myMatrix <- loadMatrix(df, TRUE, "zScore")
#'
#' # Example 3: Load from a toy matrix
#' mat <- matrix(rnorm(900), nrow = 100, ncol = 9) # Numeric data
#'
#' # Run the loadMatrix function with the mock data
#' myMatrix <- loadMatrix(mat, FALSE, "simpleFeature")
#' @export

loadMatrix <- function(input, remove_row_headings = FALSE, scaling = "no") {

  # Determine input type and load data accordingly
  if (is.character(input)) {
    # If input is a file path, read the data
    data <- read.csv(input)
  } else if (is.matrix(input) || is.data.frame(input)) {
    # If input is a matrix or data frame, use it directly
    data <- as.data.frame(input)  # Convert matrix to data frame if necessary
  } else {
    stop("Input must be a file path (character), data frame, or matrix.")
  }

  # Remove the first column if specified
  if (remove_row_headings) {
    data <- data[, -1, drop = FALSE]
  }

  # Perform data scaling if specified
  if (scaling == "simpleFeature") {
    data <- apply(data, 2, function(x) x / max(x, na.rm = TRUE))
  } else if (scaling == "minMax") {
    data <- apply(data, 2, function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  } else if (scaling == "zScore") {
    data <- apply(data, 2, scale)
  }

  # Convert the data to a matrix
  data_matrix <- as.matrix(data)

  return(data_matrix)
}
