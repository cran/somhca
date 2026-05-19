#' Load Data and Convert to a Matrix
#'
#' Loads data from a CSV file or an in-memory object (data frame or matrix),
#' optionally removes specified columns, and applies specified normalization methods
#' before converting the data to a matrix. In the original dataset, rows represent
#' observations (e.g., samples), columns represent variables (e.g., features),
#' and all cells (except for column headers and, if applicable, row headers)
#' must only contain numeric values.
#'
#' @importFrom utils read.csv
#' @param input Character or in-memory dataset. A string specifying the path to a CSV file, or an in-memory
#'   object (data frame or matrix). Argument is required.
#' @param remove_columns Integer, character, or vector of either.
#'   If specified, removes the columns of the dataset indicated by positions or names.
#'   This is useful, for example, when the first column contains
#'   non-numeric identifiers (e.g., sample names) that should be
#'   excluded from the analysis. Default is `NULL`.
#' @param remove_row_headings Deprecated, use `remove_columns = 1` instead. Logical. If `TRUE`, removes the first column of the dataset. Default is `FALSE`.
#' @param scaling Character. Normalization method to apply to the values in each column of the dataset. One of:
#'   \describe{
#'     \item{"no"}{No scaling is applied (default).}
#'     \item{"simpleFeature"}{Divided by the maximum value.}
#'     \item{"minMax"}{Scale values to range [0, 1].}
#'     \item{"zScore"}{standardize values by subtracting the mean and dividing by the standard deviation.}
#'   }
#' @return A matrix with the processed data.
#' @examples
#' # Example 1: Load toy data from a CSV file
#' file_path <- system.file("extdata", "toy_data.csv", package = "somhca")
#'
#' # Run the loadMatrix function with the mock data
#' myMatrix <- loadMatrix(file_path, remove_columns = 1, scaling = "minMax")
#'
#' # Example 2: Load from a toy data frame
#' df <- data.frame(
#'   ID = paste0("Sample", 1:100), # Character column for row headings
#'   matrix(rnorm(900), nrow = 100, ncol = 9) # Numeric data
#' )
#'
#' # Run the loadMatrix function with the mock data
#' myMatrix <- loadMatrix(df, remove_columns = 1, scaling = "zScore")
#'
#' # Example 3: Load from a toy matrix
#' mat <- matrix(rnorm(900), nrow = 100, ncol = 9) # Numeric data
#'
#' # Run the loadMatrix function with the mock data
#' myMatrix <- loadMatrix(mat, scaling = "simpleFeature")
#' @export

loadMatrix <- function(input, remove_columns = NULL, remove_row_headings = FALSE, scaling = "no") {

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

  # Validate scaling argument
  valid_scaling <- c("no", "simpleFeature", "minMax", "zScore")
  if (!scaling %in% valid_scaling) {
    stop(
      "`scaling` must be one of: ",
      paste(valid_scaling, collapse = ", ")
    )
  }

  # Handle deprecated argument
  if (!missing(remove_row_headings)) {
    .Deprecated(
      new = "remove_columns",
      msg = "`remove_row_headings` is deprecated. Use `remove_columns = 1` instead."
    )

    if (isTRUE(remove_row_headings)) {
      remove_columns <- unique(c(remove_columns, 1))
    }
  }

  # Remove specified columns
  if (!is.null(remove_columns)) {

    # If character, convert column names to indices
    if (is.character(remove_columns)) {
      missing_cols <- setdiff(remove_columns, colnames(data))
      if (length(missing_cols) > 0) {
        stop(
          "The following columns were not found in `data`: ",
          paste(missing_cols, collapse = ", ")
        )
      }
      remove_columns <- match(remove_columns, colnames(data))
    }

    # Validate numeric indices
    if (!is.numeric(remove_columns)) {
      stop("`remove_columns` must be numeric or character.")
    }

    if (any(remove_columns < 1 | remove_columns > ncol(data))) {
      stop("`remove_columns` contains invalid column indices.")
    }

    data <- data[, -remove_columns, drop = FALSE]
  }

  if (!all(vapply(data, is.numeric, logical(1)))) {
    stop("All columns must be numeric after column removal.")
  }

  # Perform data scaling if specified
  if (scaling == "simpleFeature") {
    data[] <- lapply(data, function(x) x / max(x, na.rm = TRUE))
  } else if (scaling == "minMax") {
    data[] <- lapply(data, function(x)
      (x - min(x, na.rm = TRUE)) /
        (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    )
  } else if (scaling == "zScore") {
    data[] <- lapply(data, scale)
  }

  # Convert the data to a matrix
  data_matrix <- as.matrix(data)

  return(data_matrix)
}
