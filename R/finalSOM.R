## Define the function finalSOM() to re-train the SOM model using the selected optimal grid size.

#' Train Final SOM Model
#'
#' Re-trains the SOM using a specified optimal grid size and number of iterations.
#'
#' @importFrom kohonen som somgrid
#' @param data A preprocessed data matrix containing the input data for SOM training.
#' @param dimension An integer specifying the dimension of the square SOM grid (e.g., 5 results in a 5x5 grid).
#' @param iterations An integer defining the number of iterations for training the SOM model. Use a large value, e.g., 500 or higher, for improved training (an error message could suggest that reducing the number of iterations might be necessary).
#'   For larger grids, more iterations may be required to ensure convergence. Reducing iterations may speed training but risk under-trained neurons.
#' @param chunk An integer specifying the number of iterations per training block.
#'   The SOM will be trained in chunks of this many iterations, with a progress
#'   message printed after each block. This helps notify the user that the function
#'   is running and not frozen. Default is 100. Larger values reduce the frequency
#'   of messages; smaller values provide more frequent updates but may slightly
#'   slow execution.
#' @return A trained SOM model object.
#' @examples
#' # Create a toy matrix with 9 columns and 100 rows
#' data <- matrix(rnorm(900), ncol = 9, nrow = 100)  # 900 random numbers, 100 rows, 9 columns
#'
#' # Run the finalSOM function with the mock data
#' myFinalSOM <- finalSOM(data, dimension = 6, iterations = 700)
#' @export

finalSOM <- function(data, dimension, iterations, chunk = 100) {

  # Basic safety checks
  stopifnot(is.numeric(dimension), dimension >= 2)
  stopifnot(is.numeric(iterations), iterations >= 1)
  stopifnot(is.numeric(chunk), chunk >= 1)

  message(sprintf("Training SOM: %dx%d grid, %d iterations", dimension, dimension, iterations))

  My_Grid <- somgrid(xdim = dimension, ydim = dimension, topo = "hexagonal", toroidal = TRUE)

  # Initialize SOM with 0 iterations
  My_Model <- som(X = data, grid = My_Grid, rlen = 0, alpha = c(0.05, 0.01), keep.data = TRUE)

  for (start in seq(1, iterations, by = chunk)) {
    end <- min(start + chunk - 1, iterations)

    # Train SOM for the next chunk
    My_Model <- som(X = data, grid = My_Grid, rlen = end, alpha = c(0.05, 0.01), keep.data = TRUE,
                    init = My_Model$codes)  # Continue from previous state

    message(sprintf("Completed %d / %d iterations...", end, iterations))
  }

  message("SOM training complete.")
  return(My_Model)
}
