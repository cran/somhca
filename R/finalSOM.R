## Define the function finalSOM() to re-train the SOM model using the selected optimal grid size.

#' Train Final SOM Model
#'
#' Re-trains the SOM using a specified optimal grid size and number of iterations.
#'
#' @import RColorBrewer aweSOM dplyr kohonen maptree
#' @importFrom grDevices colorRampPalette
#' @importFrom stats cutree dist hclust na.omit
#' @importFrom utils read.csv setTxtProgressBar txtProgressBar
#' @param data The preprocessed data matrix containing the input data for SOM training.
#' @param dimension An integer specifying the dimension of the square SOM grid (e.g., 5 results in a 5x5 grid).
#' @param iterations An integer defining the number of iterations for training the SOM model. Use a large value, e.g., 500 or higher, for improved training (an error message could suggest that reducing the number of iterations might be necessary).
#' @return A trained SOM model object.
#' @examples
#' # Create a toy matrix with 9 columns and 100 rows
#' data <- matrix(rnorm(900), ncol = 9, nrow = 100)  # 900 random numbers, 100 rows, 9 columns
#'
#' # Run the finalSOM function with the mock data
#' myFinalSOM <- finalSOM(data, dimension = 6, iterations = 700)
#' @export

finalSOM <- function(data, dimension, iterations) {
  My_dim <- dimension
  My_Grid <- somgrid(xdim = My_dim, ydim = My_dim, topo = "hexagonal", toroidal = T)
  # Create the model
  My_Model <- som(X = data,
                  grid = My_Grid,
                  rlen=iterations,
                  alpha=c(0.05,0.01),
                  keep.data = TRUE)
  return(My_Model)
}
