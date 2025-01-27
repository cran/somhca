## Define the function optimalSOM() to estimate the SOM training grid size based on the data.

#' Estimate Optimal SOM Grid Size
#'
#' Computes the optimal grid size for training a SOM using various quality
#' measures and heuristic approaches.
#'
#' @import RColorBrewer aweSOM dplyr kohonen maptree
#' @importFrom grDevices colorRampPalette
#' @importFrom stats cutree dist hclust na.omit
#' @importFrom utils read.csv setTxtProgressBar txtProgressBar
#' @param data A preprocessed data matrix containing the input data for SOM training.
#' @param method A character string indicating the method for estimating the maximum grid dimension. Options are:
#'   \describe{
#'     \item{"A"}{Uses the heuristic formula by Vesanto et al. (default).}
#'     \item{"B"}{Applies an alternative heuristic approach.}
#'     \item{numeric}{Manually specified maximum dimension.}
#'   }
#' @param increments An integer specifying the step size for increasing grid dimensions. For example, set increments to 2 or 5 to increment the grid size by 2 or 5 rows/columns at each step. Smaller increments lead to more granular searches but may increase computation time; larger increments risk errors if they exceed the estimated maximum SOM grid dimensions.
#' @param iterations An integer defining the number of iterations for SOM training. A lower value, such as less than 500, helps reduce computation time. If the process takes too long or an error occurs, try reducing the number of iterations for quicker results.
#' @return A data frame summarizing quality measures and their associated optimal grid dimensions. Use these results to select the most suitable grid size for your SOM.
#' @examples
#' # Create a toy matrix with 9 columns and 100 rows
#' data <- matrix(rnorm(900), ncol = 9, nrow = 100)  # 900 random numbers, 100 rows, 9 columns
#'
#' # Run the optimalSOM function with the mock data
#' myOptimalSOM <- optimalSOM(data, method = "A", increments = 2, iterations = 300)
#' @export

optimalSOM <- function(data, method = "A", increments, iterations) {
  # Calculate the maximum dimension of a square grid size using one of the approaches indicated by the argument "method".
  if(method=="A") {
    max_dim <- round(sqrt(5*sqrt(nrow(data)))) # Max dimension is determined with the heuristic formula by Vesanto et al.
  }
  else if(method=="B") {
    max_dim <- round((nrow(data))^(1/2.5)) # Max dimension is determined using a slightly different approach.
  }
  else {max_dim <- method # Max dimension is a value manually selected.
  }
  # Execute a growing SOM iterative process to calculate map quality and determine the optimal grid size.
  error_df <-data.frame(NA, NA, NA, NA, NA)
  names(error_df) <-c("Dimension","Quantisation_error","Topographic_error","Kaski-Lagus_error","Explained_variance")
  seq <- seq(2,max_dim,increments)
  progbar <- txtProgressBar(min=0,max=max_dim,style=3)
  for (i in seq) {
    My_Grid <- somgrid(xdim = i, ydim = i, topo = "hexagonal", toroidal = T)
    My_Model <- som(X = data,
                    grid = My_Grid,
                    rlen=iterations,
                    alpha=c(0.05,0.01),
                    keep.data = TRUE)
    quant.error <- as.numeric((aweSOM::somQuality(My_Model, data))$err.quant)
    topo.error <- as.numeric((aweSOM::somQuality(My_Model, data))$err.topo)
    kaski.error <- as.numeric((aweSOM::somQuality(My_Model, data))$err.kaski)
    varratio.error <- as.numeric((aweSOM::somQuality(My_Model, data))$err.varratio)
    error_df <- error_df %>% add_row(Dimension = i,
                                     `Quantisation_error` = quant.error,
                                     `Topographic_error` = topo.error,
                                     `Kaski-Lagus_error` = kaski.error,
                                     `Explained_variance` = varratio.error)
    setTxtProgressBar(progbar,value=i)
  }
  close(progbar)
  error_df <- error_df %>%
    na.omit() %>%
    mutate(across(c(Quantisation_error, Topographic_error, `Kaski-Lagus_error`, Explained_variance), scale)) %>%
    mutate(QplusT_error = Quantisation_error + Topographic_error,
           QplusTplusK_error = Quantisation_error + Topographic_error + `Kaski-Lagus_error`,
           all_error = Explained_variance - QplusTplusK_error)
  QTerror_df <- error_df %>% filter(QplusT_error == min(QplusT_error, na.rm = TRUE))
  Kerror_df <- error_df %>% filter(`Kaski-Lagus_error` == min(`Kaski-Lagus_error`, na.rm = TRUE))
  QTKerror_df <- error_df %>% filter(QplusTplusK_error == min(QplusTplusK_error, na.rm = TRUE))
  Verror_df <- error_df %>% filter(Explained_variance == max(Explained_variance, na.rm = TRUE))
  All_errors_df <- error_df %>% filter(all_error == max(all_error, na.rm = TRUE))
  My_dim_QT <- as.numeric(QTerror_df$Dimension)
  My_dim_K <- as.numeric(Kerror_df$Dimension)
  My_dim_QTK <- as.numeric(QTKerror_df$Dimension)
  My_dim_V <- as.numeric(Verror_df$Dimension)
  My_dim_all <- as.numeric(All_errors_df$Dimension)
  return(data.frame("Quality measure"=c("Min nQe+nTe","Min nKLe","Min nQe+nTe+nKLe","Max n%ev", "Max QI"),
                    "Value"=c(QTerror_df$QplusT_error, Kerror_df$`Kaski-Lagus_error`, QTKerror_df$QplusTplusK_error,
                                Verror_df$Explained_variance, All_errors_df$all_error),
                    "Associated grid dimension"=c(My_dim_QT, My_dim_K, My_dim_QTK, My_dim_V, My_dim_all),
                    check.names = FALSE))
}
