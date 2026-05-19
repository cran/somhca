#' Estimate Optimal SOM Grid Size based on the Data
#'
#' Computes the optimal grid size for training a SOM using various quality
#' measures and heuristic approaches.
#'
#' @importFrom kohonen som somgrid
#' @importFrom aweSOM somQuality
#' @import dplyr
#' @importFrom stats sd
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @param data Matrix containing numeric data. A preprocessed data matrix containing the input data for SOM training. Argument is required.
#' @param method Character or integer. Method for estimating the maximum grid dimension. One of:
#'   \describe{
#'     \item{"A"}{Uses the heuristic formula by Vesanto et al. (default).}
#'     \item{"B"}{Applies an alternative heuristic approach.}
#'     \item{numeric}{Manually specified maximum dimension.}
#'   }
#' @param increments Integer. Step size for increasing grid dimensions.
#'   For example, set increments to 2 or 5 to increment the grid size by 2 or 5 rows/columns at each step.
#'   Smaller increments lead to more granular searches but may increase computation time;
#'   larger increments risk errors if they exceed the estimated maximum SOM grid dimensions.
#'   Default is 1.
#' @param iterations Integer. Number of iterations (`rlen`) for SOM training.
#'   If set to NULL (default), the function automatically calculates a sensible number
#'   of iterations based on the dataset size.
#'   If you want to override this, you can provide a numeric value.
#'   A lower value, such as less than 500, helps reduce computation time—if the process takes too long or an error occurs, try reducing the number of iterations for quicker results.
#'
#' @return A data frame summarizing optimal SOM grid dimensions and suggested iterations for each quality measure.
#'   Use these results to select the most suitable grid size for your SOM.
#'
#' @examples
#' # Create a toy matrix with 9 columns and 100 rows
#' data <- matrix(rnorm(900), ncol = 9, nrow = 100)  # 900 random numbers, 100 rows, 9 columns
#'
#' # Run the optimalSOM function with the mock data
#' myOptimalSOM <- optimalSOM(data, method = "A", increments = 2, iterations = 300)
#'
#' @export

optimalSOM <- function(data, method = "A", increments = 1, iterations = NULL) {

  # Basic safety checks
  data <- as.matrix(data)
  stopifnot(nrow(data) > 1, ncol(data) > 1)
  stopifnot(is.numeric(increments), increments >= 1)

  # Helper functions to calculate default rlen based on data rows
  estimate_rlen <- function(data,
                            passes = NULL,
                            min_rlen = 10,
                            max_rlen = 100) {

    n <- nrow(data)

    # Automatic default:
    # larger datasets need fewer passes
    if (is.null(passes)) {
      passes <- if (n < 1000) {
        50
      } else if (n < 10000) {
        30
      } else if (n < 100000) {
        20
      } else {
        10
      }
    }

    rlen <- round(passes)

    max(min_rlen, min(rlen, max_rlen))
  }

  qe_converged <- function(qe_hist,
                           epsilon = max(1e-6, 0.01 * sd(diff(qe_hist))),
                           warmup = 3) {

    k <- length(qe_hist)

    if (k <= warmup)
      return(FALSE)

    rel_improve <- (qe_hist[k-1] - qe_hist[k]) / qe_hist[k-1]

    rel_improve < epsilon
  }

  # Calculate the maximum dimension of a square grid size
  if(method=="A") {
    max_dim <- round(sqrt(5*sqrt(nrow(data)))) # Vesanto heuristic
  }
  else if(method=="B") {
    max_dim <- round((nrow(data))^(1/2.5)) # Alternative heuristic
  }
  else if(is.numeric(method)) {
    max_dim <- round(method) # Manual selection
  } else {
    stop("method must be 'A', 'B', or a numeric value")
  }

  if (max_dim < 2) {
    stop("Maximum grid dimension must be at least 2")
  }

  # Initialize dataframe to store SOM quality metrics
  error_df <- dplyr::tibble(
    Dimension = integer(),
    Quantisation_error = numeric(),
    Topographic_error = numeric(),
    `Kaski-Lagus_error` = numeric(),
    Explained_variance = numeric()
  )

  seq_dims <- seq(2, max_dim, increments)
  progbar <- txtProgressBar(min = 0, max = length(seq_dims), style = 3)

  qe_values <- numeric(length(seq_dims))
  qe_history <- numeric()
  no_improve <- 0
  printed_message <- FALSE

  for (idx in seq_along(seq_dims)) {

    i <- seq_dims[idx]

    My_Grid <- somgrid(
      xdim = i,
      ydim = i,
      topo = "hexagonal",
      toroidal = TRUE
    )

    # Determine rlen: use user-specified iterations or calculate automatically
    rlen_to_use <- if (is.null(iterations)) {
      rlen_auto <- estimate_rlen(data)
      if (!printed_message && interactive()) {
        message(sprintf(
          "Iterations automatically set to %d.",
          rlen_auto
        ))
        printed_message <- TRUE
      }
      rlen_auto
    } else {
      if (!printed_message && interactive()) {
        message(sprintf(
          "Using user-specified iterations: %d.",
          iterations
        ))
        printed_message <- TRUE
      }
      iterations
    }

    # Train SOM
    My_Model <- som(
      X = data,
      grid = My_Grid,
      rlen = rlen_to_use,
      alpha = c(0.05, 0.01),
      keep.data = TRUE
    )

    # Compute quality metrics
    sq <- aweSOM::somQuality(My_Model, data)

    qe_values[idx] <- as.numeric(sq$err.quant)

    error_df <- error_df %>%
      dplyr::add_row(
        Dimension = i,
        Quantisation_error = qe_values[idx],
        Topographic_error  = as.numeric(sq$err.topo),
        `Kaski-Lagus_error` = as.numeric(sq$err.kaski),
        Explained_variance = as.numeric(sq$err.varratio)
      )

    # QE plateau stopping rule
    qe_history <- c(qe_history, qe_values[idx])

    if (idx > 3) {

      if (qe_converged(qe_history)) {
        no_improve <- no_improve + 1
      } else {
        no_improve <- 0
      }

      if (no_improve >= 3) {
        message(
          sprintf(
            "QE plateau reached at grid %dx%d (delta QE < 0.2%%). Stopping grid expansion.",
            i, i
          )
        )
        utils::setTxtProgressBar(progbar, idx)
        break
      }
    }

    utils::setTxtProgressBar(progbar, idx)
  }

  close(progbar)

  # Standardize and compute combined error metrics
  error_df <- error_df %>%
    dplyr::mutate(
      dplyr::across(
        c(
          Quantisation_error,
          Topographic_error,
          `Kaski-Lagus_error`,
          Explained_variance
        ),
        ~ as.numeric(scale(.x))
      ),
      QplusT_error = Quantisation_error + Topographic_error,
      QplusTplusK_error = Quantisation_error + Topographic_error + `Kaski-Lagus_error`,
      all_error = Explained_variance - QplusTplusK_error
    )

  # Determine optimal grid for each quality measure
  QTerror_df  <- error_df %>% dplyr::slice_min(QplusT_error, n = 1, with_ties = TRUE)
  Kerror_df   <- error_df %>% dplyr::slice_min(`Kaski-Lagus_error`, n = 1, with_ties = TRUE)
  QTKerror_df <- error_df %>% dplyr::slice_min(QplusTplusK_error, n = 1, with_ties = TRUE)
  Verror_df   <- error_df %>% dplyr::slice_max(Explained_variance, n = 1, with_ties = TRUE)
  All_errors_df <- error_df %>% dplyr::slice_max(all_error, n = 1, with_ties = TRUE)

  # Extract dimensions
  My_dim_QT  <- QTerror_df$Dimension
  My_dim_K   <- Kerror_df$Dimension
  My_dim_QTK <- QTKerror_df$Dimension
  My_dim_V   <- Verror_df$Dimension
  My_dim_all <- All_errors_df$Dimension

  # Calculate rlen for each optimal dimension
  estimate_rlen_final <- function(data, grid_dim, min_rlen = 500, max_rlen = 5000) {
    n <- nrow(data)
    n_neurons <- grid_dim^2  # assuming square grid
    rlen <- round(n * log10(n_neurons))
    max(min_rlen, min(rlen, max_rlen))
  }

  rlen_QT  <- estimate_rlen_final(data, My_dim_QT)
  rlen_K   <- estimate_rlen_final(data, My_dim_K)
  rlen_QTK <- estimate_rlen_final(data, My_dim_QTK)
  rlen_V   <- estimate_rlen_final(data, My_dim_V)
  rlen_all <- estimate_rlen_final(data, My_dim_all)

  # Return dataframe with optimal grid dimensions and suggested rlen
  return(data.frame(
    "Quality measure" = c(
      "Min nQe+nTe",
      "Min nKLe",
      "Min nQe+nTe+nKLe",
      "Max n%ev",
      "Max QI"
    ),
    "Value" = c(
      QTerror_df$QplusT_error,
      Kerror_df$`Kaski-Lagus_error`,
      QTKerror_df$QplusTplusK_error,
      Verror_df$Explained_variance,
      All_errors_df$all_error
    ),
    "Associated grid dimension" = c(
      My_dim_QT,
      My_dim_K,
      My_dim_QTK,
      My_dim_V,
      My_dim_all
    ),
    "Suggested iterations" = c(
      rlen_QT,
      rlen_K,
      rlen_QTK,
      rlen_V,
      rlen_all
    ),
    check.names = FALSE
  ))
}
