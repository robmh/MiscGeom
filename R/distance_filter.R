#' @title
#' Distance-based spatial filtering
#' 
#' @description
#' Function \code{distance_filter} applies a spatial filter to a set of N-dimensional coordinates such that
#' the resulting points are separated by at least a minimum distance.
#' 
#'
#' @param df numeric \code{data.frame} with as many rows as points to be included in the analysis.
#' @param min_dist \code{numeric}, minimum distance value between points.
#' @param shuffle  \code{logical}, set it to \code{TRUE} to randomly shuffle the points in \code{df}.
#' If not set, algorithm will always start at first row and will move down to the last one.
#' By default it is set to \code{TRUE}, which means that every time \code{distance_filter} is
#' called the result will likely be different.
#' @param method The method to be used (see \link[stats]{dist} for details).
#' @param columns character vector, name of the columns in \code{df} to be used to measure distance.
#' @param verbose logical, if TRUE a progress bar will be printed on terminal.
#'
#' @return
#' A \code{data.frame} with as many columns as \code{df} and with rows containing points that are
#' separated by at least a distance \code{min_dist}. Coordinates for points can be of any dimension, i.e.
#' \code{df} can have as many columns as needed (distances are calculated with the built-in \link[stats]{dist} function).
#' If all distances are less than \code{min_dist} NA is returned.
#' 
#' @export
#'
#' @examples
#' 
#' # Random set of 2-D coordinates.
#' df <- data.frame(x1 = runif(100)*10, x2 = runif(100)*10)
#' 
#' # Compute filtered and shuffled+filtered datasets. If you repeat the lines below
#' # you'll notice that the "Original" and the "Filtered" plots do not change.
#' par(mfcol = c(1, 3))
#' plot(df, xlim = c(0, 10), ylim = c(0, 10), main = "Original")
#' df_filt <- distance_filter(df, 2, shuffle = F)
#' plot(df_filt, xlim = c(0, 10), ylim = c(0, 10), main = "Filtered")
#' df_shuff_filt <- distance_filter(df, 2, shuffle = T)
#' plot(df_shuff_filt, xlim = c(0, 10), ylim = c(0, 10), main = "Shuffled + Filtered")
#' par(mfcol = c(1, 1))
#' 

distance_filter <- function(df, min_dist, columns = NULL, method = "euclidean", shuffle = T, verbose = T) {


  # Input must be a data.frame or a 2D matrix.
  stopifnot("Input 'df' must be a data.frame or a matrix" = any(c(is.data.frame(df), is.matrix(df), length(dim(df)) == 2)))

  
  # Check columns and select those that will be used to determine distance.
  if (is.null(columns)) {
    dfcoord <- df
  } else {
    stopifnot("All names in 'columns' must match column names in 'df'" = all(columns %in% colnames(df)))
    dfcoord <- df[, columns]
  }


  # Computation of distance matrix.
  dist_df <- as.matrix(dist(dfcoord, method = method, diag = T))
  diag(dist_df) <- max(dist_df, na.rm = T) + 1
  
  
  # Random shuffle, if set.
  id <- 1:nrow(dfcoord)
  if (shuffle) id <- sample(id)
  j <- id[1]
  id <- id[-1]
  
  
  # Progress bar on screen.
  if (verbose) {
    cat("\n -> distance_filter: finding points whose distance is > min_dist...\n")
    pb <- txtProgressBar(min = 0,
                         max = length(id),
                         style = 3,
                         width = 50,
                         char = "=")
  }


  # Loop through remaining points. We pick them one by one, keeping only the good ones.
  if (verbose) icount <- 0
  for (i in id) {
    
    
    # Progress bar.
    if (verbose) {
      icount <- icount + 1
      setTxtProgressBar(pb, icount)
    }
    
    
    k <- c(j, i)
    if (min(dist_df[k, k]) > min_dist) j <- k
  }

  
  # If no points are found it returns NA.
  if (length(j) == 1) df <- NA else df <- df[j, ]
  
  
  if (verbose) cat("\n")
  
  
  return(df)   
  
}