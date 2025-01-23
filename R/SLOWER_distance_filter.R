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
#' @param strict 
#' @param columns character vector, name of the columns in \code{df} to be used to measure distance.
#' @param method The method to be used (see \link[stats]{dist} for details).
#' @param shuffle  \code{logical}, set it to \code{TRUE} to randomly shuffle the points in \code{df}.
#' If not set, algorithm will always start at first row and will move down to the last one.
#' By default it is set to \code{TRUE}, which means that every time \code{distance_filter} is
#' called the result will likely be different.
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
#' df <- data.frame(x1 = runif(10000)*10, x2 = runif(10000)*10)
#' 
#' # Compute filtered and shuffled+filtered datasets. If you repeat the lines below
#' # you'll notice that the "Original" and the "Filtered" plots do not change.
#' par(mfcol = c(1, 3))
#' plot(df, xlim = c(0, 10), ylim = c(0, 10), main = "Original", pch = 16, cex = .1)
#' df_filt <- distance_filter(df, 2, shuffle = F)
#' plot(df_filt, xlim = c(0, 10), ylim = c(0, 10), main = "Filtered")
#' df_shuff_filt <- distance_filter(df, 2, shuffle = T)
#' plot(df_shuff_filt, xlim = c(0, 10), ylim = c(0, 10), main = "Shuffled + Filtered")
#' par(mfcol = c(1, 1))
#' 
#' 
#' # If the grid is perfectly squared and interpoint distance is >=2 exactly,
#' the output is exactly as the input when 'strict' is FALSE.
#' df <- as.data.frame(expand.grid(seq(0, 10, by = 2), seq(0, 10, by = 2)))
#' df_square <- distance_filter(df, 2, shuffle = FALSE, strict = FALSE)
#' identical(df, df_square)
#' 
#' # If we add a small amount of jittering to the coordinates, the results change a great deal.
#' df <- data.frame(jitter(df[, 1], .001), jitter(df[, 2], .001))
#' df_jitter <- distance_filter(df, 2, shuffle = FALSE, strict = FALSE)
#' 

distance_filter <- function(df, min_dist, strict = TRUE, columns = NULL, method = "euclidean", shuffle = TRUE, verbose = TRUE) {


  # Input must be a data.frame or a 2D matrix.
  stopifnot("Input 'df' must be a data.frame or a matrix" = any(c(is.data.frame(df), is.matrix(df), length(dim(df)) == 2)))


  # Check columns and select those that will be used to determine distance.
  if (is.null(columns)) {
    stopifnot("There must be at least two columns" = ncol(df) == 2)
    dfcoord <- df
  } else {
    stopifnot("Only two columns can be selected" = length(columns) == 2)
    stopifnot("All names in 'columns' must match actual column names in 'df'" = all(columns %in% colnames(df)))
    dfcoord <- df[, columns]
  }


  # Define a new operator to choose whether to apply '>' or '>='.
  "%>=>%" <- function(x, y) if(strict) x > y else x >= y
  

  # Random shuffle, if set.
  nr <- nrow(df)
  rownames(dfcoord) <- 1:nr
  if (shuffle) {
    id <- sample(1:nr)
    dfcoord <- dfcoord[id, ]
  }
  
  
  # Computation of distance matrix. Fill diagonal with a very large number.
  dist_df <- as.matrix(dist(dfcoord, method = method, diag = FALSE))
  if (sum(!(dist_df %>=>% min_dist)) == 0) {
    cli::cli_alert("All distances are larger than 'min_dist'")
    return(df)
  }
  diag(dist_df) <- min_dist + 1
  dist_df <- dist_df %>=>% min_dist
  
  
  # Repeat loop keeps eliminating cells or entire rows/columns.
  i <- 1
  repeat {
    # print(c(i, dim(dist_df)))
    j <- which(dist_df[i, ])
    if (length(j) > 0) {
      dist_df <- dist_df[j, j]
      i <- i + 1
    } else {
      dist_df <- dist_df[-i, -i]
    }
    
    if (i >= nrow(dist_df)) break
  }
  
  
  # Get indices from row (or column) names and select data.
  i <- as.numeric(rownames(dist_df))
  df <- df[i, ]


  
  # Progress bar on screen.
  # if (verbose) {
  #   cat(paste0("\n -> distance_filter: finding points whose distance is > ", min_dist, "...\n"))
  #   pb <- txtProgressBar(min = 0,
  #                        max = length(id),
  #                        style = 3,
  #                        width = 50,
  #                        char = "=")
  # }
  # 
  # 
  # # Loop through remaining points. We pick them one by one, keeping only the good ones.
  # if (verbose) icount <- 0
  # for (i in id) {
  #   
  #   
  #   # Progress bar.
  #   if (verbose) {
  #     icount <- icount + 1
  #     setTxtProgressBar(pb, icount)
  #   }
  #   
  #   
  #   # Check distances.
  #   di <- dist_df[i, ]
  #   k <- which(di > min_dist)
  #   if (length(k) > 0) 
  #   
  #   
  #   k <- c(j, i)
  #   if (min(dist_df[i, k]) > min_dist) j <- k
  # }


  if (verbose) icount <- 0

# 
#   for (i in id) {
#     
#     # Progress bar.
#     if (verbose) {
#       icount <- icount + 1
#       setTxtProgressBar(pb, icount)
#     }
    
  # j <- id[1]
  # id <- id[-1]
  # for (i in 1:nr) {
  #   
  #   # Is it valid?
  #   if (!(i %in% id)) {
  #     
  #     # Search.
  #     k <- which(dist_df[i, id] %>=>% min_dist)
  #     
  #     # Condition fulfilled?
  #     if (length(k) > 0) {
  #       j <- c(j, k)
  #     }
  #   }
  #   browser()
  # }


  # If no points are found it returns NA.
  # if (length(j) == 1) df <- NA else df <- df[j, ]
  
  
  # if (verbose) cat("\n")
  
  
  return(df)   
  
}