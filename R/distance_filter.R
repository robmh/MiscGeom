#' Title
#'
#' @param xy numeric \code{data.frame} with as many rows as points to be included in the analysis
#' @param min_dist \code{numeric}, minimum distance value between points. 
#' @param shuffle  \code{logical}, set it to \code{TRUE} to randomly sample rows in \code{xy}.
#' If not set, algorithm will always start at first row and will move down to the last one.
#' By default it is set to \code{TRUE}, which means that every time \code{distance_filter} is
#' run the result will likely be different.
#' @param method The method to be used (see \link[stats]{dist} for details).
#'
#' @return
#' @export
#'
#' @examples
distance_filter <- function(xy, min_dist, method = "euclidean", shuffle = T) {

  
  stopifnot("Input 'xy' must be a data.frame" = is.data.frame(xy))
  if (nrow(xy) < 2) return(xy)
  

  # Computation of distance matrix.
  dxy <- as.matrix(dist(xy, method = method, diag = T))
  diag(dxy) <- 1E64
  
  
  # Random shuffle, if set.
  id <- 1:nrow(xy)
  j <- if (shuffle) sample(id, 1) else 1
  id <- id[-j]


  # Loop through remaining points. We pick them one by one, keeping only the good ones.
  for (i in id) {
    k <- c(j, i)
    if (min(dxy[k, k]) > min_dist) j <- k
  }

  
  return(xy[j, ])   
  
}