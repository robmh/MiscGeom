#' Total length of curve
#'
#' It calculates the total length of a curve made up of a series of segments.
#'
#' @param s data.frame with at least two columns named "\code{x}" and "\code{y}" containing
#' the cartesian coordinates of segments' points. Segments must be consecutive.
#'
#' @details Total length is computed by adding the lengths of the individual segments.
#'
#' @return a number giving the total length.
#'
#' @examples
#' x <- runif(10)
#' y <- rnorm(10)
#' s <- data.frame(x=x,y=y)
#' calc_seg_length(s)
#' 
#' @export

calc_seg_length <- function(s) {
  
  if (!is.data.frame(s)) stop("s must be a data.frame")
  if (ncol(s) != 2) stop("s must have at least two columns")
  if (!all(c("x","y") %in% colnames(s))) stop("s must have 'x' and 'y' columns")
  nx <- nrow(s)
  if (nx<2) stop("There must be at least two points to define a segment")

  ds <- sqrt((s$x[-1]-s$x[-nx])^2+(s$y[-1]-s$y[-nx])^2)
  return(sum(ds))
}
