#' Check whether two line segments cross each other.
#'
#' The line segments defined by the two input matrices are intersected.
#'
#' @param s1 2x2 matrix with coordinates of vertices of the first segment. First and second columns correspond
#' to \code{x} and \code{y} coordinates, respectively.
#' @param s2 2x2 matrix with coordinates of vertices of second segment, same as for \code{s1}.
#' @param include_limits logical determining whether an intersection between the ends of the line segments is valid.
#' Default is TRUE.
#' @param verbose logical determining whether warnings are issued when at least one of the segments has zero length
#' (i.e. it is a point) or segments are parallel. Default is TRUE.
#'
#' @details It implements basic planar geometric formulas.
#'
#' @return Coordinates \code{x-y} of intersection point, if it exists. If one of the segments is a point, or the two
#' segments are parallel, it returns NA. If segment are not parallel but they do not intersect, it returns FALSE.
#'
#' @examples
#'
#' ## An example of intersection.
#' s1 <- matrix(c(0, 1, 0, 1), 2, 2)
#' s2 <- matrix(c(0, 1, 1, 0), 2, 2)
#' plot(s1, type = "l", lwd = 2, xlab = "x", ylab = "y")
#' points(s2, type = "l", lty = 2, lwd = 2)
#' xp <- segments_intersection(s1, s2)
#' points(xp[1], xp[2],cex = 2)
#'
#' ## An example of segments that do not intersect.
#' plot(s1 + 3, type = "l", lwd = 2, xlab = "x", ylab = "y", xlim = c(0, 4), ylim = c(0, 4))
#' points(s2, type = "l", lty = 2,lwd = 2)
#' segments_intersection(s1 + 3, s2)
#'
#' # An example of parallel lines.
#' s1 <- matrix(c(1, 2, 1, 2), 2, 2)
#' s2 <- matrix(c(0, 3, 1, 4), 2, 2)
#' plot(s1, type = "l", lwd = 2, xlab = "x", ylab = "y", xlim = c(0, 4), ylim = c(0, 4))
#' points(s2, type = "l", lty = 2, lwd = 2)
#' segments_intersection(s1, s2)
#'
#' @export

segments_intersection <- function(s1, s2, include_limits = T, verbose = T) {
  
  stopifnot("Inputs 's1' and 's2' must be 2x2 matrices" = (is.matrix(s1) &
                                                             is.matrix(s2) &
                                                             all(dim(s1) %in% c(2,2)) &
                                                             all(dim(s2) %in% c(2,2))))
  
  # Numerators and denominators.
  n1 <- s1[2, 2] - s1[1, 2]
  n2 <- s2[2, 2] - s2[1, 2]
  d1 <- s1[2, 1] - s1[1, 1]
  d2 <- s2[2, 1] - s2[1, 1]

  # Checking segment characteristics.
  if ((n1 == 0 & d1 == 0) | (n2 == 0 & d2 == 0)) {
    if (verbose) warning("At least one line segment has null length (i.e. a point)")
    return(NA)
  }

  if (d1 == 0 & d2 == 0) {
    if (verbose) warning("Both line segments are parallel and vertical")
    return(NA)
  }

  if (n1 == 0 & n2 == 0) {
    if (verbose) warning("Both line segments are parallel and horizontal")
    return(NA)
  }

  # Line coefficients.
  a1 <- ifelse(d1 != 0, n1/d1, 0)
  a2 <- ifelse(d2 != 0, n2/d2, 0)
  if (a1 == a2) {
    if (verbose) warning("Both line segments are parallel")
    return(NA)
  }
  b1 <- s1[1, 2] - a1*s1[1, 1]
  b2 <- s2[1, 2] - a2*s2[1, 1]

  # Finds intersection point.
  xp <- (b2 - b1)/(a1 - a2)
  yp <- a1*xp + b1

  # Is the intersection point within segment limits?
  x1min <- min(s1[, 1])
  x1max <- max(s1[, 1])
  y1min <- min(s1[, 2])
  y1max <- max(s1[, 2])
  x2min <- min(s2[, 1])
  x2max <- max(s2[, 1])
  y2min <- min(s2[, 2])
  y2max <- max(s2[, 2])
  if (include_limits) {
    le <- function(u, v) {       # Function to check if u is less or equal than v.
      uu <- unname(u)
      vv <- unname(v)
      (uu < vv) | identical(TRUE, all.equal(uu, vv))
    }
    z1 <- le(x1min, xp) & le(xp, x1max) & le(y1min, yp) & le(yp, y1max)
    z2 <- le(x2min, xp) & le(xp, x2max) & le(y2min, yp) & le(yp, y2max)
  } else {
    z1 <- x1min < xp & xp < x1max & y1min < yp & yp < y1max
    z2 <- x2min < xp & xp < x2max & y2min < yp & yp < y2max
  }

  if (z1 & z2) return(c(x = unname(xp), y = unname(yp))) else return(FALSE)

}

