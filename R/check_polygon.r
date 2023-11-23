#' Test for simple polygon
#'
#' It checks whether the sides of a general polygon do not intersect each other.
#'
#' @param xy 2D matrix with x-y coordinates of vertices.
#'
#' @details
#'
#' @return If
#'
#' @examples
#'
#' ## A simple polygon.
#' x <- cumsum(runif(10))
#' y <- cumsum(runif(10))*x^4
#' plot(x,y,type="l",lty=1,lwd=2)
#' points(c(x[10],x[1]),c(y[10],y[1]),type="l",lty=2,lwd=2)
#' p <- check_polygon(cbind(x,y))
#'
#' @export

check_polygon <- function(xy) {

  nx <- nrow(xy)

  p <- list()
  k <- 0
  for (i in 1:(nx-2)) {
    s1 <- xy[i:(i+1),]
    for (j in (i+1):(nx-1)) {
      s2 <- xy[j:(j+1),]
      q <- segments_intersection(s1, s2, include_limits = F)
      if (!isFALSE(q) & !any(is.na(q))) {
        k <- k + 1
        p[[k]] <- list(i=i,j=j,s1=s1,s2=s2,q)
      }
    }
  }

  if (k == 0) p <- T
  return(p)
}

