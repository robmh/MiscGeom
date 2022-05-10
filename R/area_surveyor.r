#' Shoelace or Surveyor' formula.
#'
#' Surveyor' formula (also known as Shoelace formula) is used to calculate the area of an arbitrary polygon
#' defined by its vertices x and y.
#'
#' @param p data.frame with at least two columns and as many rows as vertices. Columns with coordinates
#' must be named \code{x} and \code{y} in lower or upper case letters.
#'
#' @details Surveyor or shoelace formula. Polygon \code{p} does not have to be simple for the Shoelace
#' formula to work. See Wikipedia for a detailed description.
#'
#' @return Area of polygon enclosed by (\code{x},\code{y}) coordinates.
#'
#' @examples
#'
#' ## A simple polygon.
#' x <- c(0,1,2,3,2,1)
#' y <- c(3,4,4,1,0,0)
#' p <- data.frame(x=x,y=y)
#' area_surveyor(p)
#' 
#' # Geometric approximation to pi.
#' n <- 100:1000
#' pi_approx <- sapply(n,function(i) {
#' x <- seq(-1,1,length=i)
#' y <- sqrt(1-x^2)
#' area_surveyor(data.frame(x=x,y=y))*2
#' })
#' plot(n,pi_approx,log="xy",type="l",xlab="N. of segments",ylab="Approx. to pi",ylim=c(3.138,3.142))
#' points(c(min(n),max(n)),c(pi,pi),type="l",lty=2,lwd=2)
#' 
#' 
#'
#' @export

area_surveyor <- function(p) {

  if (!is.data.frame(p)) stop("p must be a data.frame")
  if (ncol(p) != 2) stop("p must have at least two columns")
  if (!all(c("x","y") %in% colnames(p))) stop("p must have 'x' and 'y' columns")
  nx <- nrow(p)
  if (nx < 3) stop("Polygon p must have at least three vertices")

  # Surveyor formula.
  a <- sum(p$x[-nx]*p$y[-1]) + p$x[nx]*p$y[1] - sum(p$x[-1]*p$y[-nx]) - p$x[1]*p$y[nx]

  return(abs(a)/2)
}

