% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_seg_length.r, R/calc_seg_length_OLD.r
\name{calc_seg_length}
\alias{calc_seg_length}
\title{Total length of curve}
\usage{
calc_seg_length(s, type = "simple", method = "natural", npoints = 1000)

calc_seg_length(s, type = "simple", method = "natural", npoints = 1000)
}
\arguments{
\item{s}{data.frame with at least two columns named "\code{x}" and "\code{y}" containing
the cartesian coordinates of segments' points. Segments must be consecutive.}

\item{type}{character specifying how to calculate length. If not set, or set to "\code{simple}",
total length is computed by assuming linear segments.
If set to "\code{spline}", a natural spline interpolation is carried out before
distances are summed.}

\item{method}{specifies the type of spline to be used. See help to base-R \code{spline} function.}

\item{npoints}{if \code{type} is set to "\code{spline}", interpolation takes place at
\code{npoints} equally spaced points between minimum and maximum abscissas. Default is 1000.}
}
\value{
a number giving the total length.

a number giving the total length.
}
\description{
It calculates the total length of a curve made up of a series of segments.

It calculates the total length of a curve made up of a series of segments.
}
\details{
Total length is computed by adding the lengths of the individual segments.

Total length is computed by adding the lengths of the individual segments.
If code{type='spline'}, segments will be curves too, modelled by splines.
Though given here as an option, splines are not recommended to compute total length
unless points are grouped very tighted (and, therefore, spline-interpolated curves do not
'wander' wildly around measured points).
}
\examples{
x <- runif(10)
y <- rnorm(10)
s <- data.frame(x=x,y=y)
calc_seg_length(s)

s <- data.frame(x=runif(10),y=runif(10))
calc_seg_length(s)
calc_seg_length(s,"spline")

s <- data.frame(x=1:10,y=sin((1:10)/pi))
calc_seg_length(s)
calc_seg_length(s,"spline")

x <- seq(-1,1,length=100)
y <- sqrt(1-x^2)
s <- data.frame(x=x,y=y)

}
