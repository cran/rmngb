\name{plotDensity}
\alias{plotDensity}
\title{
Histogram and Density Plot
}
\description{
Plot a histogram and a gaussian kernel density estimate of the data.
}
\usage{
plotDensity(x, col = "black", lwd = 2, ...)
}
\arguments{
  \item{x}{
a vector of values for which the histogram/kernel density is desired.
}
  \item{col}{
color of the kernel density line.
}
  \item{lwd}{
line width of the kernel density line.
}
  \item{...}{
additional arguments passed to \code{plot}.
}
}
\value{
Nothing of interest.
}
\author{
antoine Filipovic Pierucci
}
\seealso{
\code{\link{hist}}, \code{\link{density}}.
}
\examples{
plotDensity(rnorm(50))
}
\keyword{ misc }
