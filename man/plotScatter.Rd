\name{plotScatter}
\alias{plotScatter}
\title{
Scatterplot with LOWESS Line
}
\description{
Plots a standard scatterplot and overlays a locally-weighted polynomial regression line.
}
\usage{
plotScatter(x, y, lty = 1, lwd = 2, colLine = "red", ...)
}
\arguments{
  \item{x}{
the \code{x} coordinates of points in the plot.
}
  \item{y}{
the \code{y} coordinates of points in the plot.
}
  \item{lty}{
line type.
}
  \item{lwd}{
line width.
}
  \item{colLine}{
line colour.
}
  \item{...}{
additional arguments passed to \code{plot}.
}
}
\value{
Nothing of interest.
}
\author{
Antoine Filipovic Pierucci
}
\seealso{
\code{\link{plot}}, \code{\link{lowess}}.
}
\examples{
plotScatter(rnorm(50), rnorm(50))
}
\keyword{ misc }
