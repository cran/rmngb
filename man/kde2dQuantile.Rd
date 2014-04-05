\name{kde2dQuantile}
\alias{kde2dQuantile}

\title{
Two-Dimensional Kernel Density Estimation Quantiles
}
\description{
Produces sample quantiles from a two-dimensional kernel density estimation corresponding to the given probabilities.
}
\usage{
kde2dQuantile(d, X, Y, probs = .05, ...)
}
\arguments{
  \item{d}{
an object returnd by \code{\link[MASS]{kde2d}}.
}
  \item{X}{
numeric vector of \code{x} coordinate of data whose sample quantiles are wanted.
}
  \item{Y}{
numeric vector of \code{y} coordinate of data whose sample quantiles are wanted.
}
  \item{probs}{
numeric vector of probabilities.
}
  \item{...}{
additional arguments passed to \code{\link{quantile}}.
}
}
\details{
Useful for contour plots of 2-dimensional KDE quantiles.
}
\value{
A vector of quantiles corresponding to the given probabilities.
}
\author{
Antoine Filipovic Pierucc
}
\seealso{
\code{\link[MASS]{kde2d}}, \code{\link{contour}}.
}
\examples{
a <- rnorm(10)
b <- rnorm(10)
# an ugly imitation of a kde2d estimate
# since we don't want to load MASS just for an exemple
f1 <- structure(list(x = c(-3.1, -1.575, -0.05, 1.475, 3),
                     y = c(-3.213, -1.591, 0.032, 1.654, 3.276),
                     z = structure(c(0, 0, 0.003, 0, 0, 0,
                                     0.009, 0.05, 0.015,
                                     0, 0.001, 0.05, 0.152,
                                     0.052, 0.003, 0, 0.01,
                                     0.039, 0.028, 0.001, 0,
                                     0, 0.001, 0, 0),
                                   .Dim = c(5L, 5L))),
                     .Names = c("x", "y", "z"))

quant <- kde2dQuantile(f1, a, b, c(.05, .5))

contour(f1, levels = quant, labels = names(quant))
}
\keyword{ misc }