\name{rDist}
\alias{rDist}
\alias{plot.rDist}
\title{
Generate Random Numbers from a Gaussian Kernel Density Estimation
}
\description{
The function \code{rDist} returns a random generation function for a distribution computed through a gaussian kernel density estimate.
}
\usage{
rDist(x, ...)
\method{plot}{rDist}(x, ...)
}
\arguments{
  \item{x}{
the data from which the estimate is to be computed.
}
  \item{...}{
additional arguments passed to \code{\link{density}}.
}
}
\details{
\code{rDist} uses the \code{\link{density}} function to compute a gaussian kernel density estimation of \code{x}. Additional arguments can be passed to \code{density} through \code{...}.
It returns a function that can be used to generate random values from this estimated density.

It is also possible to have a look at the kernel density estimate by calling \code{\link{plot}} on the returned function.
}
\value{
A function of class \code{rDist}.
}
\note{
\code{density} is actually only used to compute a bandwidth and for its plotting method.

Only a gaussian kernel can be used.
}
\author{
Antoine Filipovic Pierucci
}
\seealso{
\code{\link{density}}, \code{\link{rnorm}}.
}
\examples{
x <- c(rnorm(100), rnorm(100, 4))

rTest <- rDist(x)
plot(rTest) # look at the estimate
rTest(10) # generate random values from the estimate
hist(rTest(1e3)) # density of randomly generated values
}
\keyword{ distribution }
