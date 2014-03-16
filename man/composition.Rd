\name{composition}
\alias{composition}
\title{
Compose Functions
}
\description{
Returns a composition of several functions.
}
\usage{
composition(...)
}
\arguments{
  \item{...}{
functions to compose.
}
}
\value{
A function.
}
\author{
Antoine Filipovic Pierucci
}
\examples{
f <- composition(mean, log, function(x) x^2)
f(1:10) == mean(log((1:10)^2))
}
\keyword{ misc }
