\name{locf}
\alias{locf}

\title{
Last Observation Carried Forward
}
\description{
Returns the last non-missing observation in a vector.
}
\usage{
locf(x)
}

\arguments{
  \item{x}{
vector.
}
}
\value{
Scalar corresponding to the last non-missing observation.
}
\author{
Antoine Filipovic Pierucci
}

\examples{
tab <- data.frame(a1 = c(1, 4, 6),
                  a2 = c(3, NA, NA),
                  a3 = c(4, NA, 5))
apply(tab, 1, locf)
}

\keyword{ misc }