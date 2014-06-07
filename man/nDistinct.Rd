\name{nDistinct}
\alias{nDistinct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Number of Distinct Elements
}
\description{
Count the number of distinct elements in a vector or a matrix.
}
\usage{
nDistinct(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a vector or a matrix.
}
}
\value{
A scalar giving te number of distinct elements in \code{x}.
}
\author{
Antoine Filipovic Pierucci
}
\examples{
nDistinct(c(1, 3, 3, 5))
}
\keyword{ misc }
