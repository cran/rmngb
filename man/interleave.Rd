\name{interleave}
\alias{interleave}
\title{
Interleave vectors
}
\description{
Interleave two vectors of arbitrary length.
}
\usage{
interleave(x, y)
}
\arguments{
  \item{x}{
  first vector to interleave.
}
  \item{y}{
second vector to interleave.
}
}
\details{
\code{x} and \code{y} need not be of the same length nor of the same class.
}
\value{
An interleaved vector.
}
\references{
Original function posted on R-help by Bogdan Romocea.
}
\author{
Antoine Filipovic Pierucci
}

\examples{
# same length
a <- letters[1:5]
b <- letters[10:15]
interleave(a, b)

# different length
x <- letters[1:2]
y <- letters[10:15]
interleave(x, y)
}
\keyword{ ~misc }
