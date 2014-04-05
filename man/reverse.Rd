\name{reverse}
\alias{reverse}
\title{
Reverse Elements of a Vector or a List
}
\description{
Reverse the order of the elements in a vector or a list.
}
\usage{
reverse(x)
}
\arguments{
  \item{x}{
list or vector to reverse
}
}
\details{
Using \code{x} with more than 1 dimension will produce an error.

For lists, the function is non-recursive. Only the topmost list will be reversed.
}
\value{
Same object as \code{x}, reversed.
}
\author{
Antoine Filipovic Pierucci
}
\examples{
a <- 1:10
b <- list(1, 3, 5)
reverse(a)
reverse(b)
# non recursiveness
c <- list(1, 3, b)
reverse(c)
}
\keyword{ misc }
