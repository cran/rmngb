\name{outerList}
\alias{outerList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Outer Product of Lists
}
\description{
The outer product of the lists X and Y is the matrix M with dimension c(length(X), length(Y)) where element M[i, j] = FUN(X[[i]], Y[[j]]).
}
\usage{
outerList(X, Y, FUN, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
a list.
}
  \item{Y}{
a list.
}
  \item{FUN}{
a function to use on the outer product, taking elements of X and Y as arguments.
}
  \item{...}{
additional arguments passed to FUN.
}
}
\details{
X and Y must be suitable arguments for FUN. Each will be extended by \code{\link{rep}} to length the products of the lengths of X and Y before FUN is called.

FUN is the applied to these two extended vectors by \code{\link{mapply}}.

When they exist, the names of X and Y will be used as rownames and colnames.
}
\value{
A matrix with dimension c(length(X), length(Y)) where element M[i, j] = FUN(X[[i]], Y[[j]]).
}
\author{
Antoine Filipovic Pierucci
}
\seealso{
\code{\link{outer}}, \code{\link{mapply}}.
}
\examples{
a <- list(a = 1:3, b = 4:7, c = 8:9)
b <- list(x = 4:8, y = 0:1)
f <- function(x, y)
    any(match(x, y, nomatch = 0L) > 0L)

outerList(a, b, f)
}
\keyword{ misc }
