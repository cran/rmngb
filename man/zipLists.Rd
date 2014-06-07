\name{zipLists}
\alias{zipLists}
\title{
Combination of Lists
}
\description{
Makes a list of length n lists, each length n list containing elements of the n input lists occuring at the same position.
}
\usage{
zipLists(...)
}
\arguments{
  \item{...}{
lists.
}
}
\details{
If any elements of \code{...} are too short, their elements are recycled to the length of the longest, unless any have length zero.
}
\value{
A list of length n lists, each length n list containing elements of the n input lists occuring at the same position. This list is as long as the longest element from \code{...}.
}
\author{
Antoine Filipovic Pierucci
}

\seealso{
\code{\link{mapply}}.
}
\examples{
l1 <- list(a = 1:3, b = 4:5)
l2 <- list(x = letters[1:2], y = letters[5:7])
l3 <- list(1)

zipLists(l1, l2, l3)
}
\keyword{ misc }
