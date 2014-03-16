\name{\%out\%}
\alias{\%out\%}
\title{
The Opposite of \code{\%in\%}
}
\description{
Duh.
}
\usage{
x \%out\% y
}
\arguments{
  \item{x}{
vector or NULL: the values to be matched.
}
  \item{y}{
vector or NULL: the values to be matched.
}
}
\details{
That \code{\%out\%} never returns \code{NA} makes it particularly useful in \code{if} conditions.
}
\value{
A logical vector of the same length as \code{x}, indicating if a match was located for each element of \code{x}: thus the values are \code{TRUE} or \code{FALSE} and never \code{NA}.
}
\author{
Antoine Filipovic Pierucci
}
\seealso{
\code{\link{match}}. \code{\link{pmatch}} and \code{\link{charmatch}} for (partial) string matching, \code{\link{match.arg}}, etc for function argument matching. \code{\link{findInterval}} similarly returns a vector of positions, but finds numbers within intervals, rather than exact matches.
}
\examples{
1:5 \%out\% 3:6
letters[letters \%out\% c("a", "b")]
}
\keyword{ logic }
