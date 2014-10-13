\name{Closest matching}
\alias{Closest matching}
\alias{closest}

\title{
Approximate Matching
}
\description{
These function provide different utilities to perform approximate matching of numbers.
}
\usage{
closest(x, y, tol = +Inf)
}

\arguments{
  \item{x}{
numeric scalar. Value to be approximatly matched.
}
  \item{y}{
numeric vector of values to be matched with \code{x}.
}
  \item{tol}{
numeric scalar giving the maximum difference in absolute value to perform a match.
}
}
\details{
Proximity is calculated by the absolute value of the difference.
}
\value{
\code{closest} returns a logical vector the same length as \code{y}, \code{TRUE} for the closest match(es) within \code{tol} bounds and \code{FALSE} otherwise.
}
\author{
Antoine Filipovic Pierucci
}
\seealso{
\code{\link{match}}.
}
\examples{
ref <- seq(1:5)
closest(2.8, ref)
closest(2.5, ref, .1) # no match because of lower tolerance
}
\keyword{ misc }
