\name{Closest matching}
\alias{Closest matching}
\alias{closest}
\alias{whichClosest}
\alias{whichClosest1}

\title{
Approximate Matching
}
\description{
These function provide different utilities to perform approximate matching of numbers.
}
\usage{
closest(x, y, tol = +Inf)
whichClosest(...)
whichClosest1(...)
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
\item{...}{
arguments passed to \code{closest}.
}
}
\details{
Proximity is calculated by the absolute value of the difference.

\code{whichClosest1} systematically picks the first match, if any.
}
\value{
\code{closest} returns a logical vector the same length as \code{y}, \code{TRUE} for the closest match(es) within \code{tol} bounds and \code{FALSE} otherwise.

\code{whichClosest} returns the index of the match(es) in \code{y}.

\code{whichClosest1} is the same as \code{whichClosest}, but the length of the return value is guaranteed to be \code{<= 1}.
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
whichClosest(2.8, ref)
whichClosest(2.5, ref) # 2 matched
whichClosest1(2.5, ref) # only 1
whichClosest1(2.5, ref, .1) # no match because of lower tolerance
}
\keyword{ misc }
