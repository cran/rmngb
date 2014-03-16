\name{LISP functions}
\alias{cons}
\alias{car}
\alias{cdr}
\alias{cadr}
\title{
LISP-like Functions
}
\description{
For the LISPstalgics.
}
\usage{
cons(a, b)
car(x)
cdr(x)
cadr(x)
}
\arguments{
  \item{a}{
first object to \code{cons}.
}
  \item{b}{
second object to \code{cons}.
}
  \item{x}{
\code{cons}ed object.
}
}
\value{
\code{cons} returns a functions that takes another function as an argument.
\code{car}, \code{cdr} and \code{cadr} return the objects initially \code{cons}ed.
}
\references{
SICP.
}
\author{
Antoine Filipovic Pierucci
}
\seealso{
\code{\link{list}}.
}
\examples{
a <- cons(rnorm(10), data.frame(a = 1:5, b = 6:10))
car(a)
cdr(a)
}
\keyword{ manip }
