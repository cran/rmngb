\name{vswitch}
\alias{vswitch}
\alias{vswitch.default}
\alias{vswitch.character}
\alias{vswitch.factor}
\title{
Element Selection from a List of Alternatives
}
\description{
\code{vswitch} returns a value with the same shape as \code{x} which is filled with elements selected from\code{...} depending on the values of \code{x}.
}
\usage{
vswitch(x, ...)
\method{vswitch}{default}(x, ...)
\method{vswitch}{factor}(x, ...)
\method{vswitch}{character}(x, ...)
}
\arguments{
  \item{x}{
\code{integer}, \code{factor} or \code{character}. Which element of \code{...} should be selected.
}
  \item{...}{
vectors. If \code{x} is \code{character} or \code{factor} elements in \code{...} should be named. If \code{x} is \code{integer} names will be ignored with a warning.
}
}
\details{
The class of the return value depends on the class(es) of \code{...}. All elements in \code{...} are coerced to the same class with the usual rules.

If \code{x} is \code{character} or \code{factor} then \code{...} should be named. All values of \code{x} should correspond to names in \code{...}.

If \code{x} is \code{integer} then names in \code{...} will be ignored with a warning. Furthermore if values of \code{x} are smaller than \code{1} or greater than the number of elements in \code{...} an error will be produced.

\code{NA} values in \code{x} return \code{NA}.
}
\value{
A vector of the same length as \code{x} and a class depending on the result of the coercion of all the elements in \code{...} to the same class.
}
\author{
Antoine Filipovic Pierucci
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ifelse}}, \code{\link{switch}}.
}
\examples{
a <- 1:5
b <- 11:15
c <- 21:25

condChar <- c('d', 'e', 'f', 'd', 'e')
condInt <- c(1, 2, 3, 1, 2)
condFac <- as.factor(condChar)
condMissing <- c(1, NA, 3, NA, 2)

vswitch(x = condChar, d = a, e = b, f = c)
vswitch(x = condInt, a, b, c)
vswitch(x = condFac, d = a, e = b, f = c)
vswitch(x = condMissing, a, b, c)
}
\keyword{ misc }
