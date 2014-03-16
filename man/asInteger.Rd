\name{asInteger}
\alias{asInteger}

\title{
Floating Point Numbers to Integers
}
\description{
When numbers should be integers but aren't because of floating point arithmetic issues.
}
\usage{
asInteger(x)
}
\arguments{
  \item{x}{
object to be coerced.
}
}
\details{
Standard coercion to integer using \code{as.integer} truncates floats.
That behaviour can produce unexpected results when the number is used for subsetting or in a comparaison.
}
\value{
An integer vector.
}
\author{
Antoine Filipovic Pierucci
}

\seealso{
\code{\link{as.integer}}, \code{\link{round}}.
}
\examples{
(z <- .3 / .1) == 3 # FALSE
as.integer(z) == 3 #FALSE
asInteger(z) == 3 # TRUE

x <- letters[1:5]
x[z] # not "c"
x[asInteger(z)] # "c"
}
\keyword{ manip }
