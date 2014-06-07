\name{colClasses}
\alias{colClasses}
\title{
Column Classes in a Dataframe
}
\description{
Returns a vector of basic classes of the colums of a dataframe.
}
\usage{
colClasses(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a dataframe.
}
}
\details{
Since the return value is a vector with one element per colunm, column with multiple classes (such as \code{ordered} and \code{factor}) see their classes simplified as the last class from the class vector.
}
\value{
A named vector of the same length as \code{ncol(x)} and whose names equal \code{names(x)}.
}

\author{
Antoine Filipovic Pierucci
}
\seealso{
\code{\link{class}}.
}
\examples{
tab <- data.frame(a = 1, b = "e", c = gl(1, 1), stringsAsFactors = FALSE)
colClasses(tab)
}
\keyword{misc }
