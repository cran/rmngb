\name{nameToString}
\alias{nameToString}
\title{
Convert Object Name to Character String
}
\description{
Converts an object name or an expression to a string.
}
\usage{
nameToString(x)
}
\arguments{
  \item{x}{
any object or an expression
}
}
\details{
Useful for plot labelling.
}
\value{
A character string of length 1.
}
\author{
Antoine Filipovic Pierucci
}
\examples{
foo <- 1:10
nameToString(foo) # "foo"
}
\keyword{ misc }
