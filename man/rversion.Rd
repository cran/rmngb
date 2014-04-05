\name{R version}
\alias{R version}
\alias{Rnumber}
\alias{Rname}
\title{
Version Information as a Character String
}
\description{
Provides information about the version of \code{R} running as a character string.
}
\usage{
Rnumber()
Rname()
}
\details{
Mostly useful for dynamically generated reports. \code{Rnumber} returns the version number, \code{Rname} returns the version nickmane.
}
\value{
A character string of length one.
}
\author{
Antoine Filipovic Pierucci
}
\seealso{
\code{\link{R.Version}}.
}
\examples{
Rnumber()
Rname()
}
\keyword{ misc }
