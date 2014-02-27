\name{qqnorm2}
\alias{qqnorm2}
\title{
Variation on a Quantile-Quantile Plot
}
\description{
Produces a normal QQ plot of the values with a reference line.
}
\usage{
qqnorm2(var, lty = 2, lwd = 1, col = "black", ...)
}
\arguments{
  \item{var}{
vector of values to be plotted.
}
  \item{lty}{
line type.
}
  \item{lwd}{
line width.
}
  \item{col}{
line colour.
}
  \item{...}{
additional arguments passed to \code{qqnorm}.
}
}
\details{
Like \code{\link{qqnorm}} with a line.
}
\value{
Nothing of interest.
}
\author{
Antoine Filipovic Pierucci
}

\seealso{
\code{\link{qqnorm}}.
}
\examples{
qqnorm(rlnorm(100))
}
\keyword{ ~misc }
