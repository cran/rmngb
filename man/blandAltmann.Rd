\name{blandAltman}
\alias{blandAltman}
\title{
Bland-Altman Plot
}
\description{
Produces a Bland-Altman plot.
}
\usage{
blandAltman(x, y,
             xlab = "Average", ylab = "Difference",
             main = "Bland-Altman plot", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
measurements returned by method 1.
}
  \item{y}{
measurements returned by method 2.
}
  \item{xlab}{
a title for the x axis: see \code{\link{title}}.
}
  \item{ylab}{
a title for the y axis: see \code{\link{title}}.
}
  \item{main}{
an overall title for the plot:  see \code{\link{title}}.
}
  \item{...}{
arguments to be passed to \code{\link{plot}}.
}
}
\details{
Plots the pairwise mean of \code{x} and \code{y} versus their pairwise difference. Adds horizontal lines at plus and minus 2 standard deviation of the pairwise difference.
}
\value{
Nothing of interest.
}
\author{
Antoine Filipovic Pierucci
}

\examples{
blandAltman(runif(20), runif(20))
}
\keyword{ ~hplot }
