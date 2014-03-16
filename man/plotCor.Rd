\name{plotCor}
\alias{plotCor}
\title{
Plot a Correlation Matrix
}
\description{
Plot a correlation matrix with scatterplots, correlation coefficients and colors.
}
\usage{
plotCor(tab,
        method = c("pearson", "kendall", "spearman"),
        formatCor, colCor, ...)
}
\arguments{
  \item{tab}{
a dataframe. Contain the variable whose correlation is of interest.
}
  \item{method}{
among \code{pearson}, \code{kendall} and \code{spearman}. Method used to compute the correlation coefficient.
}
  \item{formatCor}{
a function. Applied to a \code{htest} object and returning a character string, formats the display of the correlation coefficient.
}
  \item{colCor}{
a function. Applied to a \code{htest} object and returning a color (character string or \code{\link{rgb}}).
}
  \item{...}{
additional arguments passed to \code{formatCor}.
}
}
\details{
Displays a matrix plot where the diagonal contains the variable names,
the lower triangle displays scatterplots between the variables and the
upper triangle correlation coefficients with colors corresponding the the magnitude of the correlation.
}
\value{
Nothing of interest.
}
\author{
Antoine Filipovic Pierucci
}
\seealso{
\code{\link{cor.test}}.
}
\examples{
x <- iris[, 1:4]
plotCor(x)

plotCor(x, formatCor = function(x) round(x$estimate, 2))
plotCor(x, colCor = function(x) if (x$estimate > 0) "green" else "yellow")

}
\keyword{ misc }
