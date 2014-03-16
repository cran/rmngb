\name{blandAltman}
\alias{blandAltman}
\alias{blandAltman.default}
\alias{blandAltman.formula}
\title{
Bland-Altman Plot
}
\description{
Produces a Bland-Altman plot.
}
\usage{
blandAltman(x, ...)
\method{blandAltman}{default}(x, y,
           xlab = "Mean", ylab = "Difference",
           main = "Bland-Altman plot",
           sdLines = 2, ...)
\method{blandAltman}{formula}(formula, data, subset, na.action, ...)
}
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
  \item{sdLines}{
how many standard deviations from the mean difference should the dotted lines be plotted.
}
  \item{formula}{
a formula of the form lhs ~ rhs where lhs is a numeric variable giving the data values and rhs a factor with two levels giving the corresponding methods.
}
  \item{data}{
an optional matrix or data frame (or similar: see \code{\link{model.frame}}) containing the variables in the formula formula. By default the variables are taken from \code{environment(formula)}.
}
  \item{subset}{
an optional vector specifying a subset of observations to be used.
}
  \item{na.action}{
a function which indicates what should happen when the data contain NAs. Defaults to \code{getOption("na.action")}.
}
  \item{...}{
arguments to be passed to \code{\link{plot}}.
}
}
\details{
Plots the pairwise mean of \code{x} and \code{y} versus their pairwise difference. Adds horizontal lines at plus and minus \code{sdLines} standard deviation of the pairwise difference.

When used with the \code{formula} method values should be carefully ordered before in order to produce meaningful results.
}
\value{
Nothing of interest.
}
\author{
Antoine Filipovic Pierucci
}

\examples{
# meaningless examples
blandAltman(extra ~ group, data = sleep)
blandAltman(runif(20), runif(20))
}
\keyword{ hplot }
