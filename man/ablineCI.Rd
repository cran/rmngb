\name{ablineCI}
\alias{ablineCI}
\title{
Add Confidence Interval Lines to a Plot
}
\description{
This function adds confidence interval lines from a model to the current plot.
}
\usage{
ablineCI(x, level = .95, lty = 2, ...)
}
\arguments{
  \item{x}{
an object inheriting from \code{lm}.
}
  \item{level}{
confidence level.
}
  \item{lty}{
the line type.
}
  \item{...}{
additional arguments passed to \code{\link{lines}}
}
}
\value{
A matrix of predictions.
}
\author{
Antoine Filipovic Pierucci
}

\examples{
x <- rnorm(10)
y <- rnorm(10)
mod <- lm(y ~ x)

plot(x, y)
abline(mod)
ablineCI(mod)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }