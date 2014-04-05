\name{diagBinom}
\alias{diagBinom}
\title{
Plot Diagnostics for Logistic Regressions
}
\description{
Two plots are available: a plot of predicted against observed values, and an ROC curve assessing the classifying capacity of the model.
}
\usage{
diagBinom(mod, type = 1:2,
ask = prod(par("mfcol")) < length(type) && dev.interactive())
}
\arguments{
  \item{mod}{
a \code{glm} with a \code{binomial} \code{family}.
}
  \item{type}{
the plots wanted, with \code{1} for predicted against observed and \code{2} for an ROC curve.
}
    \item{ask}{
    logical, if \code{TRUE} the user is asked before continuing to a new plot.
    }
}
\details{
More plots may be available in the future.
}
\value{
Nothing of interest.
}
\references{
Inspired by the book by Hosmer and Lemeshow.
}
\author{
Antoine Filipovic Pierucci
}
\seealso{
\code{\link{plot.lm}}.
}
\examples{
require(stats)
model1 <- glm(case ~ spontaneous+induced, data = infert, family = binomial())
diagBinom(model1)
}
\keyword{ misc }
