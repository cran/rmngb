\name{diagBinom}
\alias{diagBinom}
\title{
Plot Diagnostics for Logistic Regressions
}
\description{
Two plots are available : a plot of predicted against observed values, and an ROC curve assessing the classifying capacity of the model.
}
\usage{
diagBinom(model, type = 1:2)
}
\arguments{
  \item{model}{
a \code{glm} with a \code{binomial} \code{family}.
}
  \item{type}{
the plots wanted, with \code{1} for predicted against observed and \code{2} for an ROC curve.
}
}
\details{
More diagnostic plots may be added in the future if I feel like it. The number of categories for plot 1 should be adaptative.

No ROC anymore because pROC is too heavy to load.
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
model1 <- glm(cbind(ncases, ncontrols) ~ agegp + tobgp * alcgp,
              data = esoph, family = binomial())
diagBinom(model1)
}
\keyword{ misc }
