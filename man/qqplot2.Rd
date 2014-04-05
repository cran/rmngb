\name{qqplot2}
\alias{qqplot2}
\title{
Quantile-Quantile Plots using arbitrary distributions.
}
\description{
Produces a QQ plot of the values in \code{y} against the theoretical quantiles of an arbitrary distribution.
}
\usage{
qqplot2(y, fQuant = function(q, x) qnorm(q, mean(x, na.rm = TRUE), sd(x, na.rm = TRUE)),
line = TRUE, xlab = "Theoretical Quantiles", ...)
}
\arguments{
  \item{y}{
the data sample.
}
  \item{fQuant}{
a function taking 2 arguments. The first argument is a vector of quantiles, the second is the data sample. This function returns theoretical quantile values.
}
  \item{line}{
logical. Should a line representing perfect fit be plotted?
}
  \item{xlab}{
\code{xlab} value.
}
  \item{...}{
additional arguments passed to \code{qqplot}.
}
}
\value{
Nothing of interest.
}
\author{
Antoine Filipovic Pierucci
}
\seealso{
\code{\link{qqplot}}.
}
\examples{
y1 <- rnorm(500)
y2 <- rlnorm(500)

# good fit
qqplot2(y1)
# bad fit
qqplot2(y2)

# defining a new fQuant function
fQuant2 <- function(q, x)
    qlnorm(q, mean(log(x)), sd(log(x)))

# better fit
qqplot2(y2, fQuant2)
}
\keyword{ misc }
