\name{plotICC}
\alias{plotICC}
\alias{plotICC.default}
\alias{plotICC.matrix}
\alias{plotICC.data.frame}
\title{
Visualization of Intra Class Correlation
}
\description{
Visually represents Intra Class Correlation.
}
\usage{
plotICC(x, ...)
\method{plotICC}{default}(x, subjects, p = FALSE, ptype = 4, ...)
\method{plotICC}{matrix}(x, p = FALSE, ptype = 4, ...)
\method{plotICC}{data.frame}(x, p = FALSE, ptype = 4, ...)
}
\arguments{
  \item{x}{
vector with the measurement of interest, or matrix with one line per individual and one column per measurement.
}
  \item{subjects}{
vector with a unique identifiant per subject, not used if \code{x} is a matrix.
}
  \item{p}{
logical. Should points be added for each unique measurement (useful if there is more than 2 measurements per subject)?
}
  \item{ptype}{
integer. Plotting symbol for the points, corresponds to \code{pch}. See \code{\link{points}}.
}
  \item{...}{
additional arguments passed to \code{plot}.
}
}
\details{
The range of values measured within a single subject are represented by a vertical line, so there is one line per subject.
For lisibility lines are sorted by the means of the measurements.
}
\value{Nothing of interest.
}
\author{
Antoine Filipovic Pierucci
}
\examples{
m1 <- rnorm(300)
subject <- rep(1:100, 3)
plotICC(m1, subject) # poor ICC

m2 <- m1 + subject / 5
plotICC(m2, subject) # better ICC
plotICC(m2, subject, TRUE) # with points
}
\keyword{ misc }
