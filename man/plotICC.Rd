\name{plotICC}
\alias{plotICC}
\title{
Visualization of Intra Class Correlation
}
\description{
Visually represents Intra Class Correlation.
}
\usage{
plotICC(x, subjects, p = FALSE, ...)
}
\arguments{
  \item{x}{
vector with the measurement of interest.
}
  \item{subjects}{
vector with a unique identifiant per subject.
}
  \item{p}{
logical. Should points be added for each unique measurement (useful if there is more than 2 measurements per subject).
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
\keyword{ ~misc }
