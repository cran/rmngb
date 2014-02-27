\name{pairwise.chisq.test}
\alias{pairwise.chisq.test}
\alias{pairwise.fisher.test}
\title{
Pairwise Chi-squared and Fisher Test
}
\description{
Calculate pairwise comparisons between group levels with corrections for multiple testing.
}
\usage{
pairwise.chisq.test(var, class,
                    dec = 3, p.adj = p.adjust.methods)
pairwise.fisher.test(var, class,
                     dec = 3, p.adj = p.adjust.methods)
}
\arguments{
  \item{var}{
response vector.
}
  \item{class}{
grouping vector or factor.
}
  \item{dec}{
decimal rounding.
}
  \item{p.adj}{
p adjust method chosen among \code{holm}, \code{hochberg}, \code{hommel}, \code{bonferroni}, \code{BH}, \code{BY}, \code{fdr} and \code{none}.
}
}
\details{
Work in progress, in the futur this function will be more like \code{\link{pairwise.t.test}}.
}
\value{
Nothing of interest so far. This ought to be fixed by someone...
}
\references{
Original functions written by Dr Jean-Philipe Jais.
}
\author{
Antoine Filipovic Pierucci
}

\section{Warning }{
Don't expect the return values or the argument to stay the same in future versions of the package.
}
\seealso{
\code{\link{pairwise.t.test}}, \code{\link{p.adjust}}.
}
\examples{
2 + 2 == 4
}
\keyword{ ~htest }
