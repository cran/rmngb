\name{pairwise count tests}
\alias{pairwise.chisq.test}
\alias{pairwise.fisher.test}
\alias{pairwise.chisq.test.default}
\alias{pairwise.fisher.test.default}
\alias{pairwise.chisq.test.table}
\alias{pairwise.fisher.test.table}
\title{
Pairwise Chi-squared and Fisher Test
}
\description{
Calculate pairwise comparisons between group levels with corrections for multiple testing.
}
\usage{
pairwise.chisq.test(x, ...)
pairwise.fisher.test(x, ...)

\method{pairwise.chisq.test}{default}(x, g, p.adj = p.adjust.methods, ...)
\method{pairwise.chisq.test}{table}(x, p.adj = p.adjust.methods,
                     DNAME = NULL, ...)

\method{pairwise.fisher.test}{default}(x, g, p.adj = p.adjust.methods, ...)
\method{pairwise.fisher.test}{table}(x, p.adj = p.adjust.methods,
                     DNAME = NULL, ...)
}
\arguments{
  \item{x}{
a numeric vector or matrix. x and g can also both be factors.
}
  \item{g}{
a vector; ignored if x is a matrix.
}
  \item{p.adj}{
correction method among \code{holm}, \code{hochberg}, \code{hommel}, \code{bonferroni}, \code{BH}, \code{BY}, \code{fdr} and \code{none}.
}
  \item{DNAME}{
table dimension names.
}
  \item{...}{further arguments to be passed to or from methods.}
}
\details{
Calculate Chi-square or Fisher test p-values for all combinaisons of the levels of \code{g}.
}
\value{
Object of class \code{"pairwise.htest"}.
}
\references{
Original functions written by Dr Jean-Philippe Jais.
}
\author{
Antoine Filipovic Pierucci
}
\seealso{
\code{\link{pairwise.t.test}}, \code{\link{p.adjust}}.
}
\examples{
x <- sample(1:2, 1e3, TRUE)
g <- sample(1:4, 1e3, TRUE)
pairwise.chisq.test(x, g)
tab <- table(g, x)
pairwise.fisher.test(tab, p.adj = "bonf")
}
\keyword{ htest }
