\name{Vars}
\alias{colVars}
\alias{rowVars}
\title{
Form Row and Column Variances
}
\description{
Form row and column variances for numeric arrays.
}
\usage{
colVars(x, na.rm = FALSE, dims = 1, unbiased = TRUE,
        SumSquares = FALSE, twopass = TRUE)
rowVars(x, na.rm = FALSE, dims = 1, unbiased = TRUE,
        SumSquares = FALSE, twopass = TRUE)
}
\arguments{
  \item{x}{
an array of two or more dimensions, containing numeric, complex, integer or logical values, or a numeric data frame.
}
  \item{na.rm}{
logical. Should missing values (including NaN) be omitted from the calculations?
}
  \item{dims}{
integer: Which dimensions are regarded as ?rows? or ?columns? to sum over. For row*, the sum or mean is over dimensions dims+1, ...; for col* it is over dimensions 1:dims.
}
  \item{unbiased}{
logical. Should the estimator of the variance be unbiaised, using `(N - 1)` as a denominator?
}
  \item{SumSquares}{
logical. Should the function return only the sum of squares?
}
  \item{twopass}{
logical. `twopass = TRUE` is needed for numerical accuracy if mu >> sigma.
}
}
\details{
These functions are equivalent to use of apply with `FUN = var` with appropriate margins, but are a lot faster.
}
\value{
A numeric or complex array of suitable size, or a vector if the result is one-dimensional. For the first four functions the dimnames (or `names` for a vector result) are taken from the original array.
}
\references{
From David Brahm on R-Help, inspired from Package "colSums".
}
\author{
Antoine Filipovic Pierucci
}
\seealso{
\code{\link{colSums}}, \code{\link{apply}}.
}
\examples{
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
rowVars(x); colVars(x)
# more examples should be included for the other arguments
}
\keyword{ univar }
