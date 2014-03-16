\name{rmAttr}
\alias{rmAttr}
\title{
Remove Attributes
}
\description{
Remove all the attributes of an object.
}
\usage{
rmAttr(x, except = "class")
}
\arguments{
  \item{x}{
an object whose attributes should be removed.
}
  \item{except}{
character vector. Attributes that should not be removed. \code{NULL} to remove all attributes.
}
}
\value{
\code{x} with its attibutes removed.
}
\author{
Antoine Filipovic Pierucci
}
\seealso{
\code{\link{attributes}}.
}
\examples{
x <- table(sample(letters, 1e3, TRUE))
(x1 <- rmAttr(x))
class(x1 <- rmAttr(x)) # still a table, without names
(x2 <- rmAttr(x, NULL))
class(x2) # integer vector
}
\keyword{ misc }
