\name{Stack}
\alias{createStack}
\alias{pop}
\alias{push}
\alias{print.stack}
\alias{as.list.stack}
\alias{length.stack}
\title{
Stack Operations
}
\description{
Methods to create and interact with stacks.
}
\usage{
createStack(...)
push(stack, x)
pop(stack)
\method{print}{stack}(x, ...)
\method{as.list}{stack}(x, ...)
\method{length}{stack}(x)
}
\arguments{
  \item{...}{
one or more objects to be put into a stack.
}
  \item{stack}{
a stack object as created by \code{createStack}.
}
  \item{x}{
an object to \code{push} on a stack.
}
}
\details{
The objects in \code{...} will be ordered in the stack in the order they appear, with the first at the top.
}
\value{
For \code{createStack} an object of class \code{stack}. No value for \code{push}, the first object in the stack for \code{pop}.
}
\author{
Antoine Filipovic Pierucci
}
\seealso{
\code{\link{list}}.
}
\examples{
stack <- createStack(rnorm(2), "some text")
stack
length(stack)
push(stack, 2)
stack
pop(stack)
stack
}
\keyword{ manip }
