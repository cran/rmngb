\name{counter}
\alias{makeCounter}
\alias{increment}
\alias{decrement}

\title{
Create and manipulate counters
}
\description{
Functions to create and manipulate counters.
}
\usage{
makeCounter(start)
increment(x, step = 1)
decrement(x, step = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{start}{
counter starting value.
}
  \item{x}{
counter stored value.
}
  \item{step}{
value to increment/decrement from the counter stored value.
}
}
\details{
A counter is a function that takes another function as an argument. When no function is supplied, the counter just returns its stored value.

The increment and decrement functions are provided because they are the most likely to be of any use, but any function may be used.

Copied counters created from the same original countre share the same stored value. This may be used to share states between objects.
}
\value{
A counter returns its value when called without any argument, or the return value of the supplied function applied to the stored value.
}
\author{
Antoine Filipovic Pierucci
}
\examples{
# counter creation
counter <- makeCounter()

# return stored value
counter()

# incrementing/decrementing
counter(increment)
counter(increment, 5)
counter(decrement, 2)

# arbitrary function
counter(function(x) x * x)

# shared memory
counter2 <- counter

counter()
counter2(increment)

counter()

# starting value may be anything
counter3 <- makeCounter(start = "a")
counter3()

counter3(function(x) c(x, "b"))
}
\keyword{ ~misc }
