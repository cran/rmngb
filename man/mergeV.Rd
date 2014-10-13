\name{mergeV}
\alias{mergeV}
\title{
Verbose Merge
}
\description{
A verbose wrapper for the \code{merge} function from the \code{base} package.
}
\usage{
mergeV(x, y, 
       by = intersect(names(x), names(y)),
       by.x = by, by.y = by,
       all = FALSE, all.x = all, all.y = all,
       verbose = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{data frames, or objects to be coerced to one.
}
  \item{by, by.x, by.y}{specifications of the columns used for merging.
}
  \item{all}{logical; \code{all} = L is shorthand for \code{all.x} = L and \code{all.y} = L, where L is either TRUE or FALSE.
}
  \item{all.x}{logical; if TRUE, then extra rows will be added to the output, one for each row in x that has no matching row in y. These rows will have NAs in those columns that are usually filled with values from y. The default is FALSE, so that only rows with data from both x and y are included in the output.
}
  \item{all.y}{logical; analogous to \code{all.x}.
}
  \item{verbose}{prints information about the merge. See 'Details'.
}
  \item{...}{additional arguments passed to \code{\link{merge}}.
}
}
\details{
This function is just a wrapper for \code{\link{merge}}, the behavior and the return value are the same.

Calculation of the printed information is not computationaly free; \code{mergeV} takes significantly longer than the non verbose version to compute. Cross joins have almost no overhead, and inner joins have less overhead than the other types of junctions.

When \code{verbose} is FALSE the computational overhead is removed.

The printed information is composed of 3 parts. The first part represents the number of lines from each entry tables X and Y who have matches, and the number of resulting lines in the output R. The second part gives the type of join (inner, outer, left, right and cross). The last part is only there if there is a \code{by} variable, and gives the number of nXn matches on the by variables.
}
\value{
Same value as \code{merge}.
}
\author{
Antoine Filipovic Pierucci
}
\seealso{
\code{\link{merge}}.
}
\examples{
# classical merge example
authors <- data.frame(
    surname = I(c("Tukey", "Venables", "Tierney", "Ripley", "McNeil")),
    nationality = c("US", "Australia", "US", "UK", "Australia"),
    deceased = c("yes", rep("no", 4)))
books <- data.frame(
    name = I(c("Tukey", "Venables", "Tierney",
             "Ripley", "Ripley", "McNeil", "R Core")),
    title = c("Exploratory Data Analysis",
              "Modern Applied Statistics ...",
              "LISP-STAT",
              "Spatial Statistics", "Stochastic Simulation",
              "Interactive Data Analysis",
              "An Introduction to R"),
    other.author = c(NA, "Ripley", NA, NA, NA, NA,
                     "Venables & Smith"))

mergeV(authors, books, by.x = "surname", by.y = "name")
mergeV(authors, books, by.x = "surname", by.y = "name", all = TRUE)

}
\keyword{ misc }
