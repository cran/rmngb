asInteger <- function(x) {
    as.integer(round(x))
}

invDiag <- function(x) {
    vec <- integer(x ^ 2)
    vec[cumsum(rep(x - 1, x)) + 1] <- 1
    matrix(vec, nrow = x)
}

"%out%" <- function(x, y) {
    ! x %in% y
}

nameToString <- function(x) {
    deparse(substitute(x))
}

rmAttr <- function(x, except = "class") {
    nAttr <- names(attributes(x))
    attributes(x) <- attributes(x)[except]
    x
}
