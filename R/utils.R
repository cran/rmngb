asInteger <- function(x)
    as.integer(round(x))

invDiag <- function(x) {
    vec <- integer(x ^ 2)
    vec[cumsum(rep(x - 1, x)) + 1] <- 1
    matrix(vec, nrow = x)
}

"%out%" <- function(x, y)
    ! x %in% y

reverse <- function(x) {
    if (length(dim(x)) > 1)
        stop("Objects with more than 1 dimension are not suported.")
    if(l <- length(x)) x[l:1] else x
}

nameToString <- function(x)
    deparse(substitute(x))

rmAttr <- function(x, except = "class") {
    nAttr <- names(attributes(x))
    attributes(x) <- attributes(x)[except]
    x
}

Rnumber <- function()
    paste((v <- R.Version())$major, v$minor, sep = ".")

Rname <- function()
    R.Version()$nickname
