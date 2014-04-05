kde2dQuantile <- function(d, X, Y, probs = .05, ...) {
    xInd <- sapply(X, function(x) whichClosest1(x, d$x))
    yInd <- sapply(Y, function(x) whichClosest1(x, d$y))
    zValues <- d$z[cbind(xInd, yInd)]
    quantile(zValues, probs=probs, ...)
}
