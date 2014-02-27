blandAltman <- function(x, y,
                         xlab = "Average", ylab = "Difference",
                         main = "Bland-Altman plot", ...) {
    xMean <- (x + y) / 2
    yDiff <- x - y
    ylim.min <- min(mean(yDiff, na.rm = TRUE) - 2.1 * sd(yDiff, na.rm = TRUE),
                    min(yDiff, na.rm = TRUE), na.rm = TRUE)
    ylim.max <- max(mean(yDiff, na.rm = TRUE) + 2.1 * sd(yDiff, na.rm = TRUE),
                    max(yDiff,na.rm = TRUE), na.rm = TRUE)
    plot(xMean, yDiff, xlab = xlab, ylab = ylab,
          main = main,
         ylim = c(ylim.min, ylim.max))
    abline(h = mean(yDiff, na.rm = T) - c(-2, 0, 2) * sd(yDiff, na.rm = T),
           lty = c(3, 2, 3))
}

plotCor <- function(tab,
                    method = c("pearson", "kendall", "spearman"),
                    formatCor, colCor, ...) {
    # add par() restoration in case of crash
    if (missing(formatCor)) {
        formatCor <- function(test, n = 3, ...) {
            if (is.null(test$conf.int)) {
                paste(round(test$estimate, n),
                      if (test$p.value < .001) "p < 0.001" else sprintf("p = %.03f", test$p.value),
                      sep = "\n")
            } else {
                paste(round(test$estimate, n),
                      sprintf(sprintf("[%%.%if ; %%.%if]", n, n),
                              test$conf.int[1],
                              test$conf.int[2]),
                      if (test$p.value < .001) "p < 0.001" else sprintf("p = %.03f", test$p.value),
                      sep = "\n")
            }
        }
    }
    
    if (missing(colCor)) {
        colCor <- function(test) {
            rgb(red = (r <- test$estimate > 0),
                green = 0,
                blue = ! r,
                alpha = abs(test$estimate))
        }
    }
    
    nc <- ncol(tab)
    stopifnot(nc > 1)
    method <- match.arg(method)
    op <- par(mfrow = c(nc, nc),
              mai = c(0, 0, 0, 0),
              xaxt = "n",
              yaxt = "n")
    
    for (i in 1:nc) {
        for (j in 1:nc) {
            if (i == j) {
                plot.new()
                text(.5, .5, names(tab)[i])
            } else {
                if (j > i) {
                    test <- cor.test(tab[, j],
                                     tab[, i],
                                     method = method)
                    test.text <- formatCor(test, ...)
                    plot.new()
                    rect(0, 0, 1, 1, col = colCor(test),
                         border = NA)
                    text(.5, .5, test.text)
                } else {
                    plot(tab[, j], tab[, i])
                }
            }
        }
    }
    
    par(op)
}

plotDensity <- function(x, col = "red", lwd = 2, ...) {
    d <- density(na.omit(x))
    h <- hist(x, plot = FALSE)
    plot(h, xlim = c(min(d$x), max(d$x)),
         ylim = c(0, max(c(d$y, h$density))),
         freq = FALSE, ...)
    lines(d, lwd = lwd)
}

plotICC <- function(x, subjects, p = FALSE, ...) {
    lMax <- max(tabS <- table(subjects)) + 1
    id <- names(tabS)
    FunC <- function(a, lMax) {
        c(a, rep(NA, lMax - length(a)))
    }
    listY <- lapply(tapply(x, subjects, c), FunC, lMax = lMax)
    matY <- matrix(unlist(listY), nrow = lMax)
    matY <- matY[ , order(colMeans(matY, na.rm = TRUE))]
    matX <- matrix(rep(seq_along(id), each = lMax), nrow = lMax)
    plot(matX, matY, type = "l", ...)
    if (p) points(matX, matY, pch = 4)
}

plotScatter <- function(x, y, lty = 1, lwd = 2, colLine = "red", ...) {
    m <- na.omit(cbind(x, y))
    plot(m, ...)
    lines(lowess(m), col = colLine, lwd = lwd, lty = lty)
}

qqnorm2 <- function(var, lty = 2, lwd = 1, col = "black", ...) {
    qqnorm(var, ...)
    abline(a = mean(var, na.rm = TRUE), b = sd(var, na.rm = TRUE), lty = lty, lwd = lwd, col = col)
}
