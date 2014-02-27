pairwise.chisq.test <- function(var, class,
                                dec = 3, p.adj = p.adjust.methods) {
    s.title <- paste(deparse(substitute(var)), "and", deparse(substitute(class)))
    p.adj <- match.arg(p.adj)
    x.tab <- table(class, var)
    k <- dim(x.tab)[1]
    p.values <- rep(NA, k ^ 2)
    for (row1 in 1:(k - 1)) {
        for (row2 in (row1 + 1):k) {
            xi <- asInteger(k * (row1 - 1) + row2)
            p.values[xi] <- chisq.test(x.tab[c(row1, row2), ])$p.value
        }
    }
    p.values <- p.adjust(p.values, method = p.adj)
    dn <- list(dimnames(x.tab)[[1]],
               dimnames(x.tab)[[1]])
    mat.p.values <- matrix(p.values, nrow = k, dimnames = dn)
    cat(s.title, "\n\n")
    print(mat.p.values, na.print = "-", digits = dec)
    cat("\nP value adjustment method:", p.adj,"\n\n")
}
pairwise.fisher.test <- function(var, class,
                                 dec = 3, p.adj = p.adjust.methods) {
    s.title <- paste(deparse(substitute(var)), "and", deparse(substitute(class)))
    p.adj <- match.arg(p.adj)
    x.tab <- table(class, var)
    k <- dim(x.tab)[1]
    p.values <- rep(NA, k ^ 2)
    for (row1 in 1:(k - 1)) {
        for (row2 in (row1 + 1):k) {
            xi <- asInteger(k * (row1 - 1) + row2)
            p.values[xi] <- fisher.test(x.tab[c(row1, row2), ])$p.value
        }
    }
    p.values <- p.adjust(p.values, method = p.adj)
    dn <- list(dimnames(x.tab)[[1]],
               dimnames(x.tab)[[1]])
    mat.p.values <- matrix(p.values, nrow = k, dimnames = dn)
    cat(s.title, "\n\n")
    print(mat.p.values, na.print = "-", digits = dec)
    cat("\nP value adjustment method:", p.adj,"\n\n")
}