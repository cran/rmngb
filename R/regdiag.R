diagBinom <- function(model, type = 1:2) {
    stopifnot(model$family$family == "binomial")
    
    if (1 %in% type) {
        fittedValues <- fitted(model)
        y <- model$y
        decFittedValues <- cut(fittedValues,
                               breaks = quantile(fittedValues, seq(0, 1, .1)),
                               include.lowest = TRUE)
        observedValues <- tapply(y, decFittedValues, function(x) sum(x) / length(x))
        meanFittedValues <- tapply(fittedValues, decFittedValues, mean)
        plot(observedValues, meanFittedValues,
             xlim = 0:1, ylim = 0:1,
             main = "Observed probability\nvs. Fitted")
        abline(a = 0, b = 1, lty = 2)
    }
}
