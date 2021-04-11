library('randomForest')

f <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
## drop 2
rfvar(f, iris, -2)
## top 2 (same as drop 2)
rfvar(f, iris, 2)

rfvar <- function(formula, data, nvar = -1L, verbose = FALSE, plot = verbose, ...) {
  tt <- rownames(attr(terms(formula), 'factors'))
  yy <- tt[1L]
  xx <- tt[-1L]
  
  co <- complete.cases(data[, all.vars(formula)])
  if (any(!co)) {
    message(sum(!co), ' observations removed due to missingness')
    data <- data[co, ]
  }
  
  if (nvar < 0)
    nvar <- pmax(1L, length(xx) + nvar)
  
  if (nvar == 0 || length(xx) == nvar)
    return(formula)
  
  rf <- randomForest::randomForest(formula, data = data, importance = TRUE)
  rf$call$formula <- formula
  
  if (verbose) {
    print(rf$call)
  }
  if (plot) {
    randomForest::varImpPlot(rf, main = '')
  }
  
  ## remove least relevant variable
  ri <- rf$importance[, 'MeanDecreaseAccuracy', drop = FALSE]
  ex <- names(which.min(ri))
  formula <- reformulate(setdiff(xx, ex), yy)
  
  Recall(formula, data, nvar, verbose, plot, ...)
}
