
library('randomForestSRC')

f <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
## drop 2
rfvar(f, iris, -2)
## top 2 (same as drop 2)
rfvar(f, iris, 2)

f <- Surv(time, status == 0) ~ rx + sex + age + obstruct + perfor + adhere + nodes
## drop 2
rfvar(f, colon, -2, ntree = 10)
## top 5 (same as drop 2)
rfvar(f, colon, 5, ntree = 10)

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
  
  rf <- randomForestSRC::rfsrc(formula, data = data, importance = TRUE, ...)
  rf$call$formula <- formula
  
  if (verbose) {
    print(rf$call)
  }
  if (plot) {
    randomForest::varImpPlot(rf, main = '')
  }
  
  ## remove least relevant variable
  ri <- rf$importance
  if (length(dim(ri)))
    ri <- ri[, 'all']
  ex <- names(which.min(ri))
  formula <- reformulate(setdiff(xx, ex), yy)
  
  Recall(formula, data, nvar, verbose, plot, ...)
}
