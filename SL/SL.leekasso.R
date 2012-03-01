SL.leekasso <- function (Y, X, newX, family, obsWeights, id, ...) 
{
	require("sva") # Bioconductor package, but really only need the f.pvalue function, might just replace it with internal function?
	N <- length(Y)
	mod <- cbind(rep.int(1, N), Y)
  mod0 <- cbind(rep.int(1, N))
  pValues <- f.pvalue(t(X), mod, mod0)
  index <- which(rank(pValues) <= 10) # always 10!

  lm1 <- lm(Y ~ ., data = X[, index])
  pred <- predict.lm(lm1, newdata = newX[, index])
  fit <- list(object = lm1, index = index)
  class(fit) <- c("SL.leekasso")
  out <- list(pred = pred, fit = fit)
  return(out)
}
