SL.naiveBayes <- function (Y, X, newX, family, laplace = 0, ...) {
  require("e1071")
  if (family$family == "gaussian") {
      stop("SL.naiveBayes only available for family = binomial()")
  }
	fit.naiveBayes <- naiveBayes(Y ~ ., data = X, laplace = laplace)
  pred <- predict(object = fit.naiveBayes, newdata = newX, type = "raw")[, 2]
  fit <- list(object = fit.naiveBayes)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.naiveBayes")
  return(out)
}