SL.nnetEntropy <- function (Y, X, newX, family, obsWeights, size = 2, decay = 0,...) {
  ## nnet using entropy (likelihood) instead of least squares, has same class as SL.nnet so no additional predict method
  require("nnet")
  if (family$family == "gaussian") {
      fit.nnet <- nnet::nnet(x = X, y = Y, size = size, entropy = TRUE, 
            trace = FALSE, maxit = 500, weights = obsWeights)
  }
  if (family$family == "binomial") {
      fit.nnet <- nnet::nnet(x = X, y = Y, size = size, entropy = TRUE, 
          maxit = 500, weights = obsWeights, trace = FALSE)
  }
  pred <- predict(fit.nnet, newdata = newX, type = "raw")
  fit <- list(object = fit.nnet)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.nnet")
  return(out)
}
