## Wrapper function to use iml package with a SuperLearner predictor
create_predict_fun.SuperLearner <- function(model, task = "regression", predict.fun = NULL, type = NULL) {
  function(newdata) {
    pred <- SuperLearner::predict.SuperLearner(model, newdata = newdata, onlySL = TRUE)
    out <- as.data.frame(pred$pred[, 1, drop = FALSE])  # only want the SL predictions, and as a vector
    return(out)
  }
}


## example usage
library(SuperLearner)
library(iml)

set.seed(23432)
## training set
n <- 500
p <- 10
X <- matrix(rnorm(n*p), nrow = n, ncol = p)
colnames(X) <- paste("X", 1:p, sep="")
X <- data.frame(X)
Y <- X[, 1] + sqrt(abs(X[, 2] * X[, 3])) + X[, 2] - X[, 3] + rnorm(n)

## test set
m <- 1000
newX <- matrix(rnorm(m*p), nrow = m, ncol = p)
colnames(newX) <- paste("X", 1:p, sep="")
newX <- data.frame(newX)
newY <- newX[, 1] + sqrt(abs(newX[, 2] * newX[, 3])) + newX[, 2] -
  newX[, 3] + rnorm(m)

# generate Library and run Super Learner
SL.library <- c("SL.glm", "SL.randomForest", "SL.glmnet", "SL.polymars", "SL.ranger", "SL.mean")
test <- SuperLearner(Y = Y, X = X, newX = newX, SL.library = SL.library, method = "method.NNLS")
test

create_predict_fun.SuperLearner <- function(model, task = "regression", predict.fun = NULL, type = NULL) {
		function(newdata) {
			pred <- SuperLearner::predict.SuperLearner(model, newdata = newdata, onlySL = TRUE)
			out <- as.data.frame(pred$pred[, 1, drop = FALSE])  # only want the SL predictions, and as a vector
			return(out)
		}
}


predictor <- Predictor$new(model = test,
							 data = X, 
							 y = Y, 
							 predict.function = create_predict_fun.SuperLearner)
							 
imp <- FeatureImp$new(predictor, loss = "mse")
imp
plot(imp)
effect = FeatureEffects$new(predictor)
effect$plot()




# randomForests from the ensemble
predictor <- Predictor$new(test$fitLibrary$SL.randomForest_All$object, data = X, y = Y)
imp <- FeatureImp$new(predictor, loss = "mae")
plot(imp)
imp$results
effect = FeatureEffects$new(predictor)
effect$plot()  # accumulated local effects

# surrogate model
tree <- TreeSurrogate$new(predictor, maxdepth = 3)
plot(tree)


### example with binary outcome
# binary outcome
set.seed(1)
N <- 500
X <- matrix(rnorm(N*10), N, 10)
X <- as.data.frame(X)
Y <- rbinom(N, 1, plogis(.2*X[, 1] + .1*X[, 2] - .2*X[, 3] + .1*X[, 3]*X[, 4] - .2*abs(X[, 4])))

SL.library <- c("SL.glmnet", "SL.glm", "SL.knn", "SL.gam", "SL.randomForest", "SL.mean")

# least squares loss function
fit <- SuperLearner(Y = Y, X = X, SL.library = SL.library, method = "method.NNLS", family = binomial())
fit


predictor <- Predictor$new(model = fit,
							 data = X, 
							 y = Y, 
							 predict.function = create_predict_fun.SuperLearner)
							 
imp <- FeatureImp$new(predictor, loss = "logLoss")
imp
plot(imp)

# compare with glm coef estimates
summary(fit$fitLibrary$SL.glm_All$object)
