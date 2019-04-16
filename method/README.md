
<!-- README.md is generated from README.Rmd. Please edit that file -->
Demonstrating the use of the 'method.stratifiedNNLS' Super Learner method
=========================================================================

> A new method to estimate the coefficients for the super learner and the model to combine the individual algorithms. A single algorithm in the library ('SL.stratify' in the example below) is used to data adaptively identify subgroups in the observed data. The other library algorithms are fit with the full training data, but the weights can vary by subgroup. So this differs from the [subsemble](https://github.com/ledell/subsemble) method in that an overall predictor is fit and the weights can change by subgroup, whereas subsemble estimates subset specific predictors.

**Authors:** Geoffrey Ecoto, [Antoine Chambaz](https://github.com/achambaz), [Eric Polley](https://github.com/ecpolley)

Installation
------------

Copy the [method.stratifiedNNLS.R](https://github.com/ecpolley/SuperLearnerExtra/method/method.stratifiedNNLS.R) file from the [SuperLearnerExtra](https://github.com/ecpolley/SuperLearnerExtra) repository. Source the file into `R`.

Example
-------

The code builds upon `?SuperLearner`. We first load the namespace of the package with name `SuperLearner` package, attach it on the search list, and source the file into `R`. Then, we simulate a training set and a test set.

``` r
library(SuperLearner)
source("https://raw.githubusercontent.com/ecpolley/SuperLearnerExtra/master/method/method.stratifiedNNLS.R")
set.seed(23432)

## training set
n <- 500
p <- 50
X <- matrix(rnorm(n*p), nrow = n, ncol = p)
colnames(X) <- paste("X", 1:p, sep="")
X <- data.frame(X)
Y <- X[, 1] + sqrt(abs(X[, 2] * X[, 3])) + X[, 2] - X[, 3] + rnorm(n)

## test set
m <- 1000
newX <- matrix(rnorm(m*p), nrow = m, ncol = p)
colnames(newX) <- paste("X", 1:p, sep = "")
newX <- data.frame(newX)
newY <- newX[, 1] + sqrt(abs(newX[, 2] * newX[, 3])) + newX[, 2] -
  newX[, 3] + rnorm(m)
```

We define a library of algorithms and run the super learner algorithm to obtain a meta-algorithm that performs almost as well as the best algorithm in the library. We rely on the `method.NNLS` method to determine the coefficients for the super learner and the model to combine the individual algorithms.

``` r
## generate library and run Super Learner
SL.library <- c("SL.glm", "SL.randomForest", "SL.gam",
                "SL.polymars", "SL.mean")
(test <- SuperLearner(Y = Y, X = X, newX = newX, SL.library = SL.library,
                      verbose = TRUE, method = "method.NNLS"))
#> Number of covariates in All is: 50
#> CV SL.glm_All
#> CV SL.randomForest_All
#> CV SL.gam_All
#> CV SL.polymars_All
#> CV SL.mean_All
#> Number of covariates in All is: 50
#> CV SL.glm_All
#> CV SL.randomForest_All
#> CV SL.gam_All
#> CV SL.polymars_All
#> CV SL.mean_All
#> Number of covariates in All is: 50
#> CV SL.glm_All
#> CV SL.randomForest_All
#> CV SL.gam_All
#> CV SL.polymars_All
#> CV SL.mean_All
#> Number of covariates in All is: 50
#> CV SL.glm_All
#> CV SL.randomForest_All
#> CV SL.gam_All
#> CV SL.polymars_All
#> CV SL.mean_All
#> Number of covariates in All is: 50
#> CV SL.glm_All
#> CV SL.randomForest_All
#> CV SL.gam_All
#> CV SL.polymars_All
#> CV SL.mean_All
#> Number of covariates in All is: 50
#> CV SL.glm_All
#> CV SL.randomForest_All
#> CV SL.gam_All
#> CV SL.polymars_All
#> CV SL.mean_All
#> Number of covariates in All is: 50
#> CV SL.glm_All
#> CV SL.randomForest_All
#> CV SL.gam_All
#> CV SL.polymars_All
#> CV SL.mean_All
#> Number of covariates in All is: 50
#> CV SL.glm_All
#> CV SL.randomForest_All
#> CV SL.gam_All
#> CV SL.polymars_All
#> CV SL.mean_All
#> Number of covariates in All is: 50
#> CV SL.glm_All
#> CV SL.randomForest_All
#> CV SL.gam_All
#> CV SL.polymars_All
#> CV SL.mean_All
#> Number of covariates in All is: 50
#> CV SL.glm_All
#> CV SL.randomForest_All
#> CV SL.gam_All
#> CV SL.polymars_All
#> CV SL.mean_All
#> Non-Negative least squares convergence: TRUE
#> full SL.glm_All
#> full SL.randomForest_All
#> full SL.gam_All
#> full SL.polymars_All
#> full SL.mean_All
#> 
#> Call:  
#> SuperLearner(Y = Y, X = X, newX = newX, SL.library = SL.library, method = "method.NNLS",  
#>     verbose = TRUE) 
#> 
#> 
#>                         Risk       Coef
#> SL.glm_All          1.419563 0.02435625
#> SL.randomForest_All 1.630625 0.41282926
#> SL.gam_All          1.357148 0.33530884
#> SL.polymars_All     1.407887 0.22750565
#> SL.mean_All         4.371433 0.00000000
```

We would like that the coefficients for the super learner can differ by subgroup. The subgroups are characterized by a stratification algorithm that is encoded just like any other algorithm in the library, see the `functions` `SL.stratify` and `predict.SL.stratify` below (and note that both **must** output `integer`s). Then, the new `method` is obtained through a call to the `method.stratifiedNNLS` `function`.

``` r
## CAUTION: 'SL.stratify' and 'predict.SL.stratify' MUST output integers

SL.stratify <- function(Y, X, newX, family, obsWeights, id, ...) {
  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }
  X <- sign(X[, 2] * X[, 3])
  
  if (!is.matrix(newX)) {
    newX <- as.matrix(newX)
  }
  pred <- as.integer(sign(newX[, 2] * newX[, 3]))
  fit <- list(object = X)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- "SL.statify"
  return(out)
}
environment(SL.stratify) <- asNamespace("SuperLearner")


predict.SL.stratify <- function (object, newdata, family, X = NULL, Y = NULL, ...) {
  if (!is.matrix(newdata)) {
    newdata <- as.matrix(newdata)
  }
  pred <- as.integer(sign(newdata[, 2] * newdata[, 3]))
  return(pred)
}
environment(predict.SL.stratify) <- asNamespace("SuperLearner")

method.stratif.NNLS <- method.stratifiedNNLS("SL.stratify")
environment(method.stratif.NNLS) <- asNamespace("SuperLearner")
```

To compute the new super learner, it suffices to append the stratification algorithm to the library previously defined, and to request the use of the `method.stratif.NNLS` method.

``` r

(test.stratified <- SuperLearner(Y = Y, X = X, newX = newX,
                                 SL.library = c(SL.library, "SL.stratify"),
                                 verbose = TRUE,
                                 method = method.stratif.NNLS))
#> Number of covariates in All is: 50
#> CV SL.glm_All
#> CV SL.randomForest_All
#> CV SL.gam_All
#> CV SL.polymars_All
#> CV SL.mean_All
#> CV SL.stratify_All
#> Number of covariates in All is: 50
#> CV SL.glm_All
#> CV SL.randomForest_All
#> CV SL.gam_All
#> CV SL.polymars_All
#> CV SL.mean_All
#> CV SL.stratify_All
#> Number of covariates in All is: 50
#> CV SL.glm_All
#> CV SL.randomForest_All
#> CV SL.gam_All
#> CV SL.polymars_All
#> CV SL.mean_All
#> CV SL.stratify_All
#> Number of covariates in All is: 50
#> CV SL.glm_All
#> CV SL.randomForest_All
#> CV SL.gam_All
#> CV SL.polymars_All
#> CV SL.mean_All
#> CV SL.stratify_All
#> Number of covariates in All is: 50
#> CV SL.glm_All
#> CV SL.randomForest_All
#> CV SL.gam_All
#> CV SL.polymars_All
#> CV SL.mean_All
#> CV SL.stratify_All
#> Number of covariates in All is: 50
#> CV SL.glm_All
#> CV SL.randomForest_All
#> CV SL.gam_All
#> CV SL.polymars_All
#> CV SL.mean_All
#> CV SL.stratify_All
#> Number of covariates in All is: 50
#> CV SL.glm_All
#> CV SL.randomForest_All
#> CV SL.gam_All
#> CV SL.polymars_All
#> CV SL.mean_All
#> CV SL.stratify_All
#> Number of covariates in All is: 50
#> CV SL.glm_All
#> CV SL.randomForest_All
#> CV SL.gam_All
#> CV SL.polymars_All
#> CV SL.mean_All
#> CV SL.stratify_All
#> Number of covariates in All is: 50
#> CV SL.glm_All
#> CV SL.randomForest_All
#> CV SL.gam_All
#> CV SL.polymars_All
#> CV SL.mean_All
#> CV SL.stratify_All
#> Number of covariates in All is: 50
#> CV SL.glm_All
#> CV SL.randomForest_All
#> CV SL.gam_All
#> CV SL.polymars_All
#> CV SL.mean_All
#> CV SL.stratify_All
#> Stratified non-negative least squares convergence: TRUE
#> Stratified non-negative least squares convergence: TRUE
#> full SL.glm_All
#> full SL.randomForest_All
#> full SL.gam_All
#> full SL.polymars_All
#> full SL.mean_All
#> full SL.stratify_All
#> 
#> Call:  
#> SuperLearner(Y = Y, X = X, newX = newX, SL.library = c(SL.library, "SL.stratify"),  
#>     method = method.stratif.NNLS, verbose = TRUE) 
#> 
#> 
#>                         Risk        -1          1
#> SL.glm_All          1.421432 0.0000000 0.03339491
#> SL.randomForest_All 1.672711 0.1822895 0.47575726
#> SL.gam_All          1.350152 0.3802559 0.18656215
#> SL.polymars_All     1.347726 0.4374546 0.30428567
#> SL.mean_All         4.361759 0.0000000 0.00000000
```

The stratum-specific weights differ starkly.
