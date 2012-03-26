SL.blackboost <- function(Y, X, newX, family, obsWeights, mstop = 500, ...) 
{
    require("mboost")
    if (family$family == "gaussian") {
        fit.gbm <- blackboost(Y ~ ., data = X, family = Gaussian(), control = boost_control(mstop = mstop, nu = 0.1, risk = "inbag"), tree_control = ctree_control(teststat = "max", testtype = "Teststatistic", mincriterion = 0, maxdepth = 2))
    }
    if (family$family == "binomial") {
        fit.gbm <- blackboost(as.factor(Y) ~ ., data = X, family = Binomial(), control = boost_control(mstop = mstop, nu = 0.1, risk = "inbag"), tree_control = ctree_control(teststat = "max", testtype = "Teststatistic", mincriterion = 0, maxdepth = 2))
    }
    pred <- predict(fit.gbm, newdata = newX, type = "response")
    fit <- list(object = fit.gbm)
    out <- list(pred = pred, fit = fit)
    class(out$fit) <- c("SL.blackboost")
    return(out)
}