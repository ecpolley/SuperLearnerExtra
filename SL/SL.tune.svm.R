# SVM with internal tuning
SL.tune.svm <- function (Y, X, newX, family, type.reg = "nu-regression", type.class = "nu-classification", 
    kernel = "radial", nu = 0.5, degree = 3, cost = 2^(2:8),...) 
{
    require("e1071")
    if (family$family == "gaussian") {
        fit.svm <- e1071::tune.svm(y = Y, x = X, nu = nu, type = type.reg, 
            fitted = FALSE, kernel = kernel, degree = degree, cost = cost)$best.model
        pred <- predict(fit.svm, newdata = newX)
        fit <- list(object = fit.svm)
    }
    if (family$family == "binomial") {
        fit.svm <- e1071::tune.svm(y = as.factor(Y), x = X, nu = nu, 
            type = type.class, fitted = FALSE, probability = TRUE, 
            kernel = kernel, degree = degree, cost = cost)$best.model
        pred <- attr(predict(fit.svm, newdata = newX, probability = TRUE), 
            "prob")[, "1"]
        fit <- list(object = fit.svm)
    }
    out <- list(pred = pred, fit = fit)
    class(out$fit) <- c("SL.svm")
    return(out)
}
