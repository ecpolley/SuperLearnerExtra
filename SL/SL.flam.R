## wrappers for flam
SL.flam <- function (Y, X, newX, family, obsWeights, id, useMin = TRUE, alpha = 0.75, n.fold = 10, ...) 
{
	require("flam")
    if (!is.matrix(X)) {
        X <- model.matrix(~ -1 + ., X)
        newX <- model.matrix(~ -1 + ., newX)
    }
	fitcv <- flam::flamCV(x = X, y = Y, family = family$family, alpha = alpha, n.fold = n.fold, within1SE = !useMin)

    pred <- predict(fitcv$flam.out, new.x = newX, lambda = fitcv$lambda.cv, alpha = alpha)
    fit <- list(object = fitcv, useMin = useMin)
    class(fit) <- c("SL.flam")
    out <- list(pred = pred, fit = fit)
    return(out)
}

predict.SL.flam <- function (object, newdata, ...) 
{
    require("flam")
    if (!is.matrix(newdata)) {
        newdata <- model.matrix(~-1 + ., newdata)
    }
    pred <- predict(object$object, newx = newdata, lambda = object$object$lambda.cv, alpha = object$object$alpha)
    return(pred)
}