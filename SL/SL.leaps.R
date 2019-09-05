SL.leaps <- function (Y, X, newX, family, obsWeights, id, leaps.method = 'adjr2', nvmax = 4,...) 
{
	requireNamespace("leaps")
    if (family$family == "gaussian") {
    fit_leaps <- leaps::regsubsets(x = X, y = Y, nvmax = nvmax, intercept = TRUE, method = 'exhaustive', really.big = ifelse(ncol(X) >= 50, TRUE, FALSE), nbest = 1, weights = obsWeights)
    model.id <- which.max(summary(fit_leaps)[[leaps.method]])
    var_in_final_model <- setdiff(names(coef(fit_leaps, id = model.id)), "(Intercept)")
	final_formula <- as.formula(paste("Y~", paste(var_in_final_model, collapse = "+"), sep = ""))
	final_fit <- lm(final_formula, data = data.frame(Y, X), weights = obsWeights)
	fit <- list(object = final_fit, final_formula = final_formula, regsubsets_object = fit_leaps)  # do we need to save the regsubsets output?
    }
    if (family$family == "binomial") {
    	stop("leaps not available for binomial outcomes")
    }
    pred <- predict(final_fit, newdata = newX, type = 'response')
    class(fit) <- c("SL.leaps")
    out <- list(pred = pred, fit = fit)
    return(out)
}

predict.SL.leaps <- function (object, newdata, ...) 
{
	# final object is a lm fit, so don't require the leaps package in namespace for prediction
    pred <- predict(object = object$object, newdata = newdata, type = "response")
    pred
}