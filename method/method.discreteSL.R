method.discreteSL <- function() {
    out <- list(require = NULL, 
       computeCoef = function(Z, Y, libraryNames, verbose, obsWeights, ...) {
        cvRisk <- apply(Z, 2, function(x) mean(obsWeights * (x - Y)^2)) # This is MSE, change if you want a different loss function 
        names(cvRisk) <- libraryNames
        discrete_SL_name <- libraryNames[which.min(cvRisk)] # if ties, will take the first in the list
        coef <- rep(0, length(cvRisk))
        coef[which.min(cvRisk)] <- 1 # give weight 1 to the minimum CV risk estimate, all others 0
        out <- list(cvRisk = cvRisk, coef = coef, discrete_SL_name = discrete_SL_name)
        return(out)
    }, computePred = function(predY, coef, ...) {
        out <- crossprod(t(predY), coef) # this could be simplified, but leaving as matrix crossprod 
        return(out)
    })
    invisible(out)
}
