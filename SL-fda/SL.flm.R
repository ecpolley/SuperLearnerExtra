##################################################################
##################################################################
## functional linear model
##################################################################
##################################################################

SL.flm <- function(Y, X, newX, family, obsWeights, id, ...){
  
  stopifnot(require(mgcv))
  
  if(family$family == 'gaussian'){
    X <- as.matrix(X)
    newX <- as.matrix(newX)
    
    ntrain <- nrow(X)
    ntest <- nrow(newX)
    freq <- matrix(1:ncol(X), ncol=ncol(X), nrow=ntrain, byrow=TRUE)
    
    m <- try(gam(Y ~ s(freq, by=X, k=min(nrow(X)-1, 40), bs="ad")))
    
    if(class(m)[1] != "try-error"){
      freq <- matrix(1:ncol(X), ncol=ncol(X), nrow=ntest, byrow=TRUE)
      pred <- predict(m, newdata=list(X=newX, freq=freq))
    } else {
      pred = rep(NA, nrow(newX))
    }
  }
  if(family$family == 'binomial'){
    stop("Only gaussian outcomes allowed")
  }
  
  fit = vector("list", length = 0)
  class(fit) <- 'SL.template'
  out <- list(pred = pred, fit = fit)
  return(out)
  
}