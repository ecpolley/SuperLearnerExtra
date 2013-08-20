##################################################################
##################################################################
## functional principal components regression
##################################################################
##################################################################

SL.fpcr <- function(Y, X, newX, family, obsWeights, id, ...){
  
  stopifnot(require(refund))
  
  if(family$family == 'gaussian'){
    X <- as.matrix(X)
    newX <- as.matrix(newX)
    
    ntrain <- nrow(X)
    ntest <- nrow(newX)
    
    m <- try(fpcr(y=c(Y, rep(0,ntest)), nbasis=min(ntrain+ntest-1, 80), 
                  ncomp=30, xfuncs=rbind(X, newX),
                  weights=c(rep(1, ntrain), rep(0, ntest))))
    if(class(m)[1] != "try-error"){
      pred <- fitted(m)[-(1:ntrain)]
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