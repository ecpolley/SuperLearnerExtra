##################################################################
##################################################################
## fgam
##################################################################
##################################################################

SL.fgam <- function(Y, X, newX, family, obsWeights, id, ...){
  
  stopifnot(require(refund))
  
  if(family$family == 'gaussian'){
    X <- as.matrix(X)
    newX <- as.matrix(newX)
    
    ntrain <- nrow(X)
    ntest <- nrow(newX)
    splinepars <- list(bs = "ps", k= rep(min(floor(sqrt(ntrain)), 8), 2))
    m <- try(fgam(Y ~ af(X, splinepars=splinepars, presmooth=FALSE, 
                         Xrange=range(X, newX))))
    
    if(class(m)[1] != "try-error"){
      pred <- predict(m, newdata=list(X=newX))
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