##################################################################
##################################################################
## partial least squares
##################################################################
##################################################################

SL.plsreg <- function(Y, X, newX, family, obsWeights, id, ...){
  
  stopifnot(require(ppls))
  
  if(family$family == 'gaussian'){
    X <- as.matrix(X)
    newX <- as.matrix(newX)
    
    m <-  try(penalized.pls.cv(X=X, y=Y, k=15))
    
    if(class(m)[1] != "try-error"){
      pred <- try(drop(new.penalized.pls(m, newX)$ypred))
      if(class(pred)[1] == "try-error"){
        pred = rep(NA, nrow(newX))
      } 
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
