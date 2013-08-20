##################################################################
##################################################################
## lasso
##################################################################
##################################################################

SL.flasso <- function(Y, X, newX, family, obsWeights, id,  smoothFPC=FALSE, 
                     fpcaArgs=formals(fpca.face)[-1], nFPC=min(30, min(dim(X))), 
                     ...){
  
  stopifnot(require(glmnet))
  
  if(family$family == 'gaussian'){
    
    X <- as.matrix(X)
    newX <- as.matrix(newX)
    
    ## FPC decomp: replace X, newX with respective FPC scores
    if(smoothFPC){
      stopifnot(require(refund))
      fpcaArgs$Y <- X
      fpcaVal <- do.call(fpca.face, fpcaArgs)
      X <- fpcaVal$scores
      newX <-t(t(fpcaVal$eigenvectors)%*%(t(newX)-fpcaVal$mu))
    } else {
      stopifnot(require(irlba))
      svdX <- irlba(X, nv=nFPC, nu=0)
      efcts <- t(svdX$v)
      evals <- svdX$d^2
      X <- t(efcts%*%t(X)) 
      newX <- t(efcts%*%t(newX)) 
    }
    
    
    m <- try(cv.glmnet(x=X, y=Y, alpha = 1))
    
    if(class(m)[1] != "try-error"){
      pred <- try(predict(m, newx=newX))
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