##################################################################
##################################################################
## pc linear model
##################################################################
##################################################################

SL.pclm <- function(Y, X, newX, family, obsWeights, id, smoothFPC=FALSE, 
                    fpcaArgs=formals(fpca.face)[-1], nFPC=min(30, min(dim(X))), 
                    ...){
  # default for smoothFPC=FALSE is to use first 30 components
  # default for smoothFPC=TRUE is to use the fpca.face defaults
  if(family$family == 'gaussian'){
    #browser()
    
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
    
    nc=NCOL(X)
    B=20
    
    boot <- function(){
      error <- rep(NA, nc)
      train <- sort(sample(1:nrow(X), nrow(X), replace=TRUE))
      test <- setdiff(1:nrow(X),train)
      for(n.col in 1:nc){
        y <- Y[train]
        x <- X[train,1:n.col]
        m <- lm(y ~ x)
        pred <- predict(m, newdata=list(x=X[test, 1:n.col]))
        error[n.col] <- mean((pred-Y[test])^2)
      }
      return(error)
    } 
    err <- try(replicate(B, boot()))
    use.nc <- try(which.min(rowMeans(err)))
    x <- try(X[,1:use.nc])
    m <- try(lm(Y ~ x))
    
    if(class(m)[1] != "try-error"){
      pred <-  predict(m, newdata=list(x=newX[,1:use.nc]))
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