##################################################################
##################################################################
## spike and slab generalized additive model
##################################################################
##################################################################

SL.spikeslabgam <- function(Y, X, newX, family, obsWeights, id, smoothFPC=FALSE, 
                            fpcaArgs=formals(fpca.face)[-1], nFPC=min(30, min(dim(X))), 
                            ...)
  # default for smoothFPC=FALSE is to use first 30 components
  # default for smoothFPC=TRUE is to use the fpca.face defaults
{
  
  stopifnot(require(spikeSlabGAM))
  options(mc.cores=1)
  
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
    
    nc=NCOL(X)
    
    scX <- scale(X[,1:nc])
    scnewX <- scale(newX[,1:nc], 
                    center=attr(scX, "scaled:center"),
                    scale=attr(scX, "scaled:scale"))
    if(is.null(colnames(scX))) {
      colnames(scX) <- colnames(scnewX) <- paste("x",1:nc,sep="")
    }
    scy <- scale(Y)/2
    data <- data.frame(y=scy, scX)
    frml <- formula(paste("y~", paste("sm(", colnames(scX), ", K=8, centerBase=F)", collapse="+")))
    m <- try(spikeSlabGAM(formula=frml, data=data))
    
    if(class(m)[1] != "try-error"){
      pred <-  try(2*attr(scy, "scaled:scale")*predict(m, 
                                                       newdata=data.frame(y=NA, scnewX)) +
                     attr(scy, "scaled:center"))
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