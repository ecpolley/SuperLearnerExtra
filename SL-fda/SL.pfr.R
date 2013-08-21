##################################################################
##################################################################
## PFR - modified version of pfr.bslp 
##################################################################
##################################################################

SL.pfr <- function(Y, X, newX, family, obsWeights, id, ...){
  
  stopifnot(require(mgcv))
  
  if(family$family == 'gaussian'){
    X <- as.matrix(X)
    newX <- as.matrix(newX)
    
    kz = kb = 40
    
    I = length(Y)
    t = seq(0, 1, length = dim(X)[2])
    
    # set the basis to be used for beta(t)
    phi = cbind(1, bs(t, df=kb-1, intercept=FALSE, degree=2))
    
    XJ = X %*% phi
    XJ.valid = newX %*% phi
    
    X = cbind(rep(1, I), XJ)
    
    ## set design and penalty matrices depending
    fixed.mat = X[,1:(1+1)]
    rand.mat = X[,(1+2):(1+kb)]
    
    temp=matrix(0, nrow=kb-1, ncol=kb-1)
    for(i in 1:(kb-1)){
      for(j in 1:(kb-1)){
        temp[i,j]=min(i,j)-1
      }
    }
    D = matrix(1, nrow=kb-1, ncol=kb-1)+temp
    Dinv=solve(D)
    PenMat = list(length = 1)
    PenMat[[1]] = cbind(matrix(0, 1+kb, 1+1), rbind(matrix(0, 1+1, kb-1), Dinv))
    
    m = try(gam(Y ~ X - 1, paraPen = list(X = PenMat), method = "REML", family = "gaussian"))
    
    if(class(m)[1] != "try-error"){
      
      coefs = m$coef
      
      X.valid = cbind(1, XJ.valid)
      pred = X.valid %*% coefs
      
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