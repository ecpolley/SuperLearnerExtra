##################################################################
##################################################################
## fregrenp
##################################################################
##################################################################

SL.fregrenp <- function(Y, X, newX, family, obsWeights, id, ...){
  
  stopifnot(require(fda.usc))
  
  if(family$family == 'gaussian'){
    X <- as.matrix(X)
    newX <- as.matrix(newX)
    
    fdTrain <- fdata(X)
    fdTest <- fdata(newX)            
    m <- try(fregre.np(fdTrain, Y))
    
    if(class(m)[1] != "try-error"){
      pred <- predict(m, new.fdataobj=fdTest)
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