##################################################################
##################################################################
## flirti - modified from original flirti code; intercept and 
## some options have been removed.
## Source: Gareth M. James, Jing Wang, Ji Zhu 
## "Functional linear regression that's interpretable"
## http://arxiv.org/abs/0908.2918
##################################################################
##################################################################


SL.flirti <- function(Y, X, newX, family, obsWeights, id, ...){
  
  stopifnot(require(mgcv))
  stopifnot(require(lpSolve))
  
  ## helper functions from GM James' code
  `linearoptim` <-
    function(Y,X,sigma,lambdap,weights=1,A,extra.con=NULL){
      p <- ncol(X)
      K <- nrow(A)
      f.obj <- weights
      XX <- t(X)%*%X
      b1 <- lambdap*sigma+as.vector(t(X)%*%Y)
      b2 <- lambdap*sigma-as.vector(t(X)%*%Y)
      f.con1 <- cbind(XX,-XX,matrix(0,p,2*K))
      f.con2 <- cbind(-XX,XX,matrix(0,p,2*K))
      f.con3 <- cbind(A,-A,diag(K),-diag(K))
      f.con <- rbind(f.con1,f.con2,f.con3)
      f.rhs <- c(b1,b2,rep(0,K))
      f.dir <- c(rep("<=",2*p),rep("==",K))
      if (!is.null(extra.con)){
        f.con <- rbind(f.con,extra.con)
        f.rhs <- c(f.rhs,0)
        f.dir <- c(f.dir,"==")
      }
      lpout <- lp("min",f.obj,f.con,f.dir,f.rhs)
      eta <- lpout$sol[1:p]-lpout$sol[(p+1):(2*p)]
      gamma <-lpout$sol[(2*p+1):(2*p+K)]-lpout$sol[(2*p+K+1):(2*p+2*K)]#gamma here is acutally -gamma
      if (lpout$status!=0){
        print(paste("Error: Code = ",lpout$status))
      }
      list(eta=eta,gamma=gamma,lpfit=lpout,code=lpout$status)}
  
  `makeA` <-
    function(p,deriv=2,int=F){
      A <- NULL
      if (!is.na(match(1,deriv))){
        a <- c(-1,1,rep(0,p-1))
        K <- p-1
        A <- rbind(A,matrix(rep(a,K)[1:(K*p)],K,p,byrow=T))
      }
      if (!is.na(match(2,deriv))){
        a <- c(1,-2,1,rep(0,p-2))
        K <- p-2
        A <- rbind(A,matrix(rep(a,K)[1:(K*p)],K,p,byrow=T))
      }
      if (!is.na(match(3,deriv))){
        a <- c(-1,3,-3,1,rep(0,p-3))
        K <- p-3
        A <- rbind(A,matrix(rep(a,K)[1:(K*p)],K,p,byrow=T))
      }
      if (!is.na(match(4,deriv))){
        a <- c(-1,4,-6,4,-1,rep(0,p-4))
        K <- p-4
        A <- rbind(A,matrix(rep(a,K)[1:(K*p)],K,p,byrow=T))
      }
      if (int)
        A <- cbind(0,A)
      A}
  
  
  if(family$family == 'gaussian'){
    X <- as.matrix(X)
    newX <- as.matrix(newX)
    
    ## set tuning parameters
    deriv=2; weight=1; sigma = .001
    
    p <- ncol(X)
    beta.zero <- 0
    extra.con <- NULL
    if (length(weight)>1){
      deriv <- (1:4)[weight[-1]>0]  
    } else {
      weight <- c(weight,0,0,0,0)
      weight[deriv+1] <- 1
    }
    
    weights <- rep(weight[1],2*p)
    for (i in 2:5)
      if (weight[i]>0)
        weights <- c(weights,rep(weight[i],2*(p-i+1)))
    
    A <- makeA(p,deriv,FALSE)
    colscale <- apply(X,2,function(x){sqrt(sum(x^2))})
    newW <- t(t(X)/colscale)
    
    m <- try(linearoptim(Y,newW,sigma,sqrt(2*p),weights,A%*%diag(1/colscale),extra.con))
    
    if(class(m)[1] != "try-error"){
      beta <- m$eta/colscale
      pred = newX%*%beta
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