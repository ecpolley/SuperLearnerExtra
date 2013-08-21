##################################################################
##################################################################
## single-index signal regression
##################################################################
##################################################################

SL.sisr <- function(Y, X, newX, family, obsWeights, id, ...){
  
  sisr <- function(y, t=1:ncol(X), X, d=40, pendegree=2, 
                   tol=1e-3, maxiter=50,
                   plot=TRUE, verbose=TRUE){
    stopifnot(require(mgcv))
    #use notation from B. Marx slides:
    # d = dim basis
    # P = penalty matrix
    # T = basis matrix
    d <- pmin(d, length(y)-1)
    D <- switch(pendegree, 
                "null"=diag(d), "1"=diff(diag(d)), "2"=diff(diff(diag(d))))
    P <- crossprod(D)
    
    T <- bs(t, intercept = TRUE, df=d)
    M <- X%*%T
    
    y <- drop(y)
    
    
    if(plot) layout(t(1:5))
    
    ystar <- y
    Mstar <- M
    m <- gam(ystar ~ Mstar -1 , paraPen=list(Mstar=list(P)))
    gammahat.new <- m$coefficients/sqrt(sum(m$coefficients^2))
    gammahat.0 <- 2*gammahat.new
    Mgamma <- drop(M%*%gammahat.new) 
    mse <- rep(NA,maxiter)
    iter <- 0
    while((mean(abs(gammahat.0 - gammahat.new)/pmax(abs(gammahat.0),abs(gammahat.new))) > tol) && (iter<maxiter)){
      iter  <- iter + 1
      
      
      mf <- gam(y ~ s(Mgamma))
      fhat <- drop(fitted(mf))
      
      mse[iter] <- mean((fhat-y)^2)
      if(verbose) {
        cat("iter: ", iter, 
            "diff(gamma)/|gamma|:", mean(abs(gammahat.0 - gammahat.new)/pmax(abs(gammahat.0),abs(gammahat.new))), 
            "mse:", mse[iter], "\n")
      }
      
      h <- max(1e-5, min(diff(sort(Mgamma)))/10 )
      fderiv <- drop( predict(mf, newdata=data.frame(Mgamma=Mgamma+h)) - predict(mf, newdata=data.frame(Mgamma=Mgamma-h)))/(2*h)
      if(plot){
        plot(t, T%*%gammahat.new, type="l", ask=TRUE)
        plot(Mgamma, fhat)
        plot(Mgamma, fderiv)
        plot(y, fhat); abline(c(0,1))
        plot(1:iter, mse[1:iter], type="l")
      } 
      
      ystar <- y - fhat + fderiv*Mgamma
      Mstar <- diag(fderiv)%*%M
      
      m <- gam(ystar ~ Mstar -1 , paraPen=list(Mstar=list(P)))
      gammahat.0 <- gammahat.new
      gammahat.new <- m$coefficients/sqrt(sum(m$coefficients^2))
      Mgamma <- drop(M%*%gammahat.new)
      
    }
    mf <- gam(y ~ s(Mgamma))
    
    ret <- list(gammahat=gammahat.new, fit=fitted(mf), mf=mf, T=T, call=match.call())
    return(ret)
  }    
  
  predict.sisr <- function(obj, Xnew){
    Mgammanew <- Xnew%*%(obj$T%*%obj$gammahat)
    predict(obj$mf, newdata=list(Mgamma=Mgammanew))
  }
  
  
  
  stopifnot(require(mgcv))
  
  if(family$family == 'gaussian'){
    X <- as.matrix(X)
    newX <- as.matrix(newX)
    
    m <- try(sisr(y=Y, X=X, maxiter=500, d=40, pendegree=2, plot=FALSE, verbose=FALSE))
    
    if(class(m)[1] != "try-error"){
      pred <- predict.sisr(m, Xnew=newX)
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