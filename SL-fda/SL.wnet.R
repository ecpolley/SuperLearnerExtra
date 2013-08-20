##################################################################
##################################################################
## wavelet+lasso; note predictors have to have length of 2^k for some k.
##################################################################
##################################################################

SL.wnet <- function(Y, X, newX, family, obsWeights, id, ...){
  
  ## function for wnet that gives predictions; also silences output
  wnet.pred =
    function (y, xfuncs, xfuncs.pred, min.scale, alpha, lambda = NULL, standardize = FALSE, 
              covt = NULL, pen.covt = FALSE, filter.number = 10, wavelet.family = "DaubLeAsymm", 
              family = "gaussian", nfold = 5, compare.fits = FALSE, store.cv = FALSE, 
              ...) 
    {
      n <- length(y)
      if (!is.array(xfuncs) || !length(dim(xfuncs)) %in% 2:3) 
        stop("Argument xfuncs is invalid: must either be a 2D or 3D array.")
      dim.sig <- length(dim(xfuncs)) - 1
      if (dim(xfuncs)[1] != n) 
        stop("Arguments y and xfuncs has invalid lengths: ", 
             length(y), " and ", dim(xfuncs)[1], ".")
      if (dim(xfuncs)[2] != dim(xfuncs)[1 + dim.sig]) 
        stop("Number of rows and columns in image are not identical: ", 
             dim(xfuncs)[2], " and ", dim(xfuncs)[1 + dim.sig])
      d <- dim(xfuncs)[2]
      if (as.integer(log2(d)) != log2(d)) 
        stop("Argument xfuncs is invalid: the length of xfuncs must be of \n             power of 2.")
      if (sum(!min.scale %in% 0:(log2(d) - 1)) != 0) 
        stop("Argument min.scale is invalid: must be integer(s) between 0 and ", 
             log2(d) - 1, ".")
      if (alpha < 0 || alpha > 1) 
        stop("Argument alpha s invalid: must in [0,1].")
      if (!nfold %in% 1:n) 
        stop("Argument nfold is invalid: must be an integer between 1 and ", 
             n, ".")
      groups <- split(sample(1:n), rep(1:nfold, length = n))
      if (dim.sig == 1) {
        wave.decomp <- wd
        dec <- decomp
        rec <- reconstr
      }    else {
        wave.decomp <- imwd
        dec <- decomp2d
        rec <- reconstr2d
      }
      wdobjs <- apply(xfuncs, 1, wave.decomp, filter.number = filter.number, 
                      family = wavelet.family)
      
      
      wdobjs.pred <- apply(xfuncs.pred, 1, wave.decomp, filter.number = filter.number, 
                           family = wavelet.family)
      
      
      temp <- dec(wdobjs[[1]])
      p <- length(temp$coef)
      type.gaussian <- ifelse(p > n, "naive", "covariance")
      n.covt <- if (is.null(covt)) 
      { 0 } else ncol(as.matrix(covt))
      penalty.factor <- if (pen.covt) 
      {rep(1, n.covt + p)} else c(rep(0, n.covt), rep(1, p))
      cv.table <- lambda.table <- array(0, dim = c(length(min.scale), 
                                                   length(alpha), ifelse(is.null(lambda), 100, length(lambda))))
      dimnames(cv.table) <- list(paste("ms=", min.scale, sep = ""), 
                                 paste("alpha=", alpha, sep = ""), if (is.null(lambda)) NULL else paste("lambda=", 
                                                                                                        lambda, sep = ""))
      if (compare.fits) {
        fhat.table <- array(0, dim = c(d^dim.sig, nfold, length(min.scale), 
                                       length(alpha), ifelse(is.null(lambda), 100, length(lambda))))
        dimnames(fhat.table) <- list(NULL, paste("nfold =", 1:nfold), 
                                     paste("ms =", min.scale), paste("alpha =", alpha), 
                                     paste("lambda", if (is.null(lambda)) 1:100 else lambda))
      }
      for (ims in 1:length(min.scale)) {
        coef <- t(array(unlist(sapply(wdobjs, dec, min.scale = min.scale[ims])[1, 
                                                                               ]), dim = c(p, n)))
        for (ialpha in 1:length(alpha)) {
          if (is.null(lambda)) {
            obje <- glmnet(x = as.matrix(cbind(covt, coef)), 
                           y = y, family = family, alpha = alpha[ialpha], 
                           standardize = standardize, type.gaussian = type.gaussian, 
                           penalty.factor = penalty.factor, ...)
            templam <- range(obje$lambda)
            lambda.table[ims, ialpha, ] <- seq(templam[1], 
                                               templam[2], length = 100)
          }
          else {
            lambda.table[ims, ialpha, ] <- lambda
          }
          for (ifold in 1:nfold) {
            #                cat("min.scale:", min.scale[ims], "\t", "alpha:", 
            #                  alpha[ialpha], "fold:", ifold, "\n")
            idxTest <- groups[[ifold]]
            idxTrain <- (1:n)[-idxTest]
            obje <- glmnet(x = as.matrix(cbind(covt, coef)[idxTrain, 
                                                           ]), y = y[idxTrain], lambda = lambda.table[ims, 
                                                                                                      ialpha, ], family = family, alpha = alpha[ialpha], 
                           standardize = standardize, type.gaussian = type.gaussian, 
                           penalty.factor = penalty.factor, ...)
            if (compare.fits) {
              theta.w <- matrix(predict(obje, s = lambda.table[ims, 
                                                               ialpha, ], type = "coefficients"), ncol = dim(lambda.table)[3])
              temp$callInfo$min.scale <- min.scale[ims]
              for (ilambda in 1:dim(lambda.table)[3]) {
                temp$coef <- theta.w[, ilambda]
                fhat.table[, ifold, ims, ialpha, ilambda] <- as.vector(rec(temp))
              }
            }
            yhat <- predict(obje, newx = as.matrix(cbind(covt, 
                                                         coef)[idxTest, ]), s = lambda.table[ims, ialpha, 
                                                                                             ], type = "response")
            if (family == "gaussian") 
              cv.table[ims, ialpha, ] <- cv.table[ims, ialpha, 
                                                  ] + colMeans((y[idxTest] - yhat)^2)
            else if (family == "binomial") {
              misclass <- function(x) mean((x > mean(y[idxTrain])) != 
                                             y[idxTest])
              cv.table[ims, ialpha, ] <- cv.table[ims, ialpha, 
                                                  ] + apply(yhat > mean(y[idxTrain]), 2, misclass)
            }
          }
        }
      }
      idxmin <- which(cv.table == min(cv.table[cv.table != 0], 
                                      na.rm = TRUE), arr.ind = TRUE)
      if (nrow(idxmin) > 1) 
        idxmin <- idxmin[1, ]
      min.scale <- min.scale[idxmin[1]]
      alpha <- alpha[idxmin[2]]
      lambda <- lambda.table[idxmin[1], idxmin[2], idxmin[3]]
      coef <- t(array(unlist(sapply(wdobjs, dec, min.scale = min.scale)[1, 
                                                                        ]), dim = c(p, n)))
      
      
      coef.pred <- t(array(unlist(sapply(wdobjs.pred, dec, min.scale = min.scale)[1, 
                                                                                  ]), dim = c(p, dim(xfuncs.pred)[1])))
      
      
      obje <- glmnet(x = as.matrix(cbind(covt, coef)), y = y, lambda = lambda, 
                     family = family, alpha = alpha, standardize = standardize, 
                     type.gaussian = type.gaussian, penalty.factor = penalty.factor, ...)
      theta <- as.numeric(predict(obje, s = lambda, type = "coefficients"))
      yhat <- predict(obje, newx = as.matrix(cbind(covt, coef)), 
                      s = lambda, type = "response")
      
      
      yhat.pred <- predict(obje, newx = as.matrix(cbind(covt, coef.pred)), 
                           s = lambda, type = "response")
      
      
      ret = list(yhat.pred); names(ret) = c("fitted")
      return(ret)
    }
  
  
  stopifnot(require(refund))
  
  if(family$family == 'gaussian'){
    X <- as.matrix(X)
    newX <- as.matrix(newX)
    
    ntrain <- nrow(X)
    ntest <- nrow(newX)
    
    # make sure we have 2^k columns:
    if(log2(ncol(X))%%1 != 0){
      padding <- 2^ceiling(log2(ncol(X))) -ncol(X)
      X <- cbind(X, matrix(0, ncol=padding, nrow=nrow(X)))
      newX <- cbind(newX, matrix(0, ncol=padding, nrow=nrow(newX)))
    }
    
    m <- try(wnet.pred(y=Y, min.scale = 4, alpha = 1, xfuncs=X, xfuncs.pred = newX))
    if(class(m)[1] != "try-error"){
      pred <- m$fitted
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
