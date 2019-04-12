#' Function method.stratifiedNNLS
#'
#' Function method.stratifiedNNLS creates a method to build a SuperLearner object
#'
#' @param  SL.library.stratify A  \code{character}  giving  the name  of  the
#'   algorithm used to stratify data. See the Details section below.
#'
#' @return Returns  a method that can  be used to build  a SuperLearner object
#'   using  the  'SuperLearner::SuperLearner'  function and  the  likes.   See
#'   'SuperLearner::method.template()'.
#'
#' @details The algorithm used to stratify data is defined like any prediction
#'   algorithm,         see          'SuperLearner::SL.template()'         and
#'   'SuperLearner::predict.SL.template()'.   The main  "stratifying function"
#'   comes   along    with   a    "prediction   function".     CAUTION:   both
#'   "stratifying   function"   and    "prediction   function"   MUST   output
#'   \code{integer}s.
#'
#'   Do not forget to append the \code{character} to the 'SL.library' argument
#'   in the eventual call to the 'SuperLearner::SuperLearner' function and the
#'   likes.
#'
#'   (A  contribution from  Geoffrey Ecoto,  CCR, and  Antoine Chambaz,  MAP5,
#'   Université Paris Descartes)
#' 
#' @examples
#' ## -------------------------------
#' ## code taken from '?SuperLearner'
#' ## -------------------------------
#' library(SuperLearner)
#' ## simulate data
#' set.seed(23432)
#' ## training set
#' n <- 500
#' p <- 50
#' X <- matrix(rnorm(n*p), nrow = n, ncol = p)
#' colnames(X) <- paste("X", 1:p, sep="")
#' X <- data.frame(X)
#' Y <- X[, 1] + sqrt(abs(X[, 2] * X[, 3])) + X[, 2] - X[, 3] + rnorm(n)
#' ## test set
#' m <- 1000
#' newX <- matrix(rnorm(m*p), nrow = m, ncol = p)
#' colnames(newX) <- paste("X", 1:p, sep = "")
#' newX <- data.frame(newX)
#' newY <- newX[, 1] + sqrt(abs(newX[, 2] * newX[, 3])) + newX[, 2] -
#'   newX[, 3] + rnorm(m)
#' 
#' ## generate Library and run Super Learner
#' SL.library <- c("SL.glm", "SL.randomForest", "SL.gam",
#'                 "SL.polymars", "SL.mean")
#' test <- SuperLearner(Y = Y, X = X, newX = newX, SL.library = SL.library,
#'                      verbose = TRUE, method = "method.NNLS")
#' test
#' 
#' ## -----------------------------------------------------
#' ## same as above, but one convex combination per stratum
#' ## where strata are defined by algorithm 'SL.stratify'
#' ## -----------------------------------------------------
#' 
#' ## CAUTION: 'SL.stratify' and 'predict.SL.stratify' MUST output integers
#' 
#' SL.stratify <- function(Y, X, newX, family, obsWeights, id, ...) {
#'   if (!is.matrix(X)) {
#'     X <- as.matrix(X)
#'   }
#'   X <- sign(X[, 2] * X[, 3])
#'   
#'   if (!is.matrix(newX)) {
#'     newX <- as.matrix(newX)
#'   }
#'   pred <- as.integer(sign(newX[, 2] * newX[, 3]))
#'   fit <- list(object = X)
#'   out <- list(pred = pred, fit = fit)
#'   class(out$fit) <- "SL.statify"
#'   return(out)
#' }
#' environment(SL.stratify) <- asNamespace("SuperLearner")
#' 
#' 
#' predict.SL.stratify <- function (object, newdata, family, X = NULL, Y = NULL, ...) {
#'   if (!is.matrix(newdata)) {
#'     newdata <- as.matrix(newdata)
#'   }
#'   pred <- as.integer(sign(newdata[, 2] * newdata[, 3]))
#'   return(pred)
#' }
#' environment(predict.SL.stratify) <- asNamespace("SuperLearner")
#' 
#' method.stratif.NNLS <- method.stratifiedNNLS("SL.stratify")
#' environment(method.stratif.NNLS) <- asNamespace("SuperLearner")
#' 
#' test.stratified <- SuperLearner(Y = Y, X = X, newX = newX,
#'                                 SL.library = c(SL.library, "SL.stratify"),
#'                                 verbose = TRUE,
#'                                 method = method.stratif.NNLS)
#' test.stratified
#' 
#' @export
method.stratifiedNNLS <- function(SL.library.stratify) {
  if (missing(SL.library.stratify)) {
    warning("Using 'method.NNLS'.\n")
    out <- SuperLearner::method.NNLS()
  } else {
    if (!all(is.character(SL.library.stratify))) {
      stop("Argument 'SL.library.stratify' in call to 'method.stratifiedNNLS' must be a character vector.\n")
    }
    SL.library.stratify <- paste0(SL.library.stratify, sep = "_All")
    out <- list(
      require = c("nnls", "magrittr", "tibble", "dplyr"), 
      computeCoef = function(Z, Y, libraryNames, obsWeights, control, verbose, ...) {
        `%>%` <- magrittr::`%>%`
        if (length(setdiff(SL.library.stratify, libraryNames))) {
          stop("Argument 'SL.library.stratify' in call to 'method.stratifiedNNLS' must be a subset of argument 'SL.library' in upstream call to 'SuperLearner::SuperLearner'.\n")
        }
        SL.library.stratify.idx <- grep(paste0(SL.library.stratify, collapse = "|"), libraryNames)
        ## cross-validated risks
        cvRisk <- apply(Z[, -SL.library.stratify.idx, drop = FALSE], 2, function(x) mean(obsWeights * (x - Y)^2))
        SL.library <- libraryNames[-SL.library.stratify.idx]
        names(cvRisk) <- SL.library
        ## stratified NNLS optimization
        NNLS <- function(tbl) {
          weights <- sqrt(dplyr::pull(tbl, weights))
          Z <- tbl %>% dplyr::select(SL.library) %>% as.matrix
          fit.nnls <- nnls::nnls(weights * Z, weights * dplyr::pull(tbl, Y))
          init.coef.nnls <- stats::coef(fit.nnls)
          names(init.coef.nnls) <- SL.library
          init.coef.nnls[is.na(init.coef.nnls)] <- 0
          if (sum(init.coef.nnls) > 0) {
            coef.nnls <- init.coef.nnls/sum(init.coef.nnls)
          } else {
            warning("All algorithms have zero weight", call. = FALSE)
            coef.nnls <- init.coef.nnls
          }
          if (verbose) {
            message(paste("Stratified non-negative least squares convergence:",
                          fit.nnls$mode == 1))
          }
          return(dplyr::bind_rows(coef.nnls))
        }
        data <- cbind(Y, Z, obsWeights)
        colnames(data) <- c("Y", libraryNames, "weights")
        fit.nnls <- data %>%
          (tibble::as_tibble) %>%
          dplyr::group_by_at(SL.library.stratify) %>%
          dplyr::do(NNLS(.))
        fit.nnls <- as.matrix(fit.nnls)
        idx <- grep(SL.library.stratify, colnames(fit.nnls))
        coef <- t(fit.nnls[, -idx])
        colnames(coef) <- fit.nnls[, idx]
        attr(coef, "SL.library.stratify") <- SL.library.stratify
        attr(coef, "libraryNames") <- libraryNames
        attr(coef, "SL.library") <- SL.library
        out <- list(cvRisk = cvRisk, coef = coef)
        return(out)
      },
      computePred = function(predY, coef, ...) {
        `%>%` <- magrittr::`%>%`
        SL.library.stratify <- attr(coef, "SL.library.stratify")
        libraryNames <- attr(coef, "libraryNames")
        SL.library <- attr(coef, "SL.library")
        col.sums <- apply(coef, 2, sum)
        if (any(col.sums == 0)) {
          stop("On at least one stratum, the metalearner coefficients are all zero, so cannot compute prediction.\n")
        }
        colnames(predY) <- libraryNames
        compute.preds <- function(tbl) {
          coef <- tbl %>%
            dplyr::select(paste0(SL.library, sep = ".y")) %>%
            (dplyr::distinct) %>% as.matrix
          predY <- tbl %>%
            dplyr::select(paste0(SL.library, sep = ".x")) %>%
            as.matrix
          out <- crossprod(t(predY[, coef != 0, drop = FALSE]),
                           coef[coef != 0])
          tbl <- tbl %>%
            dplyr::mutate(out = out)
          return(tbl)
        }
        coef <- as.data.frame(t(coef))
        coef[, SL.library.stratify] <- as.integer(rownames(coef))
        coef <- tibble::as_tibble(coef)
        suppressWarnings(#
          out <- predY %>% (tibble::as_tibble) %>%
            (tibble::rownames_to_column) %>%
            dplyr::inner_join(coef, by = SL.library.stratify) %>%
            dplyr::group_by_at(SL.library.stratify) %>%
            dplyr::do(compute.preds(.))#
        )
        out <- out %>% (dplyr::ungroup) %>% dplyr::arrange(as.integer(rowname)) %>% dplyr::pull(out)
        return(out)
      }
    )
  }
  invisible(out)
}
environment(method.stratifiedNNLS) <- asNamespace("SuperLearner")
