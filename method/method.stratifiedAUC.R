#' Function method.stratifiedAUC
#'
#' Function method.stratifiedAUC creates a method to build a SuperLearner object
#'
#' @param SL.library.stratify A  \code{character}  giving  the name  of  the
#'   algorithm used to stratify data. See the Details section below.
#'
#' @param optim_method  A \code{character}  passed to  the \code{optim}  call
#'   method, either  'Nelder-Mead' (default)  or 'L-BFGS-B'.  See \code{optim}
#'   for details.
#' 
#' @param nlopt_method  A  \code{character} or  'NULL'  (the default  value).
#'   Either 'optim_method' or 'nlopt_method' must  be provided, the other must
#'   be 'NULL'.
#'
#' @param normalize  A \code{logical} (defaults to  'TRUE') indicating whether
#'   or not the parameters must be normalized to sum up to 1.
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
#' p <- 10
#' X <- matrix(rnorm(n*p), nrow = n, ncol = p)
#' colnames(X) <- paste("X", 1:p, sep = "")
#' X <- data.frame(X)
#' Y <- rbinom(n, 1, plogis(0.2 * X[, 1] + 0.1 * X[, 2] - 0.2 * X[, 3] +
#'                          0.1 * X[, 3] * X[, 4] - 0.2 * abs(X[, 4])))
#' ## test set
#' m <- 1000
#' newX <- matrix(rnorm(m * p), nrow = m, ncol = p)
#' colnames(newX) <- paste("X", 1:p, sep = "")
#' newX <- data.frame(newX)
#' newY <- rbinom(m, 1, plogis(0.2 * newX[, 1] + 0.1 * newX[, 2] - 0.2 * newX[, 3] +
#'                             0.1 * newX[, 3] * newX[, 4] - 0.2 * abs(newX[, 4])))
#' ## generate Library and run Super Learner
#' SL.library <- c("SL.glm", "SL.randomForest", "SL.gam",
#'                 "SL.polymars", "SL.mean")
#' test <- SuperLearner(Y = Y, X = X, newX = newX, SL.library = SL.library,
#'                      verbose = TRUE, method = "method.AUC")
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
#'   X <- sign(X[, 4])
#'   
#'   if (!is.matrix(newX)) {
#'     newX <- as.matrix(newX)
#'   }
#'   pred <- as.integer(sign(newX[, 4]))
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
#'   pred <- as.integer(sign(newdata[, 4]))
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
method.stratifiedAUC <- function(SL.library.stratify,
                                 optim_method = "L-BFGS-B", nlopt_method = NULL,
                                 normalize = TRUE) {
  if (is.null(nlopt_method) & is.null(optim_method)) {
    stop("Please supply either a 'nlopt' or 'optim' method, one of them being set to 'NULL'.") 
  }
  if (!is.null(nlopt_method) && !is.null(optim_method)) {
    stop("Please supply either a 'nlopt' or 'optim' method, not both.")
  }
  if (missing(SL.library.stratify)) {
    warning("Using 'method.AUC'.\n")
    out <- SuperLearner::method.AUC(optim_method = optim_method, nlopt_method = nlopt_method,
                                    bounds = bounds, normalize = normalize)
  } else {
    if (!all(is.character(SL.library.stratify))) {
      stop("Argument 'SL.library.stratify' in call to 'method.stratifiedAUC' must be a character vector.\n")
    }
    SL.library.stratify <- paste0(SL.library.stratify, sep = "_All")
    if (!is.null(optim_method)) {
      if (!(optim_method %in% c("Nelder-Mead", "L-BFGS-B"))) {
        stop("Supplied 'optim_method' value not supported.")
      }
      out <- list(
        require = c("cvAUC", "magrittr", "tibble", "dplyr"), 
        computeCoef = function(Z, Y, libraryNames, obsWeights, control, verbose,
                               errorsInLibrary = NULL, ...) {
          `%>%` <- magrittr::`%>%`
          if (length(setdiff(SL.library.stratify, libraryNames))) {
            stop("Argument 'SL.library.stratify' in call to 'method.stratifiedAUC' must be a subset of argument 'SL.library' in upstream call to 'SuperLearner::SuperLearner'.\n")
          }
          SL.library.stratify.idx <- grep(paste0(SL.library.stratify, collapse = "|"), libraryNames)
          ## cross-validated risks
          auc <- apply(Z[, -SL.library.stratify.idx, drop = FALSE], 2,
                       function(x) cvAUC::AUC(predictions = x, labels = Y))
          ## If 'SuperLearner::getCoeff' was updated  to include 'folds', then
          ## we could use the below 'auc' instead:
          if (FALSE) {
            auc <- apply(Z[, -SL.library.stratify.idx, drop = FALSE], 2,
                         function(x){cvAUC(x, labels=Y, folds = validRows)$cvAUC})
          }
          cvRisk <- 1 - auc  ## rank loss
          SL.library <- libraryNames[-SL.library.stratify.idx]
          names(cvRisk) <- SL.library
          ## stratified AUC-based optimization
          AUC_based_optim <- function(tbl) {
            .cvRisk_AUC <- function(par, Z, Y, folds = NULL, opt_meth) {
              if (opt_meth == "Nelder-Mead") {
                par <- exp(par)
              }
              ## Calculate cvRisk, which is 1 - cvAUC (rank loss). This is the
              ## general loss  function that gets  fed into optim as  the 'fn'
              ## argument.   Argument 'par'  is the  "weight/coef" vector  for
              ## ensemble in Super Learner.
              predictions <- crossprod(t(Z[, par != 0, drop = FALSE]), par[par != 0])
              ## Now  calculate cvRisk  (this is  what we  want to  minimize).
              ## Remark: might change this to  AUC only since we are currently
              ## not using folds arg...
              cvRisk <- 1 - cvAUC::cvAUC(predictions = predictions, labels = Y, folds = folds)$cvAUC
              return(cvRisk)
            }
            coef_init <- rep(1 / length(SL.library), length(SL.library))
            names(coef_init) <- SL.library
            
            ## Any  algorithms  with  NA  cvRisk   will  be  restricted  to  0
            ## coefficient.   Otherwise algorithms  with  NA risk  and all  NA
            ## predictions can still receive a positive coefficient. This does
            ## not  bode well  for  this optimization  algorithm  but we  will
            ## handle anyway.
            if (sum(errorsInLibrary) > 0) {
              if (verbose) {
                cat("Removing failed learners:",
                    paste(libraryNames[errorsInLibrary], collapse = ", "), "\n")
              }
              ## Setting upper_bounds to 0 causes optim() to error out.
              ## But this part isn't actually necessary.
              ## upper_bounds[errorsInLibrary] = 0
              
              ## Also update initial coefficients so that NA learners are set to 0.
              coef_init <- rep(1 / sum(!errorsInLibrary), length(SL.library))
              coef_init[errorsInLibrary] <- 0
            }
            Z <- tbl %>% dplyr::select(SL.library) %>% as.matrix
            if (optim_method == "L-BFGS-B") {
              res <- optim(par = coef_init,
                           fn = .cvRisk_AUC,
                           Z = Z,
                           Y = dplyr::pull(tbl, Y),
                           folds = NULL,
                           opt_meth = "L-BFGS-B",
                           method = optim_method,
                           lower = 0,
                           upper = Inf,
                           control = list(trace = 0))
            } else if (optim_method == "Nelder-Mead") {
              res <- optim(par = log(coef_init),
                           fn = .cvRisk_AUC,
                           Z = Z,
                           Y = dplyr::pull(tbl, Y),
                           folds = NULL,
                           opt_meth = "Nelder-Mead",
                           method = optim_method,
                           control = list(trace = 0))
            } else {
              stop(paste0("Method 'optim_method' must be either 'L-BFGS-B' or 'Nelder-Mead', not ", optim_method))
            }
              if (verbose) {
              if (res$convergence != 0) {
                warning(paste("'optim' did not converge when estimating the super learner coefficients, reason (see '?optim'): ", res$convergence, " 'optim' message: ", res$message))
              }
            }
            coef <- res$par
            if (optim_method == "Nelder-Mead") {
              coef <- exp(coef)
            }
            if (anyNA(coef)) {
              warning("Some algorithms have weights of NA, setting to 0.")
              coef[is.na(coef)] <- 0
            }
            if (!sum(coef) > 0) {
              warning("All algorithms have zero weight", call. = FALSE)
              coef <- coef_init
            }
            if (normalize) {
              coef <- coef / sum(coef)
            }
            names(coef) <- SL.library
            return(dplyr::bind_rows(coef))
          }
          data <- cbind(Y, Z, obsWeights)
          colnames(data) <- c("Y", libraryNames, "weights")
          fit.AUC <- data %>%
            (tibble::as_tibble) %>%
            dplyr::group_by_at(SL.library.stratify) %>%
            dplyr::do(AUC_based_optim(.))
          fit.AUC <- as.matrix(fit.AUC)
          idx <- grep(SL.library.stratify, colnames(fit.AUC))
          coef <- t(fit.AUC[, -idx])
          colnames(coef) <- fit.AUC[, idx]
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
          out <- predY %>% (tibble::as_tibble) %>%
            (tibble::rownames_to_column) %>%
            dplyr::inner_join(coef, by = SL.library.stratify) %>%
            dplyr::group_by_at(SL.library.stratify) %>%
            dplyr::do(compute.preds(.))          
          out <- out %>% (dplyr::ungroup) %>% dplyr::arrange(as.integer(rowname)) %>% dplyr::pull(out)
          return(out)
        }
      )
    } else if (!is.null(nlopt_method)) {
      nlopt_global <- c("NLOPT_GN_DIRECT",
                        "NLOPT_GN_DIRECT_L",
                        "NLOPT_GN_DIRECT_L_RAND",
                        "NLOPT_GN_DIRECT_NOSCAL",
                        "NLOPT_GN_DIRECT_L_NOSCAL",
                        "NLOPT_GN_DIRECT_L_RAND_NOSCAL",
                        "NLOPT_GN_ORIG_DIRECT",
                        "NLOPT_GN_ORIG_DIRECT_L",
                        "NLOPT_GN_CRS2_LM",
                        "NLOPT_GN_ISRES")
      nlopt_local <- c("NLOPT_LN_PRAXIS",
                       "NLOPT_LN_COBYLA",
                       "NLOPT_LN_NEWUOA_BOUND",
                       "NLOPT_LN_NELDERMEAD",
                       "NLOPT_LN_SBPLX",
                       "NLOPT_LN_BOBYQA")
      ## if (length(intersect(nlopt_method, c(nlopt_global, nlopt_local))) == 0) {
      if (!(nlopt_method %in% c(nlopt_global, nlopt_local))) {
        stop("User-supplied 'nlopt_method' value not supported.")
      }
      ## }
      out <- list(
        require = c("cvAUC", "nloptr", "magrittr", "tibble", "dplyr"),
        computeCoef = function(Z, Y, libraryNames, obsWeights, control, verbose, ...) {
          `%>%` <- magrittr::`%>%`
          if (length(setdiff(SL.library.stratify, libraryNames))) {
            stop("Argument 'SL.library.stratify' in call to 'method.stratifiedAUC' must be a subset of argument 'SL.library' in upstream call to 'SuperLearner::SuperLearner'.\n")
          }
          SL.library.stratify.idx <- grep(paste0(SL.library.stratify, collapse = "|"), libraryNames)
          ## cross-validated risks
          auc <- apply(Z, 2, function(x) cvAUC::AUC(predictions = x, labels = Y))
          ## If 'SuperLearner::getCoeff' was updated  to include 'folds', then
          ## we could use the below 'auc' instead:
          if (FALSE) {
            auc <- apply(Z, 2, function(x){cvAUC(x, labels=Y, folds = validRows)$cvAUC})
          }
          cvRisk <- 1 - auc  ## rank loss
          SL.library <- libraryNames[-SL.library.stratify.idx]
          names(cvRisk) <- SL.library
          ## stratified AUC-based optimization
          AUC_based_optim <- function(tbl) {
            .cvRisk_AUC <- function(par, Z, Y) {
              ## Calculate cvRisk, which is 1 - cvAUC (rank loss). This is the
              ## general loss  function that gets  fed into optim as  the 'fn'
              ## argument.   Argument 'par'  is the  "weight/coef" vector  for
              ## ensemble in Super Learner.
              predictions <- crossprod(t(Z), par)
              ## Now  calculate cvRisk  (this is  what we  want to  minimize).
              ## Remark: might change this to  AUC only since we are currently
              ## not using folds arg...
              cvRisk <- 1 - cvAUC::cvAUC(predictions = predictions, labels = Y, folds = NULL)$cvAUC
              return(cvRisk)
            }
            coef_init <- rep(1 / ncol(Z), ncol(Z))
            names(coef_init) <- libraryNames
            ## 'nloptr' function selects the value for par that minimizes .cvRisk_AUC (ie. rank loss)
            Z <- tbl %>% dplyr::select(SL.library) %>% as.matrix
            res <- nloptr::nloptr(x0 = coef_init,
                                  eval_f = .cvRisk_AUC,
                                  lb = rep(0, ncol(Z)),
                                  ub = rep(Inf, ncol(Z)),
                                  ## eval_g_ineq = .constraint_ineq,
                                  ## eval_g_eq = .constraint_eq,
                                  opts = list(algorithm = nlopt_method, xtol_rel = 1e-08),
                                  Z = Z,
                                  Y = dplyr::pull(tbl, Y))
            if (verbose) {
              if (res$status < 1 || res$status > 4) {
                warning(res$message)
              }
            }
            coef <- res$solution
            if (anyNA(coef)) {
              warning("Some algorithms have weights of NA, setting to 0.")
              coef[is.na(coef)] <- 0
            }
            if (!sum(coef) > 0) {
              warning("All algorithms have zero weight", call. = FALSE)
              coef <- coef_init
            }
            if (normalize) {
              coef <- coef / sum(coef)
            }
            names(coef) <- SL.library
            return(dplyr::bind_rows(coef))
          }
          data <- cbind(Y, Z, obsWeights)
          colnames(data) <- c("Y", libraryNames, "weights")
          fit.AUC <- data %>%
            (tibble::as_tibble) %>%
            dplyr::group_by_at(SL.library.stratify) %>%
            dplyr::do(AUC_based_optim(.))
          fit.AUC <- as.matrix(fit.AUC)
          idx <- grep(SL.library.stratify, colnames(fit.AUC))
          coef <- t(fit.AUC[, -idx])
          colnames(coef) <- fit.AUC[, idx]
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
          out <- predY %>% (tibble::as_tibble) %>%
            (tibble::rownames_to_column) %>%
            dplyr::inner_join(coef, by = SL.library.stratify) %>%
            dplyr::group_by_at(SL.library.stratify) %>%
            dplyr::do(compute.preds(.))          
          out <- out %>% (dplyr::ungroup) %>% dplyr::arrange(as.integer(rowname)) %>% dplyr::pull(out)
          return(out)
        }
      )
    } 
  }
  invisible(out)
}
environment(method.stratifiedAUC) <- asNamespace("SuperLearner")
