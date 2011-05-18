create.SL.randomForest <- function(tune = list(mtry = c(1, 5, 10), nodesize = c(1, 5, 10))) {
  tuneGrid <- expand.grid(tune, stringsAsFactors = FALSE)
  for(mm in seq(nrow(tuneGrid))) { 
    eval(parse(file = "", text = paste("SL.randomForest.", mm, "<- function(..., mtry = ", tuneGrid[mm, 1], ", nodesize = ", tuneGrid[mm, 2], ") SL.randomForest(..., mtry = mtry, nodesize = nodesize)", sep = "")), envir = .GlobalEnv)
  }
  invisible(TRUE)
}