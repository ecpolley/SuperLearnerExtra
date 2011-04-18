# creates nnet wrappers in the global environment with different sizes. The default value for size in SL.nnet is 2

create.SL.nnet <- function(size = c(3, 4)) {
  for(mm in seq(length(size))) {
    eval(parse(text = paste('SL.nnet.', size[mm], '<- function(..., size = ', size[mm], ') SL.nnet(..., size = size)', sep = '')), envir = .GlobalEnv)
  }
  invisible(TRUE)
}