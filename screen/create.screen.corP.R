# creates screen.corP wrappers in the global environment with different p value cut-offs. The default value for minPvalue in screen.corP is 0.1

create.screen.corP <- function(minPvalue = c(0.05, 0.025, 0.01)) {
  for(mm in seq(length(minPvalue))){
    eval(parse(text = paste('screen.corP.', minPvalue[mm], '<- function(..., minPvalue = ', minPvalue[mm], ') screen.corP(..., minPvalue = minPvalue)', sep = '')), envir = .GlobalEnv)
  }
  invisible(TRUE)
}