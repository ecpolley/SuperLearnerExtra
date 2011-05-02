# creates screen.corRank wrappers in the global environment with different rank value cut-offs. The default value for rank in screen.corRank is 2.

create.screen.corRank <- function(rank = c(10, 20, 30)) {
  for(mm in seq(length(rank))){
    eval(parse(text = paste('screen.corRank.', rank[mm], '<- function(..., rank = ', rank[mm], ') screen.corRank(..., rank = rank)', sep = '')), envir = .GlobalEnv)
  }
  invisible(TRUE)
}