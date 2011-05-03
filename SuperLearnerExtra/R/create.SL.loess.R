# creates loess wrappers in the global environment with different spans. The default value for span in SL.loess is 0.75

create.SL.loess <- function(span = c(0.25, 0.50)) {
  for(mm in seq(length(span))) {
    eval(parse(text = paste('SL.loess.', span[mm], '<- function(..., span = ', span[mm], ') SL.loess(..., span = span)', sep = '')), envir = .GlobalEnv)
  }
  invisible(TRUE)
}