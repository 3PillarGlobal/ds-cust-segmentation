# path <- "/home/vikask/r/DataAudit"
# setwd(path)
# source("show_outliers_uv.R")

remove_outliers_func <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  #qnt[1] + H y
  return(y)
  
}
