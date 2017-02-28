#' Add a regression line and confidence interval to a plot
#'@importFrom scales alpha
#'@export
#'@examples
#'fit <- lm(height ~ age, data=Loblolly)
#'with(Loblolly, plot(age, height, panel.first=add_regres_line(fit)))
add_regres_line <- function(fit, from=NULL, to=NULL, band=TRUE, bandcolor=alpha("lightgrey",0.8),...){
  
  if(is.null(from))from <- min(fit$model[,2], na.rm=TRUE)
  if(is.null(to))to <- max(fit$model[,2], na.rm=TRUE)
  
  newdat <- data.frame(X = seq(from,to, length=101))
  names(newdat)[1] <- names(coef(fit))[2]
  
  pred <- as.data.frame(predict(fit, newdat, se.fit=TRUE, interval="confidence")$fit)
  
  if(band)addpoly(newdat[[1]], pred$lwr, pred$upr, col=bandcolor)
  abline_range(fit, from=from, to=to, ...)
  
}

