#' Add a regression line and confidence band to a plot
#' 
#' Plots a regression line from a simple linear model (of the form \code{lm(y ~ x)}) to a plot. Also plots the confidence band for the mean, which is calculated using \code{\link{predict.lm}}.
#'@param fit Object returned by lm. Only models of the form \code{y ~ x} are supported, without expressions in \code{I()} (see Examples), or interactions, or multiple variables.
#'@param from Optional (read from fitted model); Draw from this X value.
#'@param to Optional (read from fitted model); Draw to this x value.
#'@param band Logical. Whether to add a confidence band.
#'@param ci.col Colour of the confidence band, if plotted. Defaults to a transparent grey colour.
#'@param \dots Further arguments passed to \code{\link{abline_range}}
#'@examples
#'
#'#'Add a line across the range of the data from a regression object
#'with(mtcars, plot(1/wt, mpg, xlim=c(0,0.8), ylim=c(0,40)))
#'
#'# add_regres_line does not allow I() expressions; yet.
#'mtcars$inv_wt <- 1 / mtcars$wt
#'fit <- lm(mpg ~ inv_wt, data=mtcars)
#'add_regres_line(fit)
#'
#'# Add the regression line and confidence band behind the data
#'fit <- lm(height ~ age, data=Loblolly)
#'with(Loblolly, plot(age, height, pch=19, panel.first=add_regres_line(fit)))
#'@export
#'@importFrom stats coef
#'@importFrom stats predict
add_regres_line <- function(fit, from=NULL, to=NULL, band=TRUE, ci.col="#BEBEBEB3",...){
  
  if(is.null(from))from <- min(fit$model[,2], na.rm=TRUE)
  if(is.null(to))to <- max(fit$model[,2], na.rm=TRUE)
  
  newdat <- data.frame(X = seq(from,to, length=101))
  names(newdat)[1] <- names(coef(fit))[2]
  
  pred <- as.data.frame(predict(fit, newdat, se.fit=TRUE, interval="confidence")$fit)
  
  if(band)addpoly(newdat[[1]], pred$lwr, pred$upr, col=ci.col)
  abline_range(fit, from=from, to=to, ...)
  
}

