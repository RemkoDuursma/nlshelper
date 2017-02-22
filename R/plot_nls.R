#' Plot a non-linear regression model
#'
#' Convenient function for adding curves to an existing plot, or to plot the data used in fitting with the curve overlaid. Works for simple non-linear regression models fit with \code{\link{nls}}, and grouped non-linear regression (with \code{\link{nlsList}}).
#'@examples
#'
#'chick <- as.data.frame(ChickWeight)
#'fit0 <- nls(weight ~ a*Time^b, data=chick, start=list(a=10, b=1.1))
#'fit1 <- nlsList(weight ~ a*Time^b|Diet, data=chick, start=list(a=10, b=1.1))
#'
#'plot_nls(fit0)
#'@export
plot_nls <- function(object,
                     lines.col=palette(),
                     points.col=palette(),
                     add=FALSE,
                     ...){

  if(inherits(object, "nls")){
    pred <- predict_along(object)
    data <- get_data_nls(object)

    pff <- parse.formula(formula(object))
    predvar <- intersect(all.names(pff$rhs), names(data))
    respvar <- as.character(pff$lhs)

    if(!add)plot(data[,predvar], data[,respvar], col=points.col[1], ...)
    with(pred, lines(predvar, fit, col=lines.col[1]))
  }


}



get_data_nls <- function(x){
  eval(summary(x)$call$data, parent.frame())
}



predict_along <- function(object, n=101, ...){

  data <- get_data_nls(object)
  pff <- parse.formula(formula(object))
  predvar <- intersect(all.names(pff$rhs), names(data))
  if(length(predvar) > 1)stop("This only works for one predictor.")

  xv <- seq(min(data[,predvar], na.rm=TRUE),
            max(data[,predvar], na.rm=TRUE),
            length=n)

  preddf <- data.frame(xv)
  names(preddf) <- predvar
  data.frame(predvar=xv, fit = predict(object, newdata=preddf, ...))
}













