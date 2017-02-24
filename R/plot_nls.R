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
                     col=NULL,
                     lines.col=palette(),
                     points.col=palette(),
                     lwd=1,
                     lty=1,
                     add=FALSE,
                     xlab=NULL,
                     ylab=NULL,
                     ...){

  if(!is.null(col)){
    lines.col <- col
    points.col <- col
  }
  
  if(inherits(object, "nls")){
    pred <- predict_along(object)
    data <- get_data_nls(object)

    pff <- parse.formula(formula(object))
    predvar <- intersect(all.names(pff$rhs), names(data))
    respvar <- as.character(pff$lhs)

    if(is.null(xlab)) xlab <- predvar
    if(is.null(ylab)) ylab <- respvar
    
    if(!add)plot(data[,predvar], data[,respvar], col=points.col[1], xlab=xlab, ylab=ylab, ...)
    with(pred, lines(predvar, fit, col=lines.col[1], lwd=lwd, lty=lty))
  }

  if(inherits(object, "nlsList")){
    
    pred <- predict_along_nlslist(object)
    data <- get_data_nls(object)
    
    pff <- parse.formula(formula(object))
    predvar <- intersect(all.names(pff$rhs), names(data))
    respvar <- as.character(pff$lhs)
    groupvar <- as.character(pff$condition)
    ngroup <- length(unique(data[,groupvar]))
    
    if(is.null(xlab)) xlab <- predvar
    if(is.null(ylab)) ylab <- respvar
    
    if(!add){
      plot(data[,predvar], data[,respvar], 
           col=points.col[as.factor(data[,groupvar])], 
           xlab=xlab, ylab=ylab, ...)
    }
    for(i in seq_len(ngroup)){
      with(pred[[i]], 
           lines(predvar, fit, col=lines.col[i], lwd=lwd, lty=lty)
      )
    }
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

predict_along_nlslist <- function(object, n=101, ...){
  
  data <- get_data_nls(object)
  pff <- parse.formula(formula(object))
  predvar <- intersect(all.names(pff$rhs), names(data))
  groupvar <- as.character(pff$condition)
  
  sp <- split(data, data[,groupvar])
  
  lapply(names(sp), function(x, ...){
    
    data <- sp[[x]]
    xv <- seq(min(data[,predvar], na.rm=TRUE),
              max(data[,predvar], na.rm=TRUE),
              length=n)
    preddf <- data.frame(xv)
    names(preddf) <- predvar
    data.frame(predvar=xv, fit = predict(object[[x]], newdata=preddf, ...))
  })
}






