#' Plot a non-linear or non-parametric regression model
#'
#' Convenient function for adding curves to an existing plot, or to plot the data used in fitting with the curve overlaid. Works for simple non-linear regression models fit with \code{\link{nls}}, and grouped non-linear regression (with \code{\link{nlsList}}).
#'@examples
#'
#'chick <- as.data.frame(ChickWeight)
#'fit0 <- nls(weight ~ a*Time^b, data=chick, start=list(a=10, b=1.1))
#'plot_nls(fit0)
#'
#'fit1 <- nlsList(weight ~ a*Time^b|Diet, data=chick, start=list(a=10, b=1.1))
#'plot_nlslist(fit1)
#'
#'@export
#'@rdname plot_nls
plot_nls <- function(object,
                     col=NULL,
                     lines.col=palette(),
                     points.col=palette(),
                     ci.col="#BEBEBEB3",
                     lwd=1,
                     lty=1,
                     add=FALSE,
                     xlab=NULL,
                     ylab=NULL,
                     coverage=0.95,
                     ...){

  if(!is.null(col)){
    lines.col <- col
    points.col <- col
  }
  
  if(inherits(object, "nls") | inherits(object, "loess")){
    
    pred <- predict_along(object, coverage=coverage)
    
    data <- get_data(object)

    pff <- parse.formula(formula(object))
    predvar <- intersect(all.names(pff$rhs), names(data))
    respvar <- as.character(pff$lhs)

    if(is.null(xlab)) xlab <- predvar
    if(is.null(ylab)) ylab <- respvar
    
    
    if(!add)plot(data[,predvar], data[,respvar], col=points.col[1], xlab=xlab, ylab=ylab, ...)
    
    if(all(c("uci","lci") %in% names(x))){
      with(pred, addpoly(predvar, lci, uci, col=ci.col))
    }
    
    with(pred, lines(predvar, fit, col=lines.col[1], lwd=lwd, lty=lty))
    

  }

  if(inherits(object, "nlsList")){
    
    pred <- predict_along_nlslist(object)
    data <- get_data(object)
    
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


#'@export
#'@rdname plot_nls
plot_loess <- function(object, ...)plot_nls(object, ...)


# nls, nlsList, loess
get_data <- function(x){
  eval(summary(x)$call$data, parent.frame())
}


# nls, loess
predict_along <- function(object, n=101, coverage=0.95, ...){

  data <- get_data(object)
  pff <- parse.formula(formula(object))
  predvar <- intersect(all.names(pff$rhs), names(data))
  if(length(predvar) > 1)stop("This only works for one predictor.")

  xv <- seq(min(data[,predvar], na.rm=TRUE),
            max(data[,predvar], na.rm=TRUE),
            length=n)

  preddf <- data.frame(xv)
  names(preddf) <- predvar
  
  if(inherits(object, "nls")){
    return(data.frame(predvar=xv, fit = predict(object, newdata=preddf, ...)))
  }
  if(inherits(object, "loess")){
    
    alpha <- 1 - coverage
    qv <- coverage + alpha/2
    
    pred <- as.data.frame(predict(object, newdata=preddf, se=TRUE, ...))
    pred$lci <- with(pred, fit - qt(qv, df)*se.fit)
    pred$uci <- with(pred, fit + qt(qv, df)*se.fit)
    pred <- cbind(predvar=xv, pred)
    return(pred)
  }

}

# nlsList
predict_along_nlslist <- function(object, n=101, ...){
  
  data <- get_data(object)
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






