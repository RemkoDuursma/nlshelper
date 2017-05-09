#' Plot a non-linear or non-parametric regression model
#'
#' Convenient function for adding curves to an existing plot, or to plot the data with the fitted curve.
#' For non-linear regression plotting (\code{plot_nls}), works for simple non-linear regression models fit with \code{\link{nls}}, grouped non-linear regression (with \code{\link{nlsList}}), and non-linear quantile regression fit with \code{\link{nlrq}} from the quantreg package.
#' When plotting an \code{nlsList} object, \code{plot_nls} plots the fitted curve for each of the groups specified in the model, and sets colours with \code{lines.col} and \code{points.col} arguments.
#' For local regression models fitted with \code{loess}, use the \code{plot_loess} function, which additionally adds a confidence interval around the fitted curve (unless \code{band=FALSE}).
#' @param object The object returned by \code{nls}, \code{nlsList} or \code{loess}
#' @param col Colour to be used for the data symbols and the fitted line, unless \code{lines.col} and \code{points.col} are provided
#' @param band For \code{plot_loess}, whether to add a confidence band. Not yet implemented for \code{plot_nls}
#' @param plotdata Logical. Whether to add the data points to the plot.
#' @param extrapolate Logical (default FALSE). If TRUE, extends the fitted curve beyond the data, based on the x-axis limits specified.
#' @param lines.col Colour(s) for the fitted lines. When plotting a \code{nlsList} object, can be a vector that represents colours for each group.
#' @param points.col Colour(s) for the data symbols. When plotting a \code{nlsList} object, can be a vector that represents colours for each group.
#' @param ci.col Colour of the confidence band, if plotted. Defaults to a transparent grey colour.
#' @param lwd Thickness of the line (see \code{\link{par}})
#' @param lty Line type (see \code{\link{par}})
#' @param add Logical. Whether to add to current plot (default FALSE).
#' @param xlab Label for x-axis
#' @param ylab Label for y-axis
#' @param coverage If confidence band to be plotted, the coverage (e.g. for 95\% confidence interval, use 0.95)
#' @param xlim X-axis limits (optional).
#' @param \dots Further arguments passed to \code{\link{plot}}
#' @return Returns the predicted values used in plotting (invisibly), as a dataframe with columns 'predvar' (regularly spaced predictor values), and 'fit' (fitted values). For \code{plot_loess} also returns confidence intervals, standard error, and df of the residual.
#'@examples
#'
#'# Plot an nls object
#'chick <- as.data.frame(ChickWeight)
#'fit0 <- nls(weight ~ a*Time^b, data=chick, start=list(a=10, b=1.1))
#'plot_nls(fit0)
#'
#'# Plot a grouped nls object
#'library(nlme)
#'fit1 <- nlsList(weight ~ a*Time^b|Diet, data=chick, start=list(a=10, b=1.1))
#'plot_nls(fit1)
#'
#'# Plot a local regression object, with confidence interval
#'l <- loess(wt ~ disp, data=mtcars)
#'plot_loess(l)
#'
#'# To plot behind the data:
#'with(mtcars, plot(disp, wt, pch=19,
#'  panel.first=plot_loess(l, plotdata=FALSE)))
#'
#'@export
#'@importFrom graphics polygon
#'@rdname plot_nls
plot_nls <- function(object,
                     col=NULL,
                     band=TRUE,
                     plotdata=TRUE,
                     extrapolate=FALSE,
                     lines.col=palette(),
                     points.col=palette(),
                     ci.col="#BEBEBEB3",
                     lwd=1,
                     lty=1,
                     add=FALSE,
                     xlab=NULL,
                     ylab=NULL,
                     coverage=0.95,
                     xlim=NULL,
                     ...){

  if(!is.null(col)){
    lines.col <- col
    points.col <- col
  }

  type <- if(!plotdata)'n' else 'p'

  if(inherits(object, "nls") | inherits(object, "loess") | inherits(object, "nlrq")){


    pred <- predict_along(object, coverage=coverage,
                          xlim=if(extrapolate)xlim else NULL)
    data <- get_data(object)

    pff <- parse.formula(formula(object))
    predvar <- intersect(all.names(pff$rhs), names(data))
    respvar <- as.character(pff$lhs)

    if(is.null(xlab)) xlab <- predvar
    if(is.null(ylab)) ylab <- respvar

    if(!add)plot(data[,predvar], data[,respvar], col=points.col[1],
                 type=type,
                 xlab=xlab, ylab=ylab, xlim=xlim, ...)

    if(band && all(c("uci","lci") %in% names(pred))){
      with(pred, addpoly(predvar, lci, uci, col=ci.col))
    }

    with(pred, lines(predvar, fit, col=lines.col[1], lwd=lwd, lty=lty))


  }

  if(inherits(object, "nlsList")){

    pred <- predict_along_nlslist(object, xlim=if(extrapolate)xlim else NULL)
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
           type=type,
           col=points.col[as.factor(data[,groupvar])],
           xlab=xlab, ylab=ylab, ...)
    }
    for(i in seq_len(ngroup)){
      with(pred[[i]],
           lines(predvar, fit, col=lines.col[i], lwd=lwd, lty=lty)
      )
    }
  }


return(invisible(pred))
}


#'@export
#'@rdname plot_nls
plot_loess <- function(object, ...)plot_nls(object, ...)


# nls, nlsList, loess
get_data <- function(x){
  eval(summary(x)$call$data, parent.frame())
}


# nls, loess
predict_along <- function(object, n=101, coverage=0.95, xlim=NULL, ...){

  data <- get_data(object)
  pff <- parse.formula(formula(object))
  predvar <- intersect(all.names(pff$rhs), names(data))
  if(length(predvar) > 1)stop("This only works for one predictor.")

  if(is.null(xlim)){
    xlim <- c(min(data[,predvar], na.rm=TRUE),
              max(data[,predvar], na.rm=TRUE))
  }

  xv <- seq(xlim[1], xlim[2], length=n)

  preddf <- data.frame(xv)
  names(preddf) <- predvar

  if(inherits(object, "nls") | inherits(object, "nlrq")){
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
predict_along_nlslist <- function(object, n=101, xlim=NULL, ...){

  data <- get_data(object)
  pff <- parse.formula(formula(object))
  predvar <- intersect(all.names(pff$rhs), names(data))
  groupvar <- as.character(pff$condition)

  sp <- split(data, data[,groupvar])

  lapply(names(sp), function(x, ...){

    data <- sp[[x]]
    if(is.null(xlim)){
      xlim <- c(min(data[,predvar], na.rm=TRUE),
                 max(data[,predvar], na.rm=TRUE))
    }

    xv <- seq(xlim[1], xlim[2], length=n)

    preddf <- data.frame(xv)
    names(preddf) <- predvar
    data.frame(predvar=xv, fit = predict(object[[x]], newdata=preddf, ...))
  })
}






