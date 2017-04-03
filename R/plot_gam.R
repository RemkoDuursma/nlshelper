#' Plot a generalized additive model
#' 
#' This is a simple wrapper to fit and plot a basic type of generalized additive model. The fitted model is of the form \code{gam(Y ~ s(X, k))}, which can be fitted by a specified grouping variable (using the \code{g} argument). Also supported is an optional random effect, in which case the model fitted is \code{gamm(Y ~ s(X, k=k), random = list(R=~1), data=dfr)}. 
#' 
#' In either case, the \code{k} parameter necessary for the GAM fit can be set using the \code{kgam} argument. See\code{\link{choose.k}} for details on this setting (it is important you don't just use the default value here!).
#' @param x Variable for X axis (unquoted)
#' @param y Variable for Y axis (unquoted)
#' @param g Variable for grouping (unquoted); optional
#' @param data Dataframe containing x and y
#' @param fittype Either 'gam' (default), or 'lm' in which case a simple linear model is fit - useful for comparison.
#' @param kgam the \code{k} parameter for smooth terms in gam.
#' @param R An optional random effect (quoted)
#' @param log Whether to add log axes for x or y (but no transformations are done).
#' @param axes Logical (default TRUE), whether to add axes to the plot.
#' @param band Logical. If true, plots the confidence band (as a transparent polygon).
#' @param add Logical (default FALSE), if TRUE, adds to an existing plot.
#' @param fitoneline Whether to fit only one curve to the entire dataset, regardless of whether a grouping variable was defined. Default FALSE.
#' @param points.col Colours of the points, can be a vector (one value for each group, if present).
#' @param lines.col Colours of the lines, can be a vector (one value for each group, if present).
#' @param ci.col Colour of the confidence band, if plotted. Defaults to a transparent grey colour.
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param lwd Line thickness (see \code{\link{par}})
#' @param npred Number of x values to use for prediction
#' @param plotit Logical (default TRUE); if FALSE, suppresses the plot.
#' @param \dots Further arguments passed to \code{plot} or \code{points}, for example to change colour of plotting symbols.
#' @examples
#' data(Loblolly)
#' plot_gam(age, height, data=Loblolly)
#' plot_gam(age, height, Seed, data=Loblolly, band=FALSE, lines.col="black")
#' plot_gam(age, height, Seed, data=Loblolly, band=FALSE, lines.col="black", fittype="lm")
#' 
#' data(ChickWeight)
#' plot_gam(Time, weight, Diet, R="Chick", data=ChickWeight, lines.col=rainbow(4))
#' @importFrom mgcv gam
#' @importFrom mgcv gamm
#' @importFrom graphics axis.Date
#' @importFrom grDevices palette
#' @importFrom magicaxis magaxis
#' @importFrom stats lm
#' @importFrom graphics axis
#' @importFrom graphics box
#' @importFrom graphics plot
#' @importFrom graphics lines
#' @export
plot_gam <- function(x,y,g=NULL,data,
                       fittype=c("gam","lm"),
                       kgam=4,
                       R=NULL,
                       log="",
                       axes=TRUE,
                       fitoneline=FALSE,
                       points.col=NULL,
                       lines.col=NULL, 
                       ci.col="#D3D3D3B3",
                       xlab=NULL, 
                       ylab=NULL,
                       band=TRUE,
                       plotit=TRUE, add=FALSE,
                       npred=101,
                       lwd=2,
                       ...){

  fittype <- match.arg(fittype)

  if(!is.null(substitute(g))){
    data$G <- as.factor(eval(substitute(g),data))
  } else {
    fitoneline <- TRUE
    data$G <- 1
  }
  data$X <- eval(substitute(x),data)
  data$Y <- eval(substitute(y),data)
  
  data <- droplevels(data, except=which(sapply(data, is.ordered)))
  
  data <- data[!is.na(data$X) & !is.na(data$Y) & !is.na(data$G),]
  nlev <- length(unique(data$G))
  
  # colours
  bandcolor <- recycle(ci.col, nlev)
  linecols <- recycle(lines.col, nlev)
  pointcols <- recycle(points.col, nlev)
  
  cl <- set_cols(pointcols, linecols, palette())
  pointcols <- cl$cols1
  linecols <- cl$cols2
  
  
  if(class(data$X) == "Date"){
    xDate <- TRUE
    data$X <- as.numeric(data$X)
  } else {
    xDate <- FALSE
  }
  
  if(is.null(xlab))xlab <- substitute(x)
  if(is.null(ylab))ylab <- substitute(y)
  
  if(!fitoneline){
    
    d <- split(data, data$G)
    
    if(fittype == "gam"){
      fits <- lapply(d, function(x)try(fitgam("X","Y",x, k=kgam, R=R)))
      if(!is.null(R))fits <- lapply(fits, "[[", "gam")
    } else {
      fits <- lapply(d, function(x)lm(Y ~ X, data=x))
    }
    hran <- lapply(d, function(x)range(x$X, na.rm=TRUE))
  } else {
    if(fittype == "gam"){
      fits <- list(fitgam("X","Y",data, k=kgam, R=R))
      if(!is.null(R))fits <- lapply(fits, "[[", "gam")
    } else {
      fits <- list(lm(Y ~ X, data=data))
    }
    hran <- list(range(data$X, na.rm=TRUE))
    
  }
  
  if(plotit){
    if(xDate){
      data$X <- as.Date(data$X, origin="1970-1-1")
    }
    
    if(!add){
      with(data, plot(X, Y, axes=FALSE, pch=16, col=pointcols[G],
                      xlab=xlab, ylab=ylab, ...))
    } else {
      with(data, points(X, Y, pch=16, col=pointcols[G],
                        ...))
    }
    
    if(!add && axes){
      if(log=="xy")magaxis(side=1:2, unlog=1:2)
      if(log=="x"){
        magaxis(side=1, unlog=1)
        axis(2)
      }
      if(log=="y"){
        magaxis(side=2, unlog=2)
        axis(1)
      }
      if(log==""){
        if(xDate)
          axis.Date(1, data$X)
        else
          axis(1)
        axis(2)
      }
    }
    
    for(i in 1:length(fits)){
      
      if(fittype == "gam"){
        nd <- data.frame(X=seq(hran[[i]][1], hran[[i]][2], length=npred))
        if(!inherits(fits[[i]], "try-error")){
          p <- predict(fits[[i]],nd,se.fit=TRUE)
          if(band)addpoly(nd$X, p$fit-2*p$se.fit, p$fit+2*p$se.fit, col=bandcolor[i])
          lines(nd$X, p$fit, col=linecols[i], lwd=lwd)
          box()
        }
      }
      if(fittype == "lm"){
        pval <- summary(fits[[i]])$coefficients[2,4]
        LTY <- if(pval < 0.05)1 else 5
        add_regres_line(fits[[i]], col=linecols[i], lwd=lwd, lty=LTY, band=band)
        box()
      }
    }
  }
  return(invisible(fits))
}





