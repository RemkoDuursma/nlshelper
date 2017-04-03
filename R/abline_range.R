#'@title Add a line to a plot
#'@description As \code{abline}, but with \code{from} and \code{to} arguments.
#'If a fitted linear regression model is used as asn argument, it uses the min and max values of the data used to fit the model.
#'@param a Intercept (optional)
#'@param b Slope (optional)
#'@param reg A fitted linear regression model (output of \code{\link{lm}}).
#'@param from Draw from this X value
#'@param to Draw to this x value
#'@param \dots Further parameters passed to \code{\link{segments}}
#'@seealso See \code{\link{add_regres_line}} for adding a regression line with a confidence interval
#'@examples
#'
#'# Add a line manually
#'with(mtcars, plot(1/wt, mpg, xlim=c(0,0.8), ylim=c(0,40)))
#'abline_range(0,50,from=0.2, to=0.6)
#'
#'# Add a line across the range of the data from a regression object
#'with(mtcars, plot(1/wt, mpg, xlim=c(0,0.8), ylim=c(0,40)))
#'fit <- lm(mpg ~ I(1/wt), data=mtcars)
#'abline_range(fit)
#'
#'@export
#'@importFrom stats coefficients
#'@importFrom graphics segments
abline_range <- function(a=NULL,b=NULL,reg=NULL,from=NULL,to=NULL,...){
  
  # Borrowed from abline
  if (!is.null(reg)) a <- reg
  
  if (!is.null(a) && is.list(a)) {
    temp <- as.vector(coefficients(a))
    from <- min(a$model[,2], na.rm=TRUE)
    to <- max(a$model[,2], na.rm=TRUE)
    
    if (length(temp) == 1) {
      a <- 0
      b <- temp
    }
    else {
      a <- temp[1]
      b <- temp[2]
    }
  }
  
  segments(x0=from,x1=to,
           y0=a+from*b,y1=a+to*b,...)
  
}


