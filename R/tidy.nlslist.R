#' Tidy method for nlsList
#'
#' Adds a method to \code{\link{tidy}} (broom package), so that we can use it for models fitted with \code{\link{nlsList}}.
#'
#' @param x An object returned by \code{\link{nlsList}}
#' @param conf.int Whether to calculate confidence intervals
#' @param conf.level The level of the confidence interval
#' @param quick If TRUE, only returns the coefficients.
#' @param \dots Further arguments passed to \code{\link{tidy}}
#' @examples
#' chick <- as.data.frame(ChickWeight)
#'
#' # Fit an nlsList model, with a grouping variable (Diet)
#' fit1 <- nlsList(weight ~ a*Time^b | Diet, data=chick, start=list(a=10, b=1.1))
#'
#' # Collect coefficients
#' tidy(fit1)
#'
#' # ... and confidence intervals
#' tidy(fit1, conf.int=TRUE)
#'@importFrom broom tidy
#'@importFrom dplyr bind_rows
#'@export
tidy.nlsList <- function(x, conf.int = FALSE, conf.level = .95,
                         quick = FALSE, ...) {

  l <- lapply(names(x), function(n, ...){
    cbind(group=n, tidy(x[[n]],
                        conf.int = conf.int,
                        conf.level = conf.level,
                        quick = quick, ...), stringsAsFactors=FALSE)
  })
  bind_rows(l)

}
