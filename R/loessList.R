#' List of loess models
#' @description Like \code{\link{nlsList}}, but for \code{\link{loess}}. Supports only a single grouping variable, with a formula of the form "y ~ x|group".
#' @param model Formula of the form y ~ x|group
#' @param data A dataframe containing the three variables
#' @param \dots Further arguments passed to \code{\link{loess}}, such as \code{span} and \code{degree}.
#'@examples
#'data <- as.data.frame(Orthodont)
#'data$age <- jitter(data$age, 2)
#'#model <- distance ~ age | Sex
#'res <- loessList(distance ~ age | Sex, data=data)
#
loessList <- function(model, data, ...){
  
  # Code copied from nlsList.formula and simplified
  if (!is.data.frame(data)) 
    data <- as.data.frame(data)
  
  if (is.null(grpForm <- getGroupsFormula(model))) {
    stop("Formula must include a grouping variable (y ~ x | group)")
  } else {
    model <- eval(substitute(Y ~ RHS, list(Y = model[[2]], 
                                           RHS = getCovariateFormula(model)[[2]])))
    groups <- getGroups(data, form = grpForm)[drop = TRUE]
  }
  val <- lapply(split(data, groups), function(dat) tryCatch({
    loess(model, data=dat, ...)
  }, error = function(e) e))
  
  class(val) <- "loessList"
  
return(val)
}

