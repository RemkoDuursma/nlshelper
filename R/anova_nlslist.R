#' Anova for nlsList
#'
#' Applies an F-test to a non-linear regression model that includes a grouping variable (fit with \code{\link{nlsList}}), comparing it to a model without a grouping variable. This is a convenient way to test whether there is an overall effect of the grouping variable on the non-linear relationship.
#'
#' @param nlsfull The full model, an object returned by \code{\link{nlsList}}
#' @param nlsreduc The reduced model, which is identical to the full model except the grouping variable has been removed, and it was fit with \code{\link{nls}}
#' @examples
#' chick <- as.data.frame(ChickWeight)
#'
#' # Fit a simple model with nls
#' fit0 <- nls(weight ~ a*Time^b, data=chick, start=list(a=10, b=1.1))
#'
#' # Fit an nlsList model, with a grouping variable (Diet)
#' fit1 <- nlsList(weight ~ a*Time^b | Diet, data=chick, start=list(a=10, b=1.1))
#'
#' # Using an F-test, test whether the fit is significantly better when adding
#' # a grouping variable
#' anova_nlslist(fit1, fit0)
#'@export
anova_nlslist <- function(nlsfull, nlsreduc){

  if(!inherits(nlsfull, "nlsList"))stop("First argument must be fit with nlsList.")
  if(!inherits(nlsreduc, "nls"))stop("Second argument must be fit with nls.")

  if(!summary(nlsfull)$pool)warning("F test against model without a grouping term will assume pooled variance (even though you specified pool=FALSE).")

  get_df <- function(x){
    attr(pooledSD(x), 'df')
  }

  # SSE and df of full model
  ssf <- sum(residuals(nlsfull)^2)
  dff <- get_df(nlsfull)

  # SSE and df of reduced model
  ssr <- sum(residuals(nlsreduc)^2)  # =deviance(fit0)
  dfr <- df.residual(nlsreduc)


  df.r <- c(dfr, dff)
  ss.r <- c(ssr, ssf)

  df <- c(NA, dfr - dff)
  ss <- c(NA, ssr - ssf)
  f <- c(NA, ((ssr - ssf)/(dfr - dff)) / (ssf/dff))
  p <- pf(f, df, dfr, lower.tail=FALSE)

  models <- as.character(c(formula(nlsreduc), formula(nlsfull)))

  table <- data.frame(df.r, ss.r, df, ss, f, p)
  dimnames(table) <- list(1L:2, c("Res.Df", "Res.Sum Sq",
                                  "Df", "Sum Sq", "F value", "Pr(>F)"))
  title <- "Analysis of Variance Table\n"
  topnote <- paste("Model ", format(1L:2), ": ", models,
                   sep = "", collapse = "\n")
  structure(table, heading = c(title, topnote), class = c("anova",
                                                          "data.frame"))

}


