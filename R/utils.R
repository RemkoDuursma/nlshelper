parse.formula <- function (formula, ...)
{
  op <- formula[[1]]
  condition <- NULL
  if (length(formula) == 2) {
    rhs <- formula[[2]]
    lhs <- NULL
  }
  else if (length(formula) == 3) {
    rhs <- formula[[3]]
    lhs <- formula[[2]]
  }
  else {
    stop("Invalid formula type.")
  }
  if (inherits(rhs, "call") && rhs[[1]] == "|") {
    condition <- rhs[[3]]
    rhs <- rhs[[2]]
  }

list(op = op, lhs = lhs, rhs = rhs, condition = condition)
}

addpoly <- function(x,y1,y2,col="#BEBEBEB3",...){
  ii <- order(x)
  y1 <- y1[ii]
  y2 <- y2[ii]
  x <- x[ii]
  polygon(c(x,rev(x)), c(y1, rev(y2)), col=col, border=NA,...)
}

recycle <- function(x,n){
  len <- length(x)
  rep(x, ceiling(n/len))[1:n]
}

set_cols <- function(cols1, cols2, pal){
  
  if(is.null(cols1))cols1 <- cols2
  if(is.null(cols2))cols2 <- cols1
  if(is.null(cols1) && is.null(cols2)){
    cols1 <- cols2 <- pal
  }
  return(list(cols1=cols1, cols2=cols2))
}

