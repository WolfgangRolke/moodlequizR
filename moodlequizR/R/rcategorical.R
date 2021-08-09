#' rcategorical
#' 
#' This function generates data from a univariate or a bivariate discrete distribution
#' @param n sample size
#' @param p  vector or matrix of values
#' @return a vector or a matrix
#' @export
#' @examples
#' p=1:3
#' names(p)=letters[1:3]
#' x=rcategorical(1000, p)
#' p=matrix(1:6, 2, 3)
#' dimnames(p)=list(c("A","B"), letters[1:3])
#' x=rcategorical(1000, p)

rcategorical = function(n, p) {
  if(is.null(dim(p))) 
    return(sample(names(p), size=n, replace=TRUE, prob=p))
  nr=nrow(p)
  nc=ncol(p)
  z=sample(1:(nr*nc), size=n, replace=TRUE, prob=c(p))
  x=rownames(p)[rep(1:nr,nc)[z]]
  y=colnames(p)[rep(1:nc,each=nr)[z]]
  x=factor(x, levels=rownames(p), ordered=TRUE)
  y=factor(y, levels=colnames(p), ordered=TRUE)
  data.frame(x=x, y=y)
}