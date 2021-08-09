#' gen.cont.table.data
#' 
#' This function generates data for problems that use contingency tables
#' @param n sample size
#' @param A vector of values of first categorical variable
#' @param B vector of values of second categorical variable
#' @param tbl should output be a table
#' @param rho correlation between A and B
#' @return A matrix with two columns
#' @export
#' @examples
#' gen.cont.table.data(10, c("a", "b"), 1:3, rho=0.9)

gen.cont.table.data <-
function(n, A, B, tbl = FALSE, rho) {
  k <- length(A)
  m <- length(B)
  dta <- mvtnorm::rmvnorm(n, sigma = matrix(c(1,rho,rho,1),2,2))
  x <- dta[,1]
  y <- dta[,2]
  xr <- stats::quantile(x, c(0:k)/k)
  yr <- stats::quantile(y, c(0:m)/m)
  u <- rep(A[1], n)
  for(i in 2:k) u[x >= xr[i]] <- A[i]
  v <- rep(B[1], n)
  for(i in 2:m) v[y >= yr[i]] <- B[i]
  if(tbl) return(table(u, v))
  cbind( u, v )
}
