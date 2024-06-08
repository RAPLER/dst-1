#' Compute qq from tt
#' 
#' qq is the commonality function as a set function from the subsets of the frame to \eqn{[0,1]}. To evaluate it, input a set encoded in binary vector, so the commonality number at that set can be returned.
#' 
#' @param tt Mass assignment set matrix
#' @param m Mass assignment
#' @return f Commonality function
#' @author Peiyuan Zhu
#' @export
#' @examples 
#' x <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, byrow = TRUE),
#' m = c(0.2,0.5, 0.3), cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
#' qq <- commonality(x$tt,x$spec[,2])
#' qq(c(1,0,0))
commonality <- function(tt,m){
  f <- function(x) {
    q <- 0
    for (i in 1:nrow(tt)) {
      if (all(tt[i,] - x >= 0)) {
        q <- q + m[i]
      }
    }
    return(q)
  }
  return(f)
}