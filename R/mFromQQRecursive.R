#' Construct a mass vector from qq function and ttmatrix of focal elements recursively.
#' 
#' @param qq Commonality function
#' @param tt logical description matrix of focal elements from ttmatrixFromTT
#' @return m A corresponding mass vector
#' @author Peiyuan Zhu
#' @export
#' @examples 
#' tt<- t(matrix(c(1,0,1,1),ncol = 2))
#' m<- c(.9,.1)
#' cnames <- c("yes","no")
#' x<- bca(tt, m, cnames=cnames)
#' mFromQQ(x$qq, x$tt)
mFromQQRecursive <- function(qq, tt) {
  # Obtain tt matrix from commonality function
  #
  # Checks
  # 1. Check that the input qq is a function
  if (is.function(qq) == FALSE) {
    stop("Input qq must be a function.")
  }
  
  # 2. Check that the input tt is a matrix
  if (is.matrix(tt) == FALSE) {
    stop("Input tt must be a matrix")
  }
  
  # Calculate cardinality for each row of tt
  n <- rowSums(tt)
  
  # Calculate commonality function for each row of tt
  q <- rep(0,nrow(tt))
  for (i in 1:nrow(tt)) {
    q[i] <- qq(tt[i,])
  }
  
  # Calculate mass function
  m <- rep(0,nrow(tt))
  for (i in nrow(tt):1) {
    m[i] <- q[i] + sum((-1) ** (n[n > n[i]] - n[i]) * q[n > n[i]])
  }
  
  return(m)
}