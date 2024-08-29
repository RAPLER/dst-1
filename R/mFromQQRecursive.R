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
  
  # Fast Mobius Transform
  m_seq <- rep(0, 2**ncol(tt))
  for (i in 1:nrow(tt)) {
    w <- encode(rep(2, ncol(tt)), i)
    m_seq[i] <- qq(w)
  }
  
  for (i in 1:ncol(tt)) {
    x <- rep(1,ncol(tt))
    x[i] <- 0
    for (j in 1:2**ncol(tt)) {
      y <- encode(rep(2, ncol(tt)), j - 1)
      z <- pmin(x,y)
      w <- decode(rep(2, ncol(tt)), z)
      if (!all(z==y)) {
        m_seq[w + 1] <- m_seq[w + 1] - m_seq[j]
      }
    }
  }
  
  # Calculate mass function
  m <- rep(0,nrow(tt))
  for (i in 1:nrow(tt)) {
    w <- decode(rep(2, ncol(tt)),tt[i,])
    m[i] <- m_seq[w]
  }
  
  return(m)
}