#' Construct a mass vector from qq function and ttmatrix of focal elements recursively.
#' 
#' @param qq Commonality function
#' @param n Frame dimension
#' @param method = NULL: Use Fast Zeta Transform ("fzt") or Efficient Zeta Transform ("ezt")
#' @return m A corresponding mass vector
#' @author Peiyuan Zhu
#' @export
#' @examples 
#' tt<- t(matrix(c(1,0,1,1),ncol = 2))
#' m<- c(.9,.1)
#' cnames <- c("yes","no")
#' x<- bca(tt, m, cnames=cnames)
#' mFromQQ(x$qq, x$tt)
mFromQQRecursive <- function(qq,n,method = NULL) {
  # Obtain tt matrix from commonality function
  #
  # 1. Check that the input qq is a function
  if (is.function(qq) == FALSE) {
    stop("Input qq must be a function.")
  }
  
  if (method=="fmt") {
    # Fast Mobius Transform
    m_seq <- rep(0, 2**n)
    for (i in 1:length(m_seq)) {
      w <- encode(rep(2, n), i - 1)
      m_seq[i] <- qq(w)
    }
    
    for (i in 1:n) {
      x <- rep(1,n)
      x[i] <- 0
      for (j in 1:2**n) {
        y <- encode(rep(2, n), j - 1)
        z <- pmin(x,y)
        w <- decode(rep(2, n), z)
        if (!all(z==y)) {
          m_seq[w + 1] <- m_seq[w + 1] - m_seq[j]
        }
      }
    }
    
    return(m_seq)
  } else if (method == "emt") {
    
    # TODO: finish this
  }
}