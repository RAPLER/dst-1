#' Compute qq from tt
#' 
#' qq is the commonality function as a set function from the subsets of the frame to \eqn{[0,1]}. To evaluate it, input a set encoded in binary vector, so the commonality number at that set can be returned.
#' 
#' @param tt Mass assignment set matrix
#' @param m Mass assignment
#' @param fzt = FALSE Whether to use Fast Zeta Transform
#' @return f Commonality function
#' @author Peiyuan Zhu
#' @export
#' @examples 
#' x <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, byrow = TRUE),
#' m = c(0.2,0.5, 0.3), cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
#' qq <- commonality(x$tt,x$spec[,2])
#' qq(c(1,0,0))
commonality <- function(tt,m,fzt=FALSE){
  if (fzt==FALSE) {
    f <- function(x) {
      q <- 0
      for (i in 1:nrow(tt)) {
        if (all(tt[i,] - x >= 0)) {
          q <- q + m[i]
        }
      }
      return(q)
    }
  } else {
    # Fast Zeta Transform
    m_seq <- rep(0, 2**ncol(tt))
    for (i in 1:nrow(tt)) {
      w <- decode(rep(2, ncol(tt)), tt[i,])
      m_seq[w + 1] <- m[i]
    }

    for (i in 1:ncol(tt)) {
      x <- rep(1,ncol(tt))
      x[i] <- 0
      for (j in 1:2**ncol(tt)) {
        y <- encode(rep(2, ncol(tt)), j - 1)
        z <- pmin(x,y)
        w <- decode(rep(2, ncol(tt)), z)
        if (!all(z==y)) {
          m_seq[w + 1] <- m_seq[j] + m_seq[w + 1]
        }
      }
    }
    
    f <- function(x) {
      w <- decode(rep(2, ncol(tt)), x)
      q <- m_seq[w + 1]
      return(q)
    }
  }
  return(f)
}