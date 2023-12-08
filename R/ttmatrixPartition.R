#' Create partition matrix
#' 
#' @param n partition size
#' @param m size of the set to be partitioned
#' @return h binary partition matrix of size n by m
#' @author Peiyuan Zhu
#' @export
#' @examples 
#' # test singleton hypotheses
#' x <- bca(tt = matrix(c(1,1,0,1,1,1), nrow = 2, byrow = TRUE), m = c(0.8, 0.2), cnames = c(1,2,3))
#' pa <- ttmatrixPar(x$infovar[2], x$infovar[2])
#' belplau(x, h=pa)
ttmatrixPartition <- function(n, m) {
  # create block diagonal matrix
  h <- t(kronecker(diag(1, n), rep(1, round(m/n))))
  # match dimensions
  if (ncol(h) < m) {
    # expand matrix
    x1 <- matrix(0, n, m - ncol(h))
    x1[n, 1:(m - ncol(h))] <- 1
    h <- cbind(h, x1)
  } else {
    # shrink matrix
    h <- h[, 1:m]
  }
  return(h)
}
