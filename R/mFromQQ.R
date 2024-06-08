#' Construct a mass vector from qq function.
#' 
#' @param qq Commonality function
#' @param tt logical description matrix from ttmatrixFromQQ
#' @return m A corresponding mass vector
#' @author Peiyuan Zhu
#' @export
#' @examples 
#' tt<- t(matrix(c(1,0,1,1),ncol = 2))
#' m<- c(.9,.1)
#' cnames <- c("yes","no")
#' x<- bca(tt, m, cnames=cnames)
#' mFromQQ(x$qq, x$tt)
mFromQQ <- function(qq, tt) {
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
  
  m <- vector()
  i <- 0
  for (i in 1:nrow(tt)) {
    m0 <- mobiusInvHQQ(qq,tt[i,])
    if (m0 > 0) {
      m <- c(m, m0)
    }
    i <- i + 1
  }
  
  return(m)
}