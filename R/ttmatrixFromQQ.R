#' Construct a description matrix from qq function.
#' 
#' @param qq Commonality function
#' @param n Dimension of the frame
#' @param valuenames Vector of valuenames
#' @return ttmat A corresponding logical description matrix 
#' @author Peiyuan Zhu
#' @export
#' @examples 
#' tt<- t(matrix(c(1,0,1,1),ncol = 2))
#' m<- c(.9,.1)
#' cnames <- c("yes","no")
#' x<- bca(tt, m, cnames=cnames)
#' ttmatrixFromQQ(x$qq,as.integer(x$infovar[1,2]), unlist(x$valuenames))
ttmatrixFromQQ <- function(qq, n, valuenames) {
  # Obtain tt matrix from commonality function
  #
  # Checks
  # 1. Check that the input is a function
  if (is.function(qq) == FALSE) {
    stop("Input qq must be a function.")
  }
  
  # 2. Check that the input n is an integer
  if (is.integer(n) == FALSE) {
    stop("Input n must be an integer.")
  }
  
  # 2. Check that the input valuenames is a vector
  if (is.vector(valuenames) == FALSE) {
    stop("Input valuenames must be a vector")
  }
  
  ttmat <- c()
  i <- 0
  while (i <= (2**n - 1)) {
    x <- encode(rep(2, n), i) 
    m <- mobiusInvHQQ(qq,x)
    if (m > 0) {
      ttmat <- rbind(ttmat, x)
    }
    i <- i + 1
  }
  
  colnames(ttmat) <- valuenames
  rownames(ttmat) <- nameRows(ttmat)
  
  sort_order <-order(apply(ttmat,1,sum))
  ttmat <- ttmat[sort_order,]
  return(ttmat)
}