#' Construct a description matrix from a qq vector
#' 
#' @param qq Commonality function
#' @param n Dimension of the frame
#' @param cnames A character vector containing the names of the elements of the frame of discernment
#' @return tt A corresponding logical description matrix 
#' @author Peiyuan Zhu
#' @export
#' @examples 
#' tt<- t(matrix(c(1,0,1,1),ncol = 2))
#' m<- c(.9,.1)
#' cnames <- c("yes","no")
#' x<- bca(tt, m, cnames=cnames, method="ezt-m")
#' ttmatrixFromQQ(x$qq,as.integer(x$infovar[1,2]), cnames)
ttmatrixFromQQ <- function(qq, n, cnames) {
  # Obtain tt matrix from commonality function
  #
  # Checks
  # 1. Check that the input is a function
  if (is.numeric(qq) == FALSE) {
    stop("Input qq must be a numeric vector")
  }
  
  tt <- matrix(rep(0,length(qq) * n), nrow = length(qq), ncol = n)
  colnames(tt) <- cnames
  rownames(tt) <- names(qq)
  for (i in 1:nrow(tt)) {
    if (names(qq[i]) == "\u00f8") { 
      next 
    } else if (names(qq[i]) == "frame") { 
      tt[i,] <- rep(1,n)
    } else { 
      tt[i,] <- (cnames %in% trimws(unlist(strsplit(names(qq[i]) , "\\+")))) 
    }
  }

  return(tt)
}