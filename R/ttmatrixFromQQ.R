#' Construct a description matrix from a qq vector
#' 
#' This utility function is called when we need to reconstruct a tt matrix, e.g. to print a DSM.
#' @param qq Commonality function
#' @param n Dimension of the frame
#' @param cnames A character vector containing the names of the elements of the frame of discernment
#' @param sparse = c("yes","no") whether to use sparse matrix. Default = "no".
#' @return tt A corresponding logical description matrix 
#' @author Peiyuan Zhu
#' @export
#' @examples 
#' tt<- t(matrix(c(1,0,1,1),ncol = 2))
#' m<- c(.9,.1)
#' cnames <- c("yes","no")
#' x<- bca(tt, m, cnames=cnames, method="ezt-m")
#' ttmatrixFromQQ(x$qq,as.integer(x$infovar[1,2]), cnames)
ttmatrixFromQQ <- function(qq, n, cnames, sparse="no") {
  # Obtain tt matrix from commonality function
  #
  # Checks
  # 1. Check that the input is a function
  if (is.numeric(qq) == FALSE) {
    stop("Input qq must be a numeric vector")
  }
  
  if(sparse=="no") {
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
  } else if (sparse=="yes") {
    # Initialize row and column index vectors
    i_idx <- integer(0)
    j_idx <- integer(0)
    x_vals <- logical(0)
    
    row_names <- names(qq)
    col_names <- cnames
    n_rows <- length(qq)
    n_cols <- length(cnames)
    
    for (i in seq_along(qq)) {
      set_name <- row_names[i]
      if (set_name == "\u00f8") {
        next
      } else if (set_name == "frame") {
        j <- seq_len(n_cols)
      } else {
        elements <- trimws(unlist(strsplit(set_name, "\\+")))
        j <- which(col_names %in% elements)
      }
      if (length(j) > 0) {
        i_idx <- c(i_idx, rep(i, length(j)))
        j_idx <- c(j_idx, j)
      }
    }
    
    # Create sparse matrix
    tt <- Matrix::sparseMatrix(
      i = i_idx,
      j = j_idx,
      x = 1,
      dims = c(n_rows, n_cols),
      dimnames = list(row_names, col_names)
    )
    
  } else {
    stop("sparse can either be \"yes\" or \"no\" ")
  }

  return(tt)
}