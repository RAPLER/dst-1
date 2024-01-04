#' Construct a description matrix from a list of subsets names.
#' 
#' @param x A list of names
#' @param sparse=c("yes","no") whether to use sparse matrix
#' @return ttmat A corresponding logical description matrix 
#' @author Claude Boivin
#' @examples 
#' subsets_names <- list(c("b", "c"), "b", c("a", "b", "c"))
#' ttmatrix(subsets_names)
#' znames <- list("empty", "a", c("b", "c"), c("a", "b"), c("a", "b", "c") )
#' print(ttmatrix(znames) )
#' print(ttmatrix(znames, sparse = "yes") )
#' @export
#' 
ttmatrix <- function(x, sparse="no") {
  # Obtain tt matrix from subsets names 
  #
  # Checks
  # 1. Check that the input is a list
  if (is.list(x) == FALSE) {
    stop("Input must be a list.")
  }
  #
  if(sparse=="yes") {
    zframe <- x[[length(x)]]
    zz1 <- lapply(X=1:length(x), FUN= function(X) {lapply(X=1:length(x[[X]]), FUN = function(Y) {which(zframe == (x[[X]])[Y])}) } )
    #
    # construct the corresponding sparse tt matrix of the resulting subset names
    rowIdx <- vector()
    colIdx <- vector()
    for (i in 1:length(x) )  {
      tempc <-  (unlist(zz1[[i]]) )
      colIdx <- c(colIdx, tempc )
      tempr <- rep(i,sum(tempc >0) )
      rowIdx <- c(rowIdx, tempr)
    }
    # Obtain sparse tt matrix of the result
    ttmat <- Matrix::sparseMatrix(
      i = rowIdx,
      j = colIdx, 
      x = 1, 
      dims = c(length(x), length(zframe) )
    )
    colnames(ttmat) <- zframe
    rownames(ttmat) <- nameRows(ttmat)
    ttmat <- methods::as(ttmat, "RsparseMatrix")
  } else if(sparse=="no") {
    z1l <- lapply(X = 1:length(x), FUN = function(X) {outer(x[[X]], x[[length(x)]], "==") } ) 
    ttmat <- t(mapply(FUN= function(X,Y) {unlist(lapply(X=1:ncol(z1l[[length(z1l)]]), FUN =  function(X) { reduction(z1l[[Y]][,X], f = "|")}) ) }, Y=1:length(x) ) )
  } else {
    stop("sparse can either be \"yes\" or \"no\" ")
  }
  colnames(ttmat) <- c(x[[length(x)]])
  rownames(ttmat) <- nameRows(ttmat)
  return(ttmat)
}