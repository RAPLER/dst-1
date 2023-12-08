#' Construct a description matrix from a list of subsets names.
#' 
#' @param x A list of names
#' @param sparse=c("yes","no") whether to use sparse matrix
#' @param valuenames=NULL valuenames of bca
#' @return ttmat A corresponding logical description matrix 
#' @author Claude Boivin
#' @examples 
#' subsets_names <- list(c("b", "c"), "b", c("a", "b", "c"))
#' ttmatrix(subsets_names)
#' @export
#' 
ttmatrix <- function(x, sparse="no", valuenames=NULL) {
  # Obtain tt matrix from subsets names 
  #
  # Checks
  # 1. Check that the input is a list
  if (is.list(x) == FALSE) {
    stop("Input must be a list.")
  }
  #
  if(sparse=="yes") {
    if(is.null(valuenames)) {
      stop("Need valuenames when sprase = \"yes\" ")
    }
    rowIdx <- vector()
    colIdx <- vector()
    for(i in ifelse(length(x[[1]]) == 1, ifelse(x[[1]] == "Empty", 2, 1), 1):length(x)) {
      rowIdx<-c(rowIdx,rep(i,length(x[[i]])))
      colId <- match(x[[i]],valuenames)
      colIdx<-c(colIdx,colId)
    }
    ttmat <- Matrix::sparseMatrix(
      i = rowIdx,
      j = colIdx, 
      x = 1, 
      dims = c(length(x), length(valuenames) )
    )
    ttmat <- methods::as(ttmat, "RsparseMatrix")
    return(ttmat)
  } else if(sparse=="no") {
    z1l <- lapply(X = 1:length(x), FUN = function(X) {outer(x[[X]], x[[length(x)]], "==") } ) 
    ttmat <- t(mapply(FUN= function(X,Y) {unlist(lapply(X=1:ncol(z1l[[length(z1l)]]), FUN =  function(X) { reduction(z1l[[Y]][,X], f = "|")}) ) }, Y=1:length(x) ) )
    colnames(ttmat) <- c(x[[length(x)]])
    rownames(ttmat) <- nameRows(ttmat)
    return(ttmat)
  } else {
    stop("sparse can either be \"yes\" or \"no\" ")
  }
}