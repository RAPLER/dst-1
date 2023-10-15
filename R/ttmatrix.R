#' Construct a description matrix from a list of subsets names.
#' 
#' @param x A list of names
#' @return ttmat A corresponding logical description matrix 
#' @author Claude Boivin
#' @examples 
#' subsets_names <- list(c("b", "c"), "b", c("a", "b", "c"))
#' ttmatrix(subsets_names)
#' @export
#' 
ttmatrix <- function(x) {
  # Obtain tt matrix from subsets names 
  #
  # Checks
  # 1. Check that the input is a list
  if (is.list(x) == FALSE) {
    stop("Input must be a list.")
  }
  #
z1l <- lapply(X = 1:length(x), FUN = function(X) {outer(x[[X]], x[[length(x)]], "==") } ) 
ttmat <- t(mapply(FUN= function(X,Y) {unlist(lapply(X=1:ncol(z1l[[length(z1l)]]), FUN =  function(X) { reduction(z1l[[Y]][,X], f = "|")}) ) }, Y=1:length(x) ) )
colnames(ttmat) <- c(x[[length(x)]])
rownames(ttmat) <- nameRows(ttmat)
return(ttmat)
}