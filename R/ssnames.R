#' Construct subsets names from column names of a bca.
#' 
#' @param x A bca specification
#' @return subsets_names A list of names.
#' @author Claude Boivin
#'  
#' @examples 
#' y1 <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
#' byrow = TRUE), m = c(0.2,0.5, 0.3), 
#' cnames = c("a", "b", "c"),  
#' varnames = "x", idvar = 1) 
#' ssnames(y1)
#' @export
#' 
ssnames <- function(x) {
  # Obtain ssnames via tt colnames 
  #
  zframe <-colnames(x$tt)
  subsets_names <- lapply(X=1:nrow(x$tt), FUN = function(X) {zframe[x$tt[X,]*1:ncol(x$tt)]})
  return(subsets_names)
}