#' Construct subsets names from column names of a tt matrix
#' 
#' @param tt A description matrix with column names
#' @return subsets_names A list of names.
#' @author Claude Boivin
#'  
#' @examples 
#' y1 <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
#' byrow = TRUE), m = c(0.2,0.5, 0.3), 
#' cnames = c("a", "b", "c"),  
#' varnames = "x", idvar = 1) 
#' ssnames(y1$tt)
#' @export
#' 
DoSSnames <- function(tt) {
  # Obtain ssnames via tt colnames 
  #
  zframe <-colnames(tt)
  if (is.null(zframe) ) {
    stop("Column names missing. Add column names to your mamtrix.")
  }
  subsets_names <- lapply(X=1:nrow(tt), FUN = function(X) {zframe[tt[X,]*1:ncol(tt)]})
  return(subsets_names)
}