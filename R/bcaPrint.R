#' Simple printing of the tt matrix and mass values of a basic chance assignment (bca)
#'
#' This utility function does a simple printing of a bca
#' 
#' @param x A list of class bcaspec.
#' @return A table of subsets with their associated mass. Subsets are identified by row names.
#' @author Claude Boivin, Stat.ASSQ
#' @export
#' @examples
#' z <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow=3, 
#' byrow = TRUE), m=c(0.2,0.5, 0.3), 
#' cnames =c("a", "b", "c"), varnb = 1)
#' bcaPrint(z)
bcaPrint <- function(x) {
  #
  # Local variables: y 
  # Functions calls: None
  #
  # Input check. 
  # x must be a bca specification
  if ( inherits(x, "bcaspec") == FALSE) {
    stop("Input argument not of class bcaspec.")
  }
  y <- as.data.frame(cbind(rownames(x$tt), x$spec))
  colnames(y)[1] <- deparse(substitute(x))
  rownames(y) <- NULL
  print(y)
  }