#' Simple printing of the \code{tt} matrix and mass values of a basic chance assignment (bca)
#'
#' This utility function does a simple printing of a bca
#' 
#' @param x A list of class bcaspec.
#' @return A table of subsets with their associated mass. Subsets are identified by row names.
#' @author Claude Boivin
#' @export
#' @examples
#' z <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
#' byrow = TRUE), m = c(0.2,0.5, 0.3), 
#' cnames = c("a", "b", "c"), idvar = 1)
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
  # x must have tt and spec
  if (is.null(x$tt) || is.null(x$spec)) {
    stop("Missing tt or spec")
  }
  y <- as.data.frame(cbind(rownames(x$tt), x$spec))
  colnames(y)[1] <- deparse(substitute(x))
  if (!is.null (x$qq) ) {
    print("Closure elements have been added to the bca, since you use commonalities.")
  } else {
  y <- y[y[,ncol(y)] > 0,]
  }
  rownames(y) <- NULL
  print(y)
  }