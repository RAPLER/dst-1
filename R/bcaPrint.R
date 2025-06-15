#' Simple printing of the \code{tt} matrix and mass values of a basic chance assignment (bca)
#'
#' This utility function does a simple printing of a bca
#' 
#' @param x A list of class bcaspec.
#' @param remove Default = FALSE. Put = TRUE to exclude subsets with zero mass.
#' @return A table of subsets with their associated mass. Subsets are identified by row names.
#' @author Claude Boivin
#' @export
#' @examples
#' z <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
#' byrow = TRUE), m = c(0.2,0.5, 0.3), 
#' cnames = c("a", "b", "c"), idvar = 1)
#' bcaPrint(z)
#' y <- bca(tt=matrix(c(1,0,0,1,1,1),nrow = 2, 
#' byrow = TRUE), m = c(0.6,0.4), include_all = TRUE,
#' cnames = c("a", "b", "c"),varnames = "y", idvar = 1)
#'  bcaPrint(y)
#'  bcaPrint(y, remove = TRUE)
#' x <- bca(tt=matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
#' byrow = TRUE), m = c(0.2,0.5, 0.3),
#' cnames = c("a", "b", "c"), idvar = 1, method = "ezt-m")
#'  bcaPrint(x)
bcaPrint <- function(x, remove = FALSE) {
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
    if (!is.null (x$qq)) {
      # rebuild tt and spec
      # recover mass vector
      mx <- mFromQQ(x$qq, x$infovar[1,2], x$valuenames[[1]], method = "emt-m")
      # recover tt
      tx <- (ttmatrixFromQQ(x$qq,x$infovar[1,2],x$valuenames[[1]]))[mx >0,]
      specx <- matrix(c(1:nrow(tx),mx[mx>0]), ncol = 2 )
      rownames(specx) <- rownames(tx)
      colnames(specx) <- c("specnb", "mass")
      x$tt <- tx
      x$spec <- specx
      # End
    } else {
    stop("Missing tt or spec")
    }
  }
  y <- as.data.frame(cbind(rownames(x$tt), x$spec))
  if (!is.null (x$qq) ) {
    message("Closure elements (with 0 mass) may have been added to the bca, since you use commonalities.")
  } else
  if (remove == TRUE) {
  y <- y[y[,ncol(y)] > 0,]
  }
  rownames(y) <- NULL
  print(y)
  }