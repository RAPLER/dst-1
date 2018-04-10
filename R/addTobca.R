#' Add some elements of 0 mass to an existing mass function 
#'
#' Given a previously defined bca, the user may want to add some elements of the set of possible values or some subsets, even if they have zero mass value. This feature is useful, for example, to examine the plausibility results of these elements or subsets of zero mass value.
#' @param x A basic chance assignment mass function (see \code{\link{bca}}). It can also be the normalized result of the combination of bca’s by Dempster’s Rule.
#' @param f A matrix constructed in a boolean style (0,1) or a boolean matrix. The number of columns of the matrix \code{f} must match the number of columns of the tt matrix of \code{x} (see \code{\link{bca}}). Each row of the matrix identify a subset of the set of possible values.
#' @return The original bca  mass function \code{x} augmented with the added subsets defined by \code{f}.
#' @author Claude Boivin, Stat.ASSQ
#' @export
#' @examples  
#' y <- bca(f=matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), 
#' m=c(0.6, 0.4),  cnames = c("a", "b", "c"), varnb=1)
#' addTobca(y, matrix(c(0,1,0,0,0,1, 0,1,1), nrow=3, byrow = TRUE))
#' x <- bca(f=matrix(c(0,1,1,1,1,0,1,1,1),nrow=3, 
#' byrow = TRUE), m=c(0.2,0.5, 0.3), 
#' cnames =c("a", "b", "c"), varnb=1)
#' xy <- dsrwon(x,y)
#' xy1 <- addTobca(nzdsr(xy), matrix(c(0,1,0,0,0,1), nrow=2, byrow = TRUE))
#' xy1
#' addTobca(x, f = diag(1,  ncol(x$tt) ) ) # add all singletons
addTobca <- function(x, f) {
  if ( inherits(x, "bcaspec") == FALSE) {
    stop("Input x not of class bcaspec.")
  }
  if ((is.matrix(f) ==FALSE) ) {
    stop("f parameter must be a (0,1) or logical matrix.")
  }
  nc1 <- ncol(x$tt)
    if (nc1 != ncol(f)) {
    stop("Error in input arguments: number of columns of f not equal to ncol(x$tt)") 
    }
  x$tt <- rbind(f,x$tt)
  rownames(x$tt) <- nameRows(x$tt)
  specnb <- 1:nrow(x$tt)
  mass <- c(rep(0,nrow(f)), x$spec[,2])
  x$spec <- cbind(specnb, mass)
  return(x)
} 