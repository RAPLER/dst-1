#' Add focal elements with 0 mass
#'
#' This utility function allows to expand a bca definition with focal elements of zero mass. This is useful when we want to compare the plausibility results of some focal elements which do not already appear because their belief mass is 0.
#' @param x A belief function in its bca form (see \code{\link{bca}}). It can be a bca definition or the normalized result of a combination by Dempster'sRule.
#' @param f A matrix constructed in a boolean style (0,1) or a boolean matrix. The number of columns of the matrix must match the number of elements (values) of the frame of discernment \eqn{\Theta} of \code{x}.
#'  @return The original bca \code{x} augmented with the added focal elements defined by \code{f}.
#' @author Claude Boivin, Stat.ASSQ
#' @export
#' @examples  
#' y <- bca(f=matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), m=c(0.6, 0.4),  cnames = c("a", "b", "c"), varnb=1)
#' addTobca(y, matrix(c(0,1,0,0,0,1, 0,1,1), nrow=3, byrow = TRUE))
#' x <- bca(f=matrix(c(0,1,1,1,1,0,1,1,1),nrow=3, byrow = TRUE), m=c(0.2,0.5, 0.3), cnames =c("a", "b", "c"), varnb=1)
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
#  f1 <- cbind(rep(0, nrow(f)), f)
  f1 <- f
  nc1 <- ncol(x$tt)
    if (nc1 != ncol(f1)) {
    stop("Error in input arguments: number of columns of f not equal to ncol(x$tt)") 
    }
  x$tt <- rbind(f,x$tt)
  rownames(x$tt) <- nameRows(x$tt)
  specnb <- 1:nrow(x$tt)
#  mass <- c(f1[,1], x$spec[,2])
  mass <- c(rep(0,nrow(f)), x$spec[,2])
  x$spec <- cbind(specnb, mass)
#  x$combination <- rbind(f1,x$combination)
#  rownames(x$combination) <- rownames(x$tt)
  return(x)
} 