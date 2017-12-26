#'  Basic chance assignment distribution
#' 
#' This function assigns to some subsets of a finite set \eqn{\Theta} of possible values their corresponding mass.\cr
#' The set \eqn{\Theta} is called the frame of discernement. Each subset \eqn{A of \Theta} is called a focal element or a proposition. The associated mass is a number in the (0,1] interval, called "basic chance assignment" (the basic probability assignment of Shafer's book).
#' @aliases bpa
#' @param f A matrix constructed in a boolean style (0,1) or a boolean matrix. The number of columns of the matrix must match the number of elements (values) of the frame of discernment \eqn{\Theta}. Each row of the matrix is a focal element, i.e. a subset of the frame of discernment described by a vector of (0,1). The last line is the frame \eqn{\Theta}, represented by a vector of 1's.
#' @param m A vector of masses of length equal to the number of rows of the matrix  \code{f}. The values of \code{m} must lie in the (0,1] interval. The sum of the elements of \code{m} must be 1. The mass \code{m[k]} represents the chances allowed to the proposition represented by the row \code{k} of the matrix \code{f}.
#' 
#' @param cnames A character vector of names of the elements of the frame of discernment, of length equal to the number of elements of the frame \eqn{\Theta}. If NULL, takes column names of the matrif f if present. Otherwise, names are generated.
#' @param n The variable number. The variable number will be used when combining bca's defined on different frames of discernment. See \code{\link{productSpace}}. Set at value 0 if NULL.
#' @param inforel A two column matrix containing relation numbers and the depth (number of variables) of each relation.
#' @param con The measure of conflict. Set at 0 by default.
#' @param infovar The variable number and the size of the frame of discernment.
#' @param infovarnames Set at NULL. See \code{\link{bcaRel}}.
#' @param infovaluenames Set at NULL. See \code{\link{bcaRel}}.
#' @return The result is the representation of a belief function by its basic chance assignment of propositions. It is an object of class \code{bcaspec}, a list of six elements: \itemize{
#'   \item $combination: The table of focal elements f with the addition of the column of associated masses. Rownames of the matrix of focal elements are created from the column names of the elements of the frame. See \code{\link{nameRows}} for details.
#'   \item $tt The table of focal elements f alone. 
#'   \item $spec A two column matrix. First column contains specification numbers: 1 to  nrow(f). Second column contains the mass vector.
#'   \item $inforel A two column matrix containing relation numbers and the depth (number of variables) of each relation.
#'   }
#'   @details The Basic chance assignment distribution of a variable can also be obtained using the more general function \code{\link{bcaRel}}.
#' @author Claude Boivin, Stat.ASSQ
#' @export
#' @examples 
#' f<- t(matrix(c(1,0,1,1),ncol=2))
#' m<- c(.9,.1)
#' cnames <- c("yes","no")
#' bca(f, m)
#' bca(f, m, cnames)
#' bca(f, m, cnames, n = 1)
#' bca(f, m, cnames, infovar = matrix(c(1,2), ncol=2), inforel = matrix(c(1,2,2,3), ncol=2, byrow=TRUE))
#' x <- bca(f=matrix(c(0,1,1,1,1,0,1,1,1),nrow=3, byrow = TRUE), m=c(0.2,0.5, 0.3), cnames =c("a", "b", "c"), n = 1)
#' y <- bca(f=matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), m=c(0.6,0.4), cnames =c("a", "b", "c"), n=1)
#' frame <- bca(matrix(c(1,1,1), nrow=1), m=1, cnames = c("a","b","c"))
#' @references Shafer, G., (1976). A Mathematical Theory of Evidence. Princeton University Press, Princeton, New Jersey, p. 38: Basic probability assignment.
#' 
bca<-function(f, m, cnames = NULL, con = NULL, n = NULL, infovar = NULL, infovarnames = NULL, infovaluenames = NULL, inforel=NULL) {
  # test
   if ((!is.null(infovaluenames)) & (typeof(infovaluenames) == "character")) { cnames = infovaluenames}
  # fin test 
  if (is.null(cnames)) { cnames = colnames (f)}
   if (is.null(cnames)) {    
    cnames <- paste(rep("v",ncol(f)),c(1:ncol(f)),sep="")
    }
  if((abs(sum(m)-1)>0.000001) | (length(f[,1])!=length(m)) | (length(f[1,])!=length(cnames))){ 
    stop("Error in input arguments: check your input data.") 
    }
  else {
    colnames(f) <- cnames
    if (missing(con)) { con <- 0 }
    spec <- cbind((1:nrow(f)), m)
    colnames(spec) <- c("specnb", "mass")
    if (missing(n)) { n <- 0 }
    if (missing(infovar)) {
      infovar <- matrix(c(n, ncol(f)), ncol = 2)
    }
    colnames(infovar) <- c("varnb", "size")
# check and use infovarnames
    if ((length(infovarnames) > 1) & (nrow(infovar) == 1)) {
      message("infovarnames: only the first element used")
      infovarnames <- infovarnames[1] 
    }
    if (nrow(infovar) < 2) {
      if (missing(infovarnames)) {
      infovaluenames <- split(cnames, rep(paste(rep("v", nrow(infovar)),c(1:nrow(infovar)),sep=""), infovar[,2]))
      } else {
      infovaluenames <- split(cnames,rep(infovarnames, infovar[,2]))
      }
    }
    if (missing(inforel)) { 
      relnb <- 0
      depth <- 0 
      inforel <- matrix(c(relnb, depth), ncol = 2)
    }
    colnames(inforel) <- c("relnb", "depth")
    z<-cbind(m,f)
    colnames(z) <- c("mass", cnames)
    rownames(f) <- rownames(z) <- nameRows(rbind(z[,-1]))
    y<-list(combination=z, con = con, tt = f, spec = spec , infovar = infovar, infovaluenames = infovaluenames, inforel = inforel)
    class(y) <- append(class(y), "bcaspec")
    return(y)
  }
 }