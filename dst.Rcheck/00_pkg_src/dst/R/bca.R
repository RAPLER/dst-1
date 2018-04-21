#'  Basic chance assignment mass function
#' 
#' Function \code{bca} assigns their corresponding mass value to some subsets of a finite set \eqn{\Theta} of possible values.\cr
#' The set \eqn{\Theta} is called the frame of discernment. Each subset \eqn{A of \Theta} with a positive mass value is called a focal element or a proposition. The associated mass value is a number of the \code{(0,1]} interval, called "basic chance assignment" (the basic probability assignment of Shafer's book). All other subsets that have not received a positive mass value are assumed to have a mass of zero value.
#' @aliases bpa
#' @param f A (0,1)-matrix or a boolean matrix. The number of columns must match the number of elements (values) of the frame of discernment \eqn{\Theta}. Each row is a subset of \eqn{\Theta}. The last row is the frame \eqn{\Theta}, represented by a vector of 1's.
#' @param m A vector of masses of length equal to the number of rows of the matrix  \code{f}. The values of \code{m} must lie in the interval \code{(0,1]} and must add to one. The mass \code{m(k)} represents the chance value allotted to the proposition represented by the row \code{k} of the matrix \code{f}.
#' 
#' @param cnames A character vector containing the names of the elements of the frame of discernment \eqn{\Theta}. The length must be equal to the number of elements of \eqn{\Theta}. The names are searched in the \code{infovaluenames} parameter first. If NULL, column names of the matrix \code{f} are taken if present. Otherwise, names are generated.
#' @param infovaluenames Name and value names of the variable. See \code{\link{bcaRel}}.
#' @param con The measure of conflict. 0 by default.
#' @param varnb The number given to the variable.  0 if omitted. 
#' @param infovar  A two-column matrix containing variable identification numbers and the number of elements of the variable. Generated if omitted.
#' @param infovarnames The name of the variable. Generated if omitted.
#' @param inforel Not used. Defined within function \code{\link{bcaRel}}.
#' @return An object of class \code{bcaspec}: \itemize{
#'   \item tt The table of focal elements f. Rownames of the matrix of focal elements are generated from the column names of the elements of the frame. See \code{\link{nameRows}} for details.
#'   \item spec A two column matrix. First column: numbers given to the subsets, 1 to  \code{nrow(f)}. Second column: the mass values of the subsets. 
#'   \item con The measure of conflict.
#'   \item infovar The number of the variable and the size of the frame of discernment.
#'   \item infovaluenames The names of the elements of the frame of discernment of the variable (the column names of the \code{tt} matrix).
#'   \item inforel Set at 0. used in function \code{\link{bcaRel}}.
#'   }
#' @author Claude Boivin, Stat.ASSQ
#' @export
#' @examples 
#' f<- t(matrix(c(1,0,1,1),ncol=2))
#' m<- c(.9,.1)
#' cnames <- c("yes","no")
#' bca(f, m)
#' bca(f, m, cnames)
#' bca(f, m, cnames, varnb = 1)
#' x <- bca(f=matrix(c(0,1,1,1,1,0,1,1,1),nrow=3, 
#' byrow = TRUE), m=c(0.2,0.5, 0.3), 
#' cnames =c("a", "b", "c"), varnb = 1)
#' y <- bca(f=matrix(c(1,0,0,1,1,1),nrow=2, 
#' byrow = TRUE), m=c(0.6,0.4), 
#' cnames =c("a", "b", "c"),infovarnames = "y", varnb = 1)
#' frame <- bca(matrix(c(1,1,1), nrow=1), m=1, cnames = c("a","b","c"))
#' @references \itemize{
#' \item Shafer, G., (1976). A Mathematical Theory of Evidence. Princeton University Press, Princeton, New Jersey, p. 38: Basic probability assignment.
#' \item Guan, J. W. and Bell, D. A., (1991). Evidence Theory and its Applications. Elsevier Science Publishing company inc., New York, N.Y., p. 29: Mass functions and belief functions 
#' }
bca<-function(f, m, cnames = NULL, infovaluenames = NULL, con = NULL, varnb = NULL, infovar = NULL, infovarnames = NULL, inforel=NULL) {
   if ((!is.null(infovaluenames)) & (typeof(infovaluenames) == "character")) { cnames = infovaluenames}
  if (is.null(cnames)) { cnames = colnames (f)}
  if (is.null(cnames)) {    
    cnames <- paste(rep("col",ncol(f)),c(1:ncol(f)),sep="")
    }
  if((abs(sum(m)-1)>0.000001) | (length(f[,1])!=length(m)) | (length(f[1,])!=length(cnames))){ 
    stop("Error in input arguments: check your input data.") 
    }
  else {
    colnames(f) <- cnames
    if (missing(con)) { con <- 0 }
    spec <- cbind((1:nrow(f)), m)
    colnames(spec) <- c("specnb", "mass")
    if (missing(varnb)) { varnb <- 0 }
    # infovar parameter
    if (missing(infovar)) {
      infovar <- matrix(c(varnb, ncol(f)), ncol = 2)
    }
    colnames(infovar) <- c("varnb", "size")
   # check and use infovarnames
    if ((length(infovarnames) > 1) & (nrow(infovar) == 1)) {
      message("infovarnames: only the first element used")
      infovarnames <- infovarnames[1] 
    }
    # infovaluenames parameter
    if (nrow(infovar) < 2) {
      if (missing(infovarnames)) {
      infovaluenames <- split(cnames, rep(paste(rep("v", nrow(infovar)),c(1:nrow(infovar)),sep=""), infovar[,2]))
      } else {
        infovaluenames <- split(cnames,rep(infovarnames, infovar[,2]))
      }
    }
   # inforel parameter
    if (missing(inforel) | is.null(inforel)) { 
      relnb <- 0
      depth <- 0 
      inforel <- matrix(c(relnb, depth), ncol = 2)
    }
    colnames(inforel) <- c("relnb", "depth")
    # construction of the result
    rownames(f) <- nameRows(f)
    y<-list(con = con, tt = f, spec = spec , infovar = infovar, infovaluenames = infovaluenames, inforel = inforel)
    class(y) <- append(class(y), "bcaspec")
    return(y)
  }
 }