#'  Basic chance assignment mass function
#' 
#' Function \code{bca} is used to define subsets of a finite set \eqn{\Theta} of possible values and to assign their corresponding mass value.\cr
#' The set \eqn{\Theta} is called the frame of discernment. Each subset \eqn{A} of  \eqn{Theta} with a positive mass value is called a focal element or a proposition. The associated mass value is a number of the \code{(0,1]} interval, called "basic chance assignment" (the basic probability assignment of Shafer's book). All other subsets that have not received a positive mass value are assumed to have a mass value of zero.
#' 
#' There is two ways of defining the bca: a (0,1) matrix or a list of subsets labels.
#' @aliases bpa
#' @param tt Mandatory. A (0,1)-matrix or a boolean matrix. The number of columns must match the number of elements (values) of the frame of discernment \eqn{\Theta}. Each row is a subset of \eqn{\Theta}. The last row is the frame \eqn{\Theta}, represented by a vector of 1's.
#' @param ssnames A list of subsets names which will be obtained from the column names of the tt matrix.
#' @param m A numeric vector of length equal to the number of rows of the matrix  \code{tt}. Values of \code{m} must lie in the interval \code{(0,1]} and must add to one. The mass \code{m(k)} represents the chance value allotted to the proposition represented by the row \code{k} of the matrix \code{tt}.
#' @param cnames A character vector containing the names of the elements of the frame of discernment \eqn{\Theta}. The length must be equal to the number of elements of \eqn{\Theta}. The names are first searched in the \code{valuenames} parameter. If NULL, column names of the matrix \code{tt} are taken if present. Otherwise, names are generated.
#' @param con The measure of conflict can be provided. 0 by default. 
#' @param idvar The number given to the variable. A number is necessary to manage relations between variables  and make computations on a graph. 0 if omitted. 
#' @param infovar  A two-column matrix containing variable identification numbers and the number of elements of the variable. Generated if omitted.
#' @param varnames The name of the variable. Generated if omitted.
#' @param valuenames A list of the names of the variables with the name of the elements of their frame of discernment.
#' @param inforel Not used here. Defined within function \code{\link{bcaRel}}.
#' @return y An object of class \code{bcaspec} called a bca for "basic chance assignment": \itemize{
#'   \item tt  The table of focal elements. Rownames of the matrix of focal elements are generated from the column names of the elements of the frame. See \code{\link{nameRows}} for details.
#'   \item qq  Commonality functions from the frame of discernment to \eqn{[0,1]}
#'   \item spec  A two column matrix. First column contains numbers given to the subsets, 1 to  \code{nrow(tt)}. Second column contains the mass values of the subsets. 
#'   \item con  The measure of conflict.
#'   \item infovar  The number of the variable and the size of the frame of discernment.
#'   \item varnames  The name of the variable.
#'   \item valuenames  A list of length 1 consisting of the name of the variable with the names of the elements of the frame of discernment (the column names of the \code{tt} matrix).
#'   \item ssnames A list of subsets names done from the column names of the tt matrix.
#'   \item inforel  Set at 0. used in function \code{\link{bcaRel}}.
#'   }
#' @author Claude Boivin
#' @export
#' @examples 
#' tt<- t(matrix(c(1,0,1,1),ncol = 2))
#' m<- c(.9,.1)
#' cnames <- c("yes","no")
#' bca(tt, m)
#' bca(tt, m, cnames)
#' tt1<- t(matrix(c(1,0,1,1),ncol = 2))
#' colnames(tt1) <- c("yes", "no")
#' m <- c(.9, .1)
#' bca(tt=tt1, m, idvar = 1)
#' x <- bca(tt=matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
#' byrow = TRUE), m = c(0.2,0.5, 0.3), 
#' cnames = c("a", "b", "c"), idvar = 1)
#' y <- bca(tt=matrix(c(1,0,0,1,1,1),nrow = 2, 
#' byrow = TRUE), m = c(0.6,0.4), 
#' cnames = c("a", "b", "c"),varnames = "y", idvar = 1)
#' vacuous <- bca(matrix(c(1,1,1), nrow = 1), m = 1, cnames = c("a","b","c"), ssnames = c("a","b","c"))
#' @references \itemize{
#' \item Shafer, G., (1976). A Mathematical Theory of Evidence. Princeton University Press, Princeton, New Jersey, p. 38: Basic probability assignment.
#' \item Guan, J. W. and Bell, D. A., (1991). Evidence Theory and its Applications. Elsevier Science Publishing company inc., New York, N.Y., p. 29: Mass functions and belief functions 
#' }
bca<-function(tt = NULL, m, qq = NULL, cnames = NULL, con = NULL, ssnames = NULL, idvar = NULL, infovar = NULL, varnames = NULL, valuenames = NULL, inforel=NULL) {
  #
  # Local variables: ztable, zdup, zframe, znames
  # Functions calls: nameRows, DoSSnames
  #
  # 1, Empty section
  #
  # 2. Choose between tt or ssnames and Determine names of columns of tt matrix and fix some parameters
  #
  # 2 Check inputs: bca constructed with a (0,1) description matrix
  # Check on tt
 if (is.null(tt)) {
   stop("Error in input arguments: description matrix tt is missing.") 
 }
  #
  # Check column names
  if (is.null(cnames)) { 
    cnames = colnames (tt) # cnames stay null if no column names present
  } 
  if (is.null(cnames)) {    
    cnames <- colnames(tt, do.NULL = FALSE, prefix = "col") 
  } 
  #
  # Check for duplicates column names
  ztable <- outer(cnames, cnames, "==")
  zdup <- apply(ztable, 2, sum)
  if (sum(zdup) != ncol(tt)) {
    stop("Error in input arguments: Duplicate names in column names of tt matrix or in column names supplied.") 
  }
  #
  # Check mass vector
  #
  if((abs(sum(m)-1)>0.000001) | (length(tt[,1])!=length(m)) ) { 
    stop("Error in input arguments: check your input data.") 
    }
  else {
    colnames(tt) <- cnames # for the case where tt matrix had no column names
    if (is.null(con)) { con <- 0 }
    if (is.null(idvar)) { idvar <- 0 }
  #
  # 3. Build infovar parameter
  #
    if (is.null(infovar)) {
      infovar <- matrix(c(idvar, ncol(tt)), ncol = 2)
    }
    colnames(infovar) <- c("varnb", "size")
    idvar <- infovar[,1]
    #
    # 4. Build varnames and valuenames (former = infovaluenames)
    #
    # check and use varnames if provided
    if (is.null(valuenames) | missing(valuenames)) {
    valuenames <- split(colnames(tt), rep(paste(rep("v",length(idvar)),c(1:length(idvar)),sep=""), infovar[,2]))
    }
    if (!is.null(varnames)) {
      if (is.numeric(varnames)) {
        stop("Names of variables must start with a letter.")
        }
      if (length(varnames) != (nrow(infovar)) ) {
        stop("number of variable names  not equal to number of variables") }
      names(valuenames) <- varnames
    }
    if (is.null(varnames)) {
      varnames <- names(valuenames)
      } 
    #
    # 5. Build specification matrix spec
    #
    spec <- cbind((1:nrow(tt)), m)
    colnames(spec) <- c("specnb", "mass")
    #  
    # 6. inforel parameter
    #
    if (missing(inforel) | is.null(inforel)) { 
      inforel <- matrix(c(0, 0), ncol = 2)
    }
    colnames(inforel) <- c("relnb", "depth")
    # 
    # 7 build subsets names
    # 
    if (is.null(ssnames) == TRUE ) {
      znames <- DoSSnames(tt)
    }
    else {
      znames <- ssnames
    }
    
    # 7.1 build qq
    if (is.null(qq) == TRUE ) {
      qq <- commonality(tt,m)
    }
    
    #
    # 8. Construction of the result
    #
    rownames(tt) <- nameRows(tt)
    #
    y<-list(con = con, tt = tt, qq=qq, spec = spec , infovar = infovar, varnames = varnames, valuenames = valuenames, ssnames = znames, inforel = inforel) 
    # end test
    #
    class(y) <- append(class(y), "bcaspec")
    return(y)
  }
 } 