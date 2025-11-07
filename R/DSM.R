#' Dempser-Shafer models
#' 
#' Function \code{DSM} is used to define subsets of a finite set \eqn{\Theta} of possible values and to assign their corresponding mass value.\cr
#' The set \eqn{\Theta} is called the state-space model (SSM) or "frame of discernment". Each subset \eqn{A} of \eqn{Theta} with a positive mass value is called a focal set. The associated mass value is a number of the \code{(0,1]} interval, called "basic mass assignment" (the basic probability assignment of Shafer's book). All other subsets that have not received a positive mass value are assumed to have a mass value of zero.
#' 
#' There is two ways of defining the DSM: a (0,1) matrix or a list of subsets labels.
#' @aliases bpa bca
#' @param tt Mandatory. A (0,1)-matrix or a boolean matrix. The number of columns must match the number of elements (values) of the SSM \eqn{\Theta}. Each row is a subset of \eqn{\Theta}. The last row is the frame \eqn{\Theta}, represented by a vector of 1's.
#' @param ssnames A list of subsets names which will be obtained from the column names of the tt matrix.
#' @param m A numeric vector of length equal to the number of rows of the matrix  \code{tt}. Values of \code{m} must lie in the interval \code{(0,1]} and must add to one. The mass \code{m(k)} represents the mass value allotted to the focal set represented by the row \code{k} of the matrix \code{tt}.
#' @param include_all Default = FALSE. Put TRUE to include all elements with 0 mass in the DSM.
#' @param cnames A character vector containing the names of the elements of the SSM \eqn{\Theta}. The length must be equal to the number of elements of \eqn{\Theta}. The names are first searched in the \code{valuenames} parameter. If NULL, column names of the matrix \code{tt} are taken if present. Otherwise, names are generated.
#' @param con The measure of conflict can be provided. 0 by default. 
#' @param idvar The number given to the variable. A number is necessary to manage relations between variables  and make computations on a graph. 0 if omitted. 
#' @param infovar  A two-column matrix containing variable identification numbers and the number of elements of the variable. Generated if omitted.
#' @param varnames The name of the variable. Generated if omitted.
#' @param valuenames A list of the names of the variables with the name of the elements of their SSM.
#' @param inforel Not used here. Defined within function \code{\link{jointDSM}}.
#' @return y An object of class \code{DSMspec} called a DSM for "Dempster-Shafer model": \itemize{
#'   \item tt  The table of focal sets. Rownames of the matrix of focal sets are generated from the column names of the elements of the frame. See \code{\link{nameRows}} for details.
#'   \item spec  A two column matrix. First column contains numbers given to the subsets, 1 to  \code{nrow(tt)}. Second column contains the mass values of the subsets. 
#'   \item con  The measure of conflict.
#'   \item infovar  The number of the variable and the size of the SSM.
#'   \item varnames  The name of the variable.
#'   \item valuenames  A list of length 1 consisting of the name of the variable with the names of the elements of the SSM (the column names of the \code{tt} matrix).
#'   \item ssnames A list of subsets names done from the column names of the tt matrix.
#'   \item inforel  Set at 0. used in function \code{\link{jointDSM}}.
#'   }
#' @author Claude Boivin
#' @export
#' @examples 
#' tt<- t(matrix(c(1,0,1,1),ncol = 2))
#' m<- c(.9,.1)
#' cnames <- c("yes","no")
#' DSM(tt, m)
#' DSM(tt, m, cnames)
#' tt1<- t(matrix(c(1,0,1,1),ncol = 2))
#' colnames(tt1) <- c("yes", "no")
#' m <- c(.9, .1)
#' DSM(tt=tt1, m, idvar = 1)
#' y <- DSM(tt=matrix(c(1,0,0,1,1,1),nrow = 2, 
#' byrow = TRUE), m = c(0.6,0.4), include_all = TRUE,
#' cnames = c("a", "b", "c"),varnames = "y", idvar = 1)
#'  printDSM(y)
#' vacuous <- DSM(matrix(c(1,1,1), nrow = 1), m = 1, cnames = c("a","b","c"), ssnames = c("a","b","c"))
#'  printDSM(vacuous)
#' @references \itemize{
#'   \item Shafer, G., (1976). A Mathematical Theory of Evidence. Princeton University Press, Princeton, New Jersey, p. 38: Basic probability assignment.
#'   \item Guan, J. W. and Bell, D. A., (1991). Evidence Theory and its Applications. Elsevier Science Publishing company inc., New York, N.Y., p. 29: Mass functions and belief functions 
#'   \item Dempster, A., (2008). The Dempster–Shafer calculus for statisticians. International Journal of approximate reasoning, 48(2), 365-377.
#' }
DSM <- function(tt = NULL, m, include_all = FALSE, cnames = NULL, con = NULL, ssnames = NULL, idvar = NULL, infovar = NULL, varnames = NULL, valuenames = NULL, inforel=NULL) {
  #
  # Local variables: ztable, zdup, zframe, znames, str1, str2, tt_all, m_all
  # Functions calls: nameRows, DoSSnames
  #
  # 1
  .Deprecated("DSM", msg = "DSM is the new function name for the bca function.", old = "bca")
  #
  # 2. Determine names of columns of tt matrix and fix some parameters
  #
  # 2 Check inputs: DSM constructed with a (0,1) description matrix
  # Check on tt
 if (is.null(tt)) {
   stop("Error in input arguments: description matrix tt is missing.") 
 }
  #
  # Check column names
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
    rownames(tt) <- nameRows(tt)
    if (is.null(con)) { con <- 0 }
    if (is.null(idvar)) { idvar <- 0 }
    #
    # including all elements of the frame in the DSM
    # encode the complete tt matrix
    #
    if (include_all == TRUE) {
      n_frame <- ncol(tt)
      tt_all <- t(sapply(1:2^n_frame,FUN = function(x) {encode(rep(2,n_frame), x-1)}) )
      colnames(tt_all) <- colnames(tt)
      rownames(tt_all) <- nameRows(tt_all)
    }
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
    if (include_all == TRUE) {
    if (is.null(valuenames) | missing(valuenames)) {
      valuenames <- split(colnames(tt_all), rep(paste(rep("v",length(idvar)),c(1:length(idvar)),sep=""), infovar[,2]))
      }
    }
    else {
      if (is.null(valuenames) | missing(valuenames)) {
        valuenames <- split(colnames(tt), rep(paste(rep("v",length(idvar)),c(1:length(idvar)),sep=""), infovar[,2]))
      }
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
    rownames(spec) <- rownames(tt)
    #  
    # mass vector and spec matrix of all elements of the frame
    if (include_all == TRUE) {
      # 2. add elements with 0 mass to the DSM
      # masses
      m_mat <- matrix(spec[,2])
      rownames(m_mat) <- rownames(tt)
      # compute expanded vector of masses
      str1 <- rownames(tt_all)
      str2 <-rownames(m_mat)
      m_mat_pos <- outer(str1, str2, "==")
      m_all <- m_mat_pos %*% m_mat
      m <- m_all
      spec <- cbind((1:nrow(tt_all)), m)
      colnames(spec) <- c("specnb", "mass")
      rownames(spec) <- rownames(tt_all)
    }
    #
    # Update tt
    #
    if (include_all == TRUE) {
      tt <- tt_all
    }
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
    
    #
    # 8. Construction of the result
    #
    y<-list(con = con, tt = tt, spec = spec , infovar = infovar, varnames = varnames, valuenames = valuenames, ssnames = znames, inforel = inforel) 
    # end test
    #
    class(y) <- append(class(y), "DSMspec")
    return(y)
  }
} 
#' @rdname DSM
#' @export
bca <- DSM