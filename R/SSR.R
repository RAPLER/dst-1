#'  One dimensional State Space representation
#' 
#' Function \code{SSR} is used to specify a set \eqn{\Theta} of possible values.\cr
#' The set \eqn{\Theta} is called the state space.
#' 
#' @param varnames The name of the variable. Generated if omitted.
#' @param idvar The number given to the state space. A number is necessary to manage relations between variables of multidimensional state spaces and enable computations on a graph. 0 if omitted. 
#' @param size The number of elements of the state space representation \eqn{\Theta}.
#' @param cnames A character vector containing the names (labels) of the elements of the state space \eqn{\Theta}. The length must be equal to the \code{size} parameter. If omitted, names are generated.
#' @return y A list called a SSR for "State Space Representation": \itemize{
#'   \item infovar  The number of the variable and the size of the state space.
#'   \item varnames  The name of the variable.
#'   \item valuenames  A list of length 1 consisting of the name of the variable with the names (labels) of the elements of the state space.
#'   }
#' @author Claude Boivin
#' @export
#' @examples 
#'ss1 <- SSR("x1", 1,4,c("a","b","c","d"))
#'
#' @references \itemize{
#' \item dDmpster (2007)...
#' \item Gonng (2024)...
#' }
SSR<- function(varnames = NULL, idvar = NULL, size = NULL, cnames = NULL) {
  #
  # Local variables: ztable, zdup
  #
  # 2. Check input parameters
  #
  if (is.null(varnames)) {
    stop("Error in input arguments: a variable name is required.") 
  } 
  if (typeof(varnames) != "character") {
    stop("Error in input arguments: variable name must be a character string.") 
  }
  if (is.null(idvar)) { 
    stop("Error in input arguments: a number is required.") 
  }
  # Check labels supplied
  if (!is.null(cnames)) { 
    # check size
    if (shape(cnames) != size) {
      stop("Error in input arguments: number of labels supplied not equal to size of the state space.") 
    }
    #
    # Check for duplicates labels
    ztable <- outer(cnames, cnames, "==")
    zdup <- apply(ztable, 2, sum)
    if (sum(zdup) != size) {
      stop("Error in input arguments: Duplicate names in labels supplied.") 
    }
    #
  }
  if (is.null(cnames)) {    
    cnames <-  paste("col", 1:size,sep="")
  } 
  #
  # 3. Build infovar parameter
  #
  infovar <- matrix(c(idvar, size), ncol = 2)
  colnames(infovar) <- c("varnb", "size")
  #
  # 4. Build varnames and valuenames
  #
  valuenames <- split(cnames, rep(paste(rep("v",length(idvar)),c(1:length(idvar)),sep=""), infovar[,2]))
  #
  if (!is.null(varnames)) {
      if (is.numeric(varnames)) {
        stop("Names of variables must start with a letter.")
        }
      if (length(varnames) != (nrow(infovar)) ) {
        stop("number of variable names  not equal to number of variables") }
      names(valuenames) <- varnames
    }
  #
  #
  # 8. Construction of the result
  #
  y<-list(varnames = varnames, idvar = idvar, infovar = infovar, valuenames = valuenames) 
  return(y)
} 