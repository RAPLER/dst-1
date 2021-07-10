#' Transformation of the tt matrix of a relation
#'
#' The matrix representation of a relation is converted to the array representation or product space representation.
#' @param tt A (0,1)-matrix or a boolean matrix establishing the relation between two or more variables. The matrix is constructed by placing the variables side by side, as in a truth table representation.
#' @param valuenames A list of the names of the variables with the name of each value of their frame of discernment.
#' @return mtt The array (product space) representation of the tt matrix.
#' @author Claude Boivin, Stat.ASSQ
#' @export
#' @examples
#' # Define  wr_tt, a matrix describing the relation between two variables
#' wr_tt <- matrix(c(1,rep(0,3),rep(c(1,0),3),0,1,1,1,0,0,
#' 1,0,rep(1,5),0,1,1,0,rep(1,5)), ncol=4, byrow = TRUE)
#' colnames(wr_tt) <- c("Wy Ry", "Wy Rn", "Wn Ry", "Wn Rn")
#' rownames(wr_tt) <- nameRows(wr_tt)
#' vars= list( RdWorks = c("Wy", "Wn") , Rain = c("Ry", "Rn"))
#' print(zmToa <- matrixToMarray(tt = wr_tt, valuenames = vars ) )
#'  
matrixToMarray <- function(tt, valuenames) {
  #
  # 0. Local variables: size, nbvar, mtt0
  #
  # 1. Define working variables and Check input data
  #
  size <- lengths(valuenames)
  nbvar <- length(size)
  if ( ncol(tt) != prod(size) ) {
    stop("Product of size of variables not equal to number of columns of tt matrix.")
  }
  # 2. Calculations
  #
  mtt0 <- array(as.vector(t(tt)), c(size[nbvar:1],nrow(tt)), dimnames = c((valuenames)[nbvar:1], list(ev=1:nrow(tt))))  # OK. Variables must be put in reverse order
  mtt <- aperm(mtt0, perm = c(nbvar:1, nbvar+1))
  return(mtt)
}
