#' Transformation of an array data to its matrix representation
#'
#'The array representation or product space representation is converted to the matrix representation of the corresponding relation.
#' @param mtt The matrix \code{tt} of the relation in  array format
#' @return The matrix representation of the data.
#' @author Claude Boivin, Stat.ASSQ
#' @export
#' @examples 
#'  mtt <- array(c(1,0,0,0,1,1,0,0,1,0,0,1,1,0,1,0,1,1,0,1,1,1,1,0,1,0,1,1,1,1,1,1), c(2,2,8), 
#'  dimnames = list( RdWorks=c("Wy", "Wn") , Rain = c("Ry", "Rn"), ev=1:8) )
#'  print(z <- marrayToMatrix(mtt))
#' 
marrayToMatrix <- function(mtt) {
  #
  # 0. Local variables: nbvar, dims, var_to_keep, ztt0
  #
  # # 1. Check input data
  #
  if ( sum(class(mtt) == c("matrix", "array") ) == 0) {
    stop("Input is not an array or a matrix.")
  }
  #
  # 2. Processing
  #
  # 2.1. Initialize result
  #
  nbvar <- -1+length(dim(mtt))
  dims <- dim(mtt)[-length(dim(mtt))]
  var_to_keep <- 1:(length(dim(mtt))-1)
  ztt0 <- aperm(mtt, perm = c(nbvar:1, nbvar+1))
  ztt <- t(matrix(ztt0, ncol = dim(mtt)[length(dim(mtt))], nrow = prod(dims) ))
  #
  # 2.2. add names the columns of the tt matrix
  #
 znamesCols <- nameCols_prod(valuenames =  dimnames(mtt)[var_to_keep] , size = dims)
  colnames(ztt) <- as.vector(znamesCols)
  # End naming cols
  #
  # 3. Final result
  rownames(ztt) <- nameRows(ztt)
  return(ztt)
}