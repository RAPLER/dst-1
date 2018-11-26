#' Transformation of an array data to a matrix-represented relation
#'
#'The array representation or product space representation is converted to the matrix representation of the corresponding relation.
#' @param mtt The matrix tt of the relation in  multiarray format
#' @param infovar specification of the number and size of each variable
#' @return tt The matrix representation of the data.
#' @author Claude Boivin, Stat.ASSQ
#' @export
#' @examples 
#'  wr_infovar = matrix(c(4,5,2,2), ncol = 2, 
#'  dimnames = list(NULL, c("varnb", "size")) )
#'  mtt <- array(c(0,1,0,0,0,0,0,1,0,1,0,1,1,0,1,0,1,1,1,0,1,0,1,1,1,1,1,1), c(2,2,7), 
#'  dimnames = list( RdWorks=c("rWdy", "rWdn") , Rain=c("Ry", "Rn"), ev=1:7))
#' z <- marrayToMatrix(mtt, wr_infovar)
#' 
marrayToMatrix <- function(mtt, infovar) {
  # Check input data
  if (nrow(infovar) != -1+length(dim(mtt)) ) {
    stop("Number of varables of the two parameters not compatible.")
  }
  if (sum(infovar[,2] != dim(mtt)[-length(dim(mtt))]) > 0 ) {
    stop("Dimensions not compatible.")
  }
  # Calculations
  nbvar <- dim(infovar)[1]
  ztt0 <- aperm(mtt, perm = c(nbvar:1, nbvar+1))
  ztt <- t(matrix(ztt0, ncol = dim(mtt)[length(dim(mtt))], nrow = prod(infovar[,2])))
  # naming the columns of tt matrix
  var_to_keep <- 1:(length(dim(mtt))-1)
  idnames <- names(dimnames(mtt))[var_to_keep]
  znamesCols <- matrix(dimnames(mtt)[[var_to_keep[1]]], ncol = 1)
  if (length(idnames) > 1) {
    for (i in 2:length(idnames)) {
      ci <-matrix(dimnames(mtt)[[var_to_keep[i]]], ncol = 1)
      znamesCols <- dotprod(znamesCols, t(ci), "paste", "paste")  # for dotprod of names
      znamesCols <-t(matrix(t(znamesCols), ncol = prod(dim(znamesCols))))
    } }
  colnames(ztt) <- as.vector(znamesCols)
  # End naming cols
  rownames(ztt) <- nameRows(ztt)
  return(ztt)
}