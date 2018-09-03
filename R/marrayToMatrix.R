#' Transformation of a m-array to a matrix-represented relation
#'
#'description
#' @param mtt the matrix tt of the relation in  multiarray format
#' @param relRef A relation of reference
#' @return tt a matrix.
#' @examples 
#' wr_tt <- matrix(c(0,1,rep(0,5),rep(c(1,0),2),1,1,0,1,0,
#' rep(1,3),0,1,0,rep(1,6)), ncol=4, byrow = TRUE)
#' colnames(wr_tt) <- c("rWdy Ry", "rWdy Rn", "rWdn Ry", "rWdn Rn")
#'  wr_spec = matrix(c(1:7, 0.0476, 0.7619, 0.1905, 0,0,0,0), 
#'  ncol = 2, dimnames = list(NULL, c("specnb", "mass"))) 
#'  wr_infovar = matrix(c(4,5,2,2), ncol = 2, 
#'  dimnames = list(NULL, c("varnb", "size")) )
#'  wr_rel <- list(tt=wr_tt, con=0.16, spec=wr_spec,
#'   infovar=wr_infovar, 
#'   infovaluenames= list( RdWorks=c("rWdy", "rWdn") , Rain=c("Ry", "Rn")))
#'  class(wr_rel)="bcaspec"
#'  mtt <- array(c(0,1,0,0,0,0,0,1,0,1,0,1,1,0,1,0,1,1,1,0,1,0,1,1,1,1,1,1), c(2,2,7), dimnames = list( RdWorks=c("rWdy", "rWdn") , Rain=c("Ry", "Rn"), ev=1:7))
marrayToMatrix <- function(mtt, infovar) {
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
      znamesCols <- dotprod(znamesCols, t(ci), "paste", "paste")  # pour dotprod des noms
      znamesCols <-t(matrix(t(znamesCols), ncol = prod(dim(znamesCols))))
    } }
  colnames(ztt) <- as.vector(znamesCols)
  # End naming cols
  
  rownames(ztt) <- nameRows(ztt)
  return(ztt)
}