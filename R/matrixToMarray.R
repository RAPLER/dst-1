#' Transformation of the tt matrix of a relation
#'
#' The matrix representation of a relation is converted to the array representation or product space representation.
#' @param rel An object of class bcaspec, i.e. a mass function of one variable or a relation.
#' @return mtt The array (product space) representation of the tt matrix.
#' @author Claude Boivin, Stat.ASSQ
#' @export
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
#' z <- matrixToMarray(wr_rel)
#'  
matrixToMarray <- function(rel) {
  # Check input data
  if ( inherits(rel, "bcaspec") == FALSE) {
    stop("Input argument not of class bcaspec.")
  }
  # Calculations
  nbvar <- dim(rel$infovar)[1]
  mtt0 <- array(as.vector(t(rel$tt)), c((rel$infovar[,2])[nbvar:1],nrow(rel$tt)), dimnames = c((rel$infovaluenames)[nbvar:1], list(ev=1:nrow(rel$tt)))) # OK. Variables must be in reverse order
  mtt <- aperm(mtt0, perm = c(nbvar:1, nbvar+1))
  return(mtt)
}