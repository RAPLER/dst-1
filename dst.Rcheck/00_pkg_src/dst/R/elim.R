#'  Reduction of a relation
#' 
#' This function works on a relation defined on a product of two variables or more.  Having fixed a variable to eliminate from the relation,  the reduced product space is determined and the corresponding reduced bca is computed.This operation is also called "marginalization".
#' @param rel The relation to reduce, an object of class bcaspec.
#' @param xnb Number of variable to eliminate.
#' @return The reduced relation
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
#'   infovaluenames= list(Rain=c("Ry", "Rn"), RdWorks=c("rWdy", "rWdn") ))
#'  class(wr_rel)="bcaspec"
#'  elim(wr_rel, xnb = 5)
#'  elim(wr_rel, xnb = 4)
#'  
#'  mrt_tt <- matrix(c(1,0,1,0,0,1,1,0,0,1,0,1,1,0,0,1,0,1,1,0,0,1,0,1,rep(1,4)), 
#'  ncol=4, byrow = TRUE)
#' colnames(mrt_tt) <- c("t6", "f6", "t8", "f8")
#'  mrt_spec = matrix(c(1,1,1,2,2,2,3, 0.1, 0.1, 0.1, 0.7,0.7,0.7,0.2), 
#'  ncol = 2, dimnames = list(NULL, c("specnb", "mass"))) 
#'  mrt_infovar =matrix(c(6,8,2,2), ncol = 2, 
#'  dimnames = list(NULL, c("varnb", "size")) )
#'  mrt_rel <- bcaRel(tt=mrt_tt, spec=mrt_spec, 
#'  infovar=mrt_infovar, 
#'  infovarnames= c("Maintenance", "Repair") )
#'  elim(mrt_rel, xnb = 6)
#'  elim(mrt_rel, xnb = 8)
elim <- function(rel, xnb) {
  if (inherits(rel, "bcaspec") == FALSE)  {
    stop("Rel input not of class bcaspec.")
  }
  size_vars <- rel$infovar[,2]    # vector of the sizes of axes
  nbvar <- length(size_vars)
  if (nbvar < 2)  {
    stop("Input is not a relation. No variable to eliminate.")
  }
  size_vars_inv <- size_vars[nbvar:1]          # same order as APL prog
  varnb <- rel$infovar[,1]         # numbers of the variables 
  varnb_inv <- varnb[nbvar:1]          # same order as APL prog
  varRank <- match(xnb, varnb_inv)       # rank of the variable
  if(is.na(varRank)) {
    stop("Invalid variable number.")
  }
   dim_to_keep <- (1:length(varnb_inv))*!varnb_inv %in% (xnb) # axes to keep
  n <- nrow(rel$tt)
  m <- as.vector(unlist(rel$spec[,2]) )   # extract mass vector
  itab <-array(t(rel$tt), c(size_vars_inv,n)) # restructure data
  fun3 <- function(xnb, oper) {reduction(xnb, f=oper)}
  proj <- apply(itab, c(dim_to_keep,(1+nbvar)), FUN= fun3, oper= "|")
  # transformer proj en matrice
  z2<- t(matrix(as.vector(proj), ncol=n, nrow =prod(dim(proj)[-length(dim(proj))])))
  w1 <- doubles(z2*1) # mult by 1 transforms from logical to numeric
  I12 <- dotprod(w1, t(z2), g = "&", f = "==")
  m1<- t(array(m,c(ncol(I12), nrow(I12))))
  m1 <- apply(I12*m1, 1, sum)
  tt=w1
# naming the columns of tt matrix
  var_to_keep <- varnb*!varnb %in% (xnb)
  var_to_keep <- var_to_keep[var_to_keep>0]
  varRank_to_keep <- match(var_to_keep, varnb)
  idnames <- names(rel$infovaluenames)[varRank_to_keep]
  znamesCols <- matrix(rel$infovaluenames[[varRank_to_keep[1]]], ncol = 1)
if (length(idnames) > 1) {
  for (i in 2:length(idnames)) {
    ci <-matrix(rel$infovaluenames[[varRank_to_keep[i]]], ncol = 1)
    znamesCols <- dotprod(znamesCols, t(ci), "paste", "paste")  # pour dotprod des noms
    znamesCols <-t(matrix(t(znamesCols), ncol = prod(dim(znamesCols))))
  } }
  colnames(tt) <- as.vector(znamesCols)
# End naming cols
# infovar parameter
 infovar <- matrix(rel$infovar[varRank_to_keep,], ncol=2)
# infovaluenames parameter
  infovaluenames <- rel$infovaluenames[varRank_to_keep]
# inforel parameter
  inforel <- rel$inforel
  r <- bca(f = tt, m = m1, cnames = rownames(w1), con = rel$con, infovar = infovar, infovarnames = idnames, infovaluenames = infovaluenames, inforel = inforel)
return(r)
}
