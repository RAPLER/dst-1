#'  Reduction of a relation
#' 
#' This function works on a relation defined on a product of two variables or more.  Having fixed a variable to eliminate from the relation,  the reduced product space is determined and the corresponding reduced bca is computed.This operation is also called "marginalization".
#' @param rel The relation to reduce, an object of class bcaspec.
#' @param xnb Identification number of the variable to eliminate.
#' @return r The reduced relation
#' @author Claude Boivin
#' @export
#' @examples  
#' # We construct a relation between two variables to show marginalization.
#' wr_tt <- matrix(c(1,rep(0,3),rep(c(1,0),3),0,1,1,1,0,0,
#' 1,0,rep(1,5),0,1,1,0,rep(1,5)), ncol = 4, byrow = TRUE)
#' colnames(wr_tt) <- c("Wy Ry", "Wy Rn", "Wn Ry", "Wn Rn")
#' rownames(wr_tt) <- nameRows(wr_tt)
#' wr_spec = matrix(c(1:8, 0.017344, 0.046656, 
#' 0.004336, 0.199456,0.011664,0.536544,0.049864, 0.134136), 
#'  ncol = 2, dimnames = list(NULL, c("specnb", "mass"))) 
#'  wr_infovar = matrix(c(4,5,2,2), ncol = 2, 
#'  dimnames = list(NULL, c("varnb", "size")) )
#' wr_rel <- list(tt = wr_tt, con = 0.16, spec=wr_spec,
#'   infovar = wr_infovar, varnames = c("Roadworks","Rain"),
#'   valuenames = list( RdWorks = c("Wy", "Wn"), Rain=c("Ry", "Rn") ))
#' class(wr_rel) <- "bcaspec"
#' bcaPrint(elim(wr_rel, xnb = 5))
#' bcaPrint(elim(wr_rel, xnb = 4))
#'  
elim <- function(rel, xnb) {
  #
  # Local variables: size_vars, nbvar, size_vars_inv, varnb, varnb_inv, varRank, dim_to_keep, n, m, itab, fun3, proj, var_to_keep, varRank_to_keep, z2, w1, I12, m1, idnames
  # Functions calls: matrixToMarray, marrayToMatrix, dotprod, bca
  #
  # 1. Checks and Some working vars
  #
  if (inherits(rel, "bcaspec") == FALSE)  {
    stop("Rel input not of class bcaspec.")
  }
  size_vars <- rel$infovar[,2]    # vector of the sizes of axes
  nbvar <- length(size_vars)
  if (nbvar < 2)  {
    stop("Input is not a relation. No variable to eliminate.")
  }
  # Some working vars
  size_vars_inv <- size_vars[nbvar:1]   # same order as APL prog
  varnb <- rel$infovar[,1]         # numbers of the variables 
  varnb_inv <- varnb[nbvar:1]   
  varRank <- match(xnb, varnb_inv)       # rank of the variable
  if(is.na(varRank)) {
    stop("Invalid variable number.")
  }
  # End checks
  #
  # 2. extract mass vector and obtain projection
  #
  dim_to_keep <- (1:length(varnb))*!varnb %in% (xnb)
  n <- nrow(rel$tt)
  m <- as.vector(unlist(rel$spec[,2]) )   # extract mass vector
  itab <- matrixToMarray(rel$tt, valuenames = rel$valuenames)
  fun3 <- function(x, oper) {Reduce(x, f=oper)} # projection
  proj <- apply(itab, c(dim_to_keep,(1+nbvar)), FUN= fun3, oper= "|")
  #
  # 3. transform projection array to tt matrix
  # uses utility functions "marrayToMatrix", "dotprod"
  #
  # 3.1. Define infovar parameter
  var_to_keep <- varnb*!varnb %in% (xnb)
  var_to_keep <- var_to_keep[var_to_keep>0]
  varRank_to_keep <- match(var_to_keep, varnb)
  infovar <- matrix(rel$infovar[varRank_to_keep,], ncol=2)
  #
  # 3.2. convert array to matrix
  z2 <- marrayToMatrix(proj)
  #
  w1<- (z2*1)[!duplicated(z2*1),]  # Remove duplicate rows
  if (is.matrix(w1) == FALSE) {
    w1 <- t(as.matrix(w1))
  }
  #
  I12 <- dotprod(w1, t(z2), g = "&", f = "==")
  m1<- t(array(m,c(ncol(I12), nrow(I12))))
  m1 <- apply(I12*m1, 1, sum)
  tt=w1
  idnames <- names(rel$valuenames)[varRank_to_keep]
  #
  # 4. naming the columns of tt matrix. Already done
  #
  # 5. Other parameters
  #
  # valuenames parameter
  valuenames <- rel$valuenames[varRank_to_keep]  
  # inforel parameter
  inforel <- rel$inforel
  #
  # 6. Result
  r <- bca(tt = tt, m = m1,  con = rel$con, infovar = infovar, varnames = idnames, valuenames = valuenames, inforel = inforel)
return(r)
}