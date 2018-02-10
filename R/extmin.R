#' Extension of a relation
#'
#' This function works on a relation rel1 to extend it to the product space of a set of variables. This set of variables must contais at least one of the variables in rel1 for the extension to be made possible.
#' 
#' @param rel1 A relation, an object of class bcaspec.
#' @param relRef a relation of reference to extract the varables names and columns names of the tt matrix.
#' @details The relation of reference relRef may simply be an empty relation defined on the set of variables of interest or a relation already defined. The relRef parameter normally contains all the information on the variables, namely their identification numbers and the number of elements of each variable ($infovar parameter). The relRef relation also contains the names of the variables and of the columns of the tt matrix.
#' 
#' @return R the resulting extended relation.
#' @author Claude Boivin, Stat.ASSQ
#' @export
#' @examples
#' # making an empty reference relation with m(frame) = 1
#' init_tt= matrix(rep(1,10),nrow=1, dimnames =list(NULL, c("0", "1", "2", "3", "true", "false", "foul", "fair", "true", "false")) )
#'  init_spec <- matrix(c(1,1), ncol = 2, dimnames = list(NULL, c("specnb", "mass")))
#'  init_info <- matrix(c(2,4,5,6,4,2,2,2), ncol = 2, dimnames = list(NULL, c("varnb", "size")) )
#'  relRef <- bcaRel(tt = init_tt, spec = init_spec, infovar = init_info, infovarnames = c("Delay", "Loading", "Forecast", "Maintenance"), relnb = 0)
#'  # a bcaspec defined on one variable
#'  l_rel <- bca(f=matrix(c(1,0,1,0,1,1), ncol=2), m=c(0.3,0.5,0.2), cnames=c("true", "false"), infovar=matrix(c(4,2), ncol = 2, dimnames = list(NULL, c("varnb", "size"))), infovarnames= c("Loading"), inforel= matrix(c(7,1), ncol = 2, dimnames = list(NULL, c("relnb", "depth"))))
#'  extmin(l_rel, relRef)
#'  
extmin <- function(rel1, relRef) {
  # inputs validation
  if ( (inherits(rel1, "bcaspec") == FALSE) | (inherits(relRef, "bcaspec") == FALSE)) {
    stop("One or more inputs not of class bcaspec.")
  }
  # stop if no variable common to the two relations
  infovar <- relRef$infovar
  varnb_ps <- as.vector(infovar[,1]) # variables nb of the product space to obtain
  varnb_rel1 <- as.vector(rel1$infovar[,1])
  # position of the variables of the relation
  lvars <- (is.element(varnb_ps, varnb_rel1))*varnb_ps
  if (sum(lvars) == 0) {
    stop("no common variable to the two relations.") 
  }
  # check that the space of the extension is greater than the space of rell1
  # determine the missing variables
  ind_lvars <- (lvars >0) * 1:length(lvars)
  lvman <- (!is.element(varnb_ps, varnb_rel1))*varnb_ps
  if (sum(lvman) == 0) {
    stop("No missing variable. Check your inputs.")
  }
  ind_lvman <- (lvman >0) * 1:length(lvman)
  cardrelRef <- as.vector(infovar[,2])
  sizeToAdd <- cardrelRef[ ind_lvman]
  rev_sizeToAdd <- sizeToAdd[length(sizeToAdd):1]
  # extent the relation
  zinit=array(1,rev_sizeToAdd)
  z <- outer(zinit, t(rel1$tt), FUN = "&") ## revoir pour array
  ## reorder the variables
  actualOrder <- c(ind_lvars, ind_lvman)
  actualOrder <- actualOrder[actualOrder>0]
  v <- varnb_ps[actualOrder]
  v=v[length(v):1]
  v=order(v)
  indices <- v[length(v):1]  # indices to order the variables of the product space
  nbvar <- length(varnb_ps) 
  z1 <-aperm(z, c(indices, 1+nbvar))
  rtt <- matrix(z1, nrow = nrow(rel1$tt), ncol = prod(dim(z1)[-length(dim(z1))]) )
  ## il faut des noms de colonnes et de ligne
  colnames(rtt) <- colnames(relRef$tt)
  rownames(rtt) <- nameRows(rtt)
  zr <-list(tt = rtt, spec = rel1$spec, infovar = infovar, infovaluenames= relRef$infovaluenames, inforel = rel1$inforel)
  class(zr) <- append(class(zr), "bcaspec")
  return(zr)
}