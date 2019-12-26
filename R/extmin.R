#' Extension of a relation
#'
#'This function works on a mass function defined on a single variable or a relation defined onto a group of two variables or more. An extension of their space is made to a larger product space of a relation of reference. The mass function or relation to extend and the relation of reference must have at least one common variable for the extension to be made possible. 
#' 
#' @param rel1 An object of class bcaspec, i.e. a mass function of one variable or a relation.
#' @param relRef The relation of reference. It can be an existing relation, or it can be constructed as a vacuous function. 
#' @details The \code{relRef} parameter is used to extract all the information on the variables, namely their identification numbers and the number of elements of each variable, variables names and columns names of the tt matrix. The relation of reference \code{relRef}  may simply be an empty relation defined on the set of variables of interest or a relation already defined.
#' 
#' @return R the resulting extended relation.
#' @author Claude Boivin, Stat.ASSQ
#' @references G. Shafer and P. P. Shenoy. Local Computations in Hypertrees. School of Business, University of Kansas, Lawrence, KS, 1991. See p. 78, vacuous extension of a belief function.
#' @export
#' @examples
#' # Making an empty reference relation with mass(frame) = 1 and
#' # extending a bca to its space.
#' init_tt= matrix(rep(1,10),nrow=1, 
#' dimnames =list(NULL, c("3", "2", "1", "0", 
#'  "true", "false",  "foul", "fair",  "true", "false")) )
#'  init_spec <- matrix(c(1,1), ncol = 2, 
#'  dimnames = list(NULL, c("specnb", "mass")))
#'  init_info <- matrix(c(3,4,7,8,4,2,2,2), ncol = 2,
#'   dimnames = list(NULL, c("varnb", "size")) )
#'  relRef <- bcaRel(tt = init_tt, spec = init_spec,
#'   infovar = init_info, 
#'   infovarnames = c("Sail", "Loading", "Weather", "Repairs"),
#'   relnb = 0)
#'  # a bcaspec defined on one variable
#'  l_rel <- bca(f=matrix(c(1,0,1,0,1,1), ncol=2), 
#'  m=c(0.3,0.5,0.2), cnames=c("true", "false"), 
#'  infovar=matrix(c(4,2), ncol = 2, 
#'  dimnames = list(NULL, c("varnb", "size"))), 
#'  infovarnames= c("Loading"), 
#'  inforel= matrix(c(7,1), ncol = 2, 
#'  dimnames = list(NULL, c("relnb", "depth"))))
#'  z <- extmin(l_rel, relRef)
#'  prmatrix(t(z$tt), collab = rep("", nrow(z$tt)))
#'  
extmin <- function(rel1, relRef) {
  # A. Validation of input data
  # 1. inputs must be of class bcaspec
  if ( (inherits(rel1, "bcaspec") == FALSE) | (inherits(relRef, "bcaspec") == FALSE)) {
    stop("One or more inputs not of class bcaspec.")
  }
  # 2. Stop if no variable common to the two relations
  infovar <- relRef$infovar
  varnb_ps <- as.vector(infovar[,1]) # variables nb of the product space to obtain
  nbvar <- length(varnb_ps) 
  varnb_rel1 <- as.vector(rel1$infovar[,1])
  # position of the variables of the relation
  lvars <- (is.element(varnb_ps, varnb_rel1))*varnb_ps
  if (sum(lvars) == 0) {
    stop("no common variable to the two relations.") 
  }
  # 3. check that the space of the extension is greater than the space of rell1
  # determine the missing variables
  ind_lvars <- (lvars >0) * 1:length(lvars)
  lvman <- (!is.element(varnb_ps, varnb_rel1))*varnb_ps
  if (sum(lvman) == 0) {
    stop("No missing variable. Check your inputs.")
  }
  # 4 Stop if names of variables are missing,
  if (is.null(names(rel1$infovaluenames)) | (is.null(names(relRef$infovaluenames)))  ){
    stop("Names of variables missing. Check your inputs.")
  }
  # B. Calculations
  ind_lvman <- (lvman >0) * 1:length(lvman)
  cardrelRef <- as.vector(infovar[,2])
  sizeToAdd <- cardrelRef[ ind_lvman]
  rev_sizeToAdd <- sizeToAdd[length(sizeToAdd):1]
  # 1: initialize data array
  zinit=array(1,rev_sizeToAdd) 
  dimnames(zinit)=relRef$infovaluenames[ind_lvman[nbvar:1]] 
  # end initialize
  # 2. restructure tt matrix of rel1 in product space
  za <- matrixToMarray(rel1)
  # 3: extent the relation 
  # compute extension
  zb <- 1*outer(zinit, za, FUN = "&") # OK
  # 4: reorder the variables
  names_zb=names(dimnames(zb))[1:nbvar]
  zorder=names(relRef$infovaluenames)
  zzb1=outer(names_zb, zorder, FUN = "==")
  zzb2=apply(zzb1*1:length(zorder), 2, sum)
  zc <-aperm(zb, c(zzb2, 1+nbvar)) # OK 
  # end reorder
  # 4: obtain final tt matrix
  rtt <- marrayToMatrix(zc, infovar = relRef$infovar)
  zr <-list(con = rel1$con, tt = rtt, spec = rel1$spec, infovar = infovar, infovaluenames= relRef$infovaluenames, inforel = relRef$inforel)
  class(zr) <- append(class(zr), "bcaspec")
  return(zr)
}