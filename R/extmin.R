#' Extension of a relation
#'
#'This function works on a basic chance assignment (bca) \code{x} defined on a single variable or more. A relation of reference is given, and an extension of the space of \code{x} is made to the larger product space of the relation of reference. The basic chance assignment to extend and the relation of reference must have at least one common variable for the extension to occur. 
#' 
#' @param rel1 An object of class bcaspec, i.e. a basic chance assignment defined on one variable or a relation.
#' @param relRef The relation of reference. It can be an existing relation, or it can be constructed as a vacuous function. 
#' @details The \code{relRef} parameter is used to extract all the information on the variables, namely their identification numbers and the number of elements of each variable, variables names and columns names of the \code{tt} matrix. The relation of reference \code{relRef}  may be a relation already existing or simply the the vacuous relation defined on the product set of variables of interest.
#' 
#' @return the resulting extended bca.
#' @author Claude Boivin, Stat.ASSQ
#' @references G. Shafer and P. P. Shenoy. Local Computations in Hypertrees. School of Business, University of Kansas, Lawrence, KS, 1991. See p. 78, vacuous extension of a belief function.
#' @export
#' @examples
#' # Making a vacuous reference relation and extending a bca to its space.
#' init_tt = matrix(rep(1,10),nrow = 1, 
#' dimnames = list(NULL, c("3", "2", "1", "0", 
#'  "true", "false",  "foul", "fair",  "true", "false")) )
#'  init_spec <- matrix(c(1,1), ncol = 2, 
#'  dimnames = list(NULL, c("specnb", "mass")))
#'  init_info <- matrix(c(3,4,7,8,4,2,2,2), ncol = 2,
#'   dimnames = list(NULL, c("varnb", "size")) )
#'  relRef <- bcaRel(tt = init_tt, spec = init_spec,
#'   infovar = init_info, 
#'   varnames = c("Sail", "Loading", "Weather", "Repairs"),
#'   relnb = 0)
#'  # a bcaspec defined on one variable
#'  l_rel <- bca(tt = matrix(c(1,0,1,0,1,1), ncol = 2), 
#'  m = c(0.3,0.5,0.2), cnames = c("true", "false"), 
#'  infovar = matrix(c(4,2), ncol = 2, 
#'  dimnames = list(NULL, c("varnb", "size"))), 
#'  varnames = c("Loading"), 
#'  inforel = matrix(c(7,1), ncol = 2, 
#'  dimnames = list(NULL, c("relnb", "depth"))))
#'  z <- extmin(l_rel, relRef)
#'  prmatrix(t(z$tt), collab = rep("", nrow(z$tt)))
#'  
extmin <- function(rel1, relRef) {
  #
  # Local variables: 
  # infovar, varnb_ps, nbvar, varnb_rel1, lvars, ind_lvars, lvman, 
  # values1, values2, values_ck, nbval, values1_nb, values2_nb, values_nb_ck, nb_ck,
  # ind_lvman, cardrelRef, sizeToAdd, rev_sizeToAdd, 
  # zinit, za, zb,  names_zb, zorder, zzb1, zzb2, zc, rtt
  #
  # Functions calls: None
  #
  # A. Validation of input data
  # 1. inputs must be of class bcaspec
  if ( (inherits(rel1, "bcaspec") == FALSE) | (inherits(relRef, "bcaspec") == FALSE)) {
    stop("One or more inputs not of class bcaspec.")
  }
  #
  # 2. Stop if no variable common to the two relations
  #
  infovar <- relRef$infovar
  varnb_ps <- as.vector(infovar[,1]) # retrieve variables nb of the product space to obtain
  nbvar <- length(varnb_ps) 
  varnb_rel1 <- as.vector(rel1$infovar[,1])
  # find position of the variables of the relation
  lvars <- (is.element(varnb_ps, varnb_rel1))*varnb_ps
  if (sum(lvars) == 0) {
    stop("No common variable to the two relations. Check variable names and numbers.") 
  }
  #
  # 3. check that the space of the extension is greater than the space of rell1
  # and determine the missing variables
  #
  ind_lvars <- (lvars >0) * 1:length(lvars)
  lvman <- (!is.element(varnb_ps, varnb_rel1))*varnb_ps
  if (sum(lvman) == 0) {
    zr <- rel1
    return(zr)
    stop("There is no missing variable in this relation.")
  }
  #
  # 4. Stop if the names of the variables are missing,
  #
  if (is.null(names(rel1$valuenames)) | (is.null(names(relRef$valuenames)))  ){
    stop("Names of variables missing. Check your inputs.")
  }
  #
  # 5 Verifiy that the variables names of rel1 are also in relRef
  #
  values1 <- names(rel1$valuenames)
  values2 <- names(relRef$valuenames)
  values_ck <- outer(values1, values2, FUN="==")
  nbval=as.integer(sum(rowSums(values_ck, dims = 1)))
  if (nbval < length(values1)) {
    stop("Variables names of rel not in relRef. Check variables names.")
  }
  #
  # 6. Check that names of variables and their numbers are correct.
  values1_nb <- rel1$infovar[,1]
  values2_nb <- relRef$infovar[,1]
  values_nb_ck <- outer(values1_nb, values2_nb, FUN="==")
  nb_ck = values_nb_ck == values_ck
  if (sum(nb_ck) < prod(dim(nb_ck))) {  
    stop("Variables names and variabless numbers do not match. Check variables names, numbers and their position.")
  }
  #
  # B. Calculations
  #
  # 0. define some working variables
  #
  ind_lvman <- (lvman >0) * 1:length(lvman)
  cardrelRef <- as.vector(infovar[,2])
  sizeToAdd <- cardrelRef[ ind_lvman]
  rev_sizeToAdd <- sizeToAdd[length(sizeToAdd):1]
  #
  # 1: initialize data array
  #  
  zinit=array(1,rev_sizeToAdd) 
  dimnames(zinit)=relRef$valuenames[ind_lvman[nbvar:1]] 
  # end initialize
  #  
  # 2. restructure tt matrix of rel1 in product space form
  #
  za <- matrixToMarray(tt = rel1$tt, valuenames = rel1$valuenames)
  #
  # 3: extend the relation (compute extension)
  #
  zb <- 1*outer(zinit, za, FUN = "&") # OK
  #  
  # 4: reorder the variables
  #  
  names_zb=names(dimnames(zb))[1:nbvar]
  zorder=names(relRef$valuenames)
  zzb1=outer(names_zb, zorder, FUN = "==")
  zzb2=apply(zzb1*1:length(zorder), 2, sum)
  zc <-aperm(zb, c(zzb2, 1+nbvar)) # OK 
  # end reorder
  #  
  # 4: obtain final tt matrix
  #  
  rtt <- marrayToMatrix(zc)
  #
  # result  
  zr <-list(con = rel1$con, tt = rtt, spec = rel1$spec, infovar = infovar, varnames = relRef$varnames, valuenames= relRef$valuenames, inforel = relRef$inforel)
  class(zr) <- append(class(zr), "bcaspec")
  return(zr)
}