#' Representation of a belief function in a product space
#'
#' When a relation between two or more variables is established, a product space representation of this relation can be done. The relation is described with a matrix input table of (0,1), where the different spaces are side by side, as a truth table.
#' 
#' @param tt The (0,1) matrix establishing the relation between two or more variables.
#' @param spec A two column matrix. First column contains specification numbers. Second column contains the mass associated with each element of the relation.
#' @param infovar  A two column matrix containing variable identification numbers and the number of elements of each variable.
#' @param infovarnames The names of the variables. if omitted, variables are named v1, v2, etc.
#' @param relnb A number given to the relation. 
#' @return An object of class \code{bcaspec}. This is a list containing the following elements:  \itemize{
#' \item combination A table of focal elements tt with the addition of the column of associated masses. Rownames of the matrix of focal elements are created from the column names of the elements of the product frame.
#' \item $con The measure of conflict. Set at 0 by default.
#' \item $tt The resulting table of focal elements alone. 
#' \item $spec A two column matrix. First column contains specification numbers. Second column contains the mass vector. If the same specification number is repeated (e.g.  a logical implication) the mass value is also repeated.
#' \item $infovar A two column matrix containing variable identification numbers and the size of the frame of discernment of each variable.
#' \item infovaluenames A list of the names of the variables with the value name of each element.
#' \item $inforel. A two column matrix containing variable numbers and the depth of the relation.
#' } 
#' @author Claude Boivin, Stat.ASSQ
#' @export
#' @examples 
#' ## A logical implication table.
#' ## A typical relation between two variables is the logical implication (a -> b) or its conditional probability equivalent (b | a). let us suppose a stands for Rain (yes, no) and b stands for Road Works (yes, no). From our experience, we are 75 % sure that there will be Road Works if no rain.
#' ## The truth table
#'  ttrwf= matrix(c(0,1,1,0,1,0,1,0,1,0,0,1,1,1,1,1),nrow=4, byrow = TRUE, dimnames =list(NULL, c("rWdy", "rWdn", "Ry", "Rn")) )
#'  ## The mass distribution
#'  specrw = matrix(c(1,1,1,2,0.75,0.75,0.75,0.25), ncol = 2, dimnames = list(NULL, c("specnb", "mass"))) 
#'  ## variables numbers and sizes
#'  inforw =matrix(c(4,5,2,2), ncol = 2, dimnames = list(NULL, c("varnb", "size")) )
#' bcaRel(tt = ttrwf, spec = specrw, infovar = inforw, infovarnames = c("RdWorks", "Rain"), relnb = 6)
#'  
bcaRel <- function(tt, spec, infovar, infovarnames = NULL, relnb = NULL) {
  # pour transformer les relations originales
  # xyz est une relation décrite selon le format initial
  # zr est la relation représentée dans un espace produit
  # appelle les fonctions PRODUIT et DOUBLES
  # no des variables
 # zmat <- as.matrix(xyz) # le type "list" n'est pas traité
 # zdims<-matrix(zmat[1,-c(1,2)],ncol = 1)
 # varnb<-zdims[!duplicated(zdims)] # enlever les doubles
  varnb <- (infovar)[,1]
  if (length(varnb) < 2) { 
    zr <- bca(tt, (spec)[,2], cnames = colnames(tt), n = varnb)
    return(zr)
    } # pas de transfo si 1 var seulement
    else {
    xyz <- list(tt=tt, spec=spec, infovar=infovar)
    z1 <- productSpace(xyz) # représentation dans l'espace produit
    # vecteur des masses des ss-ensembles, sans les doubles
    v <- (spec)[,2] 
 #   v <- v[!duplicated(v[,1]),2]
    v <- v[!duplicated(v)]
 #   zr <-rbind(0,z1)
    colnz1 <-colnames(z1)
    if (missing(relnb)) { relnb <- 0 }
    inforel <- matrix(c(relnb, length(varnb)), ncol = 2)
    colnames(inforel) <- c("relnb", "depth")
    if (missing(infovarnames)) {
      valuenames <- split(colnames(tt), rep(paste(rep("v",length(varnb)),c(1:length(varnb)),sep=""), infovar[,2]))
    } else {
 # test
      valuenames <- split(colnames(tt),rep(infovarnames, infovar[,2]))
      #     valuenames <- split(colnames(tt),rep(infovarnames, infovar[,2]))
      ## fin test
    }
   zr <-bca(f = z1, m = v, cnames = colnz1, infovar = infovar, infovarnames = infovarnames, infovaluenames = valuenames, inforel = inforel)
   return(zr)
  }
}