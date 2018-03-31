#' Representation of a belief function in a product space by its mass function
#'
#' A relation between two or more variables can be established in their product space and a mass function defined accordingly.
#' 
#' @param tt A (0,1) or logical matrix establishing the relation between two or more variables. The relation is described by a matrix input table of (0,1), where the  variables are put side by side, as in a truth table representation.
#' @param spec A two column matrix. First column contains subsets numbers. Second column contains the mass value associated with each subset of the relation. A subset number and its associated mass value are repeated to match the number of elements of the subset.
#' @param infovar  A two column matrix containing variable identification numbers and the number of elements of each variable. The identification numbers must be ordered in increasing number.
#' @param infovarnames The names of the variables. if omitted, variables are named v1, v2, etc.
#' @param relnb A number given to the relation. Set at 0 if omitted.
#' @return An object of class \code{bcaspec}. This is a list containing the following elements:  \itemize{
#' \item $con The measure of conflict. Set at 0 by default.
#' \item $tt The resulting table of focal elements. Rownames of the matrix of focal elements are created from the column names of the elements of the product frame.
#' \item $spec The resulting two column matrix of specification numbers with associated masses.
#' \item $infovar The two column matrix given in the input data.
#' \item infovaluenames A list of the names of the variables with the value name of each element.
#' \item $inforel A two column matrix containing variable numbers and the depth  (number of variables) of the relation.
#' } 
#' @author Claude Boivin, Stat.ASSQ
#' @export
#' @examples 
#' ## A logical implication rule
#' ## A typical relation between two variables is the
#'  logical implication rule (a -> b). let us suppose
#'   that a stands for Rain: {yes, no} and b stands for
#'    RoadWorks: {yes, no}. From experience in my region,
#'     I am 75 % sure that there will be RoadWorks if no Rain.
#' ## The truth table
#'  ttrwf= matrix(c(0,1,1,0,1,0,1,0,1,0,0,1,1,1,1,1),
#'  nrow=4, byrow = TRUE, 
#'  dimnames =list(NULL, c("rWdy", "rWdn", "Ry", "Rn")) )
#'  ## The mass distribution
#'  specrw = matrix(c(1,1,1,2,0.75,0.75,0.75,0.25), ncol = 2, 
#'  dimnames = list(NULL, c("specnb", "mass"))) 
#'  ## variables numbers and sizes
#'  inforw =matrix(c(4,5,2,2), ncol = 2, 
#'  dimnames = list(NULL, c("varnb", "size")) )
#' bcaRel(tt = ttrwf, spec = specrw, infovar = inforw,
#'  infovarnames = c("RdWorks", "Rain"), relnb = 6)
#'  
bcaRel <- function(tt, spec, infovar, infovarnames = NULL, relnb = NULL) {
  if ((is.matrix(spec) == FALSE) ) {
    stop("spec parameter must be a 2 columns matrix.")
  }
  if ((is.matrix(tt) ==FALSE) ) {
    stop("tt parameter must be a (0,1) or logical matrix.")
  }
  if ((is.matrix(infovar) ==FALSE) ) {
    stop("infovar parameter must be a 2 column numerical matrix with variables numbers in fist column and with sum of 2nd column = ncol(tt).")
  }
  if( (nrow(tt) != nrow(spec)) | (sum(infovar[,2]) != ncol(tt)) ){ 
    stop("Error in input arguments: check your input data.") 
  }
  # transform vector of mases
  #remove duplicates in each specification
  nbspec <- max(spec[,1])
  v <- spec[spec[,1]== 1,2]
  v <- v[!duplicated(v)]
  if (nbspec > 1) {
    for (i in 2:nbspec) {
      mi <- spec[spec[,1]== i,2]
      mi <- mi[!duplicated(mi)]
      v <- c(v, mi)
    }
  }  
  if (abs(sum(v)-1)>0.000001)  { 
    stop("Sum of masses not equal to 1 : check your data.") 
  }
  # fin tetst
  varnb <- (infovar)[,1]
  if (length(varnb) < 2) # No transfo if only 1 variable.
    { 
    zr <- bca(tt, (spec)[,2], cnames = colnames(tt), varnb = varnb)
    return(zr)
    } 
    else 
      {
    z1 <- productSpace(tt=tt, specnb = spec[,1], infovar=infovar) # representation in the product space
    colnz1 <-as.vector(colnames(z1))
    if (missing(relnb)) { relnb <- 0 }
    inforel <- matrix(c(relnb, length(varnb)), ncol = 2)
    colnames(inforel) <- c("relnb", "depth")
    # test
    valuenames <- split(colnames(tt), rep(paste(rep("v",length(varnb)),c(1:length(varnb)),sep=""), infovar[,2]))
    if (!missing(infovarnames)) {
      names(valuenames) <- infovarnames
    }
   zr <-bca(f = z1, m = v, cnames = colnz1, infovar = infovar, infovarnames = infovarnames, infovaluenames = valuenames, inforel = inforel)
   return(zr)
  }
}