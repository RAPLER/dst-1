#' Representation of a mass function in a product space
#'
#' This function is used to represent a mass function which establish a relation between two or more variables in their product space.
#' 
#' @param tt A (0,1)-matrix or a boolean matrix establishing the relation between two or more variables. The matrix is constructed by placing the variables side by side, as in a truth table representation.
#' @param spec A two-column matrix. First column: numbers given to the subsets. Second column: the mass values of the subsets. A subset number and its associated mass value are repeated to match the number of elements of the subset.
#' @param infovar  A two column matrix containing variable identification numbers and the number of elements of each variable. The identification numbers must be ordered in increasing number.
#' @param varnames The names of the variables. If omitted, variables are named \code{v1, v2, etc}.
#' @param relnb A number given to the relation. Set at 0 if omitted.
#' @return zr An object of class \code{bcaspec} called a bca for "basic chance assignment". This is a list containing the following components:  \itemize{
#' \item con The measure of conflict.
#' \item tt The resulting table of subsets. Rownames of the matrix of subsets are generated from the column names of the elements of the product frame. See \code{\link{nameRows}} for details.
#' \item spec The resulting two-column matrix of specification numbers with associated mass values.
#' \item infovar The two-column matrix of variables number and size given in the input data.
#' \item valuenames A list of the names of the variables with the name of the elements of their frame of discernment.
#' \item inforel A two-column matrix containing the relation number and the depth (number of variables) of the relation.
#' } 
#' @author Claude Boivin, Stat.ASSQ
#' @export
#' @examples
#' # A logical implication rule
#' # A typical relation between two variables is the
#' # logical implication a -> b. Let us suppose
#' # that a stands for Rain: {yes, no} and b stands for
#' # Roadworks: {yes, no}. From experience,
#' # I am 75 % sure that there will be RoadWorks if there is no rain.
#' 
#' #' # 1. The tt table of the logical implication
#'  ttrwf <- matrix(c(0,1,1,0,1,0,1,0,1,0,0,1,1,1,1,1),
#'  nrow=4, byrow = TRUE, 
#'  dimnames = list(NULL, c("rWdy", "rWdn", "Ry", "Rn")) )
#'  
#'  # 2. The mass distribution
#'  specrw <-  matrix(c(1,1,1,2,0.75,0.75,0.75,0.25), ncol = 2, 
#'  dimnames = list(NULL, c("specnb", "mass")))
#'   
#'  # 3. Variables numbers and sizes
#'  inforw <- matrix(c(4,5,2,2), ncol = 2, 
#'  dimnames = list(NULL, c("varnb", "size")) )
#' bcaRel(tt = ttrwf, spec = specrw, infovar = inforw,
#'  varnames = c("RdWorks", "Rain"), relnb = 6)
#'
  bcaRel <- function(tt, spec, infovar, varnames, valuenames, relnb = NULL, infovarnames, infovaluenames) {
  #
  # Local variables: v, z1, colnz1, 
  # Functions calls: doubles, productSpace, bca
  #
  # 1. Catch old parameters names, if anay and replace by the new ones
  #
  calls <- names(sapply(match.call(), deparse))[-1]
  if(any("infovarnames" %in% calls) & missing(varnames)) {
    varnames <- infovarnames
  }
  if(any("infovaluenames" %in% calls) & missing(valuenames)) {
    valuenames <- infovaluenames
  }
  #
  # 2. checks
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
  if (is.null(colnames(tt))) { 
    stop("Column names of tt matrix are missing.") 
  }
  # End checks
  #
  # 3. Transform mass vector
  # remove duplicates in each specification to test the sum of masses
  #
  v <- doubles(spec)[,2]
  if (abs(sum(v)-1)>0.000001)  { 
    stop("Sum of masses not equal to 1 : check your data.") 
  }
  #
  # 4. case for one variable only
  varnb <- (infovar)[,1]
  if (length(varnb) < 2) # No transfo if only 1 variable.
    { 
    zr <- bca(tt, (spec)[,2], cnames = colnames(tt), varnb = varnb)
    return(zr)
    } 
    else
        {
        #
        # 5. Case for two or more variables    
        #  
    z1 <- productSpace(tt=tt, specnb = spec[,1], infovar=infovar) # representation in the product space
    colnz1 <-as.vector(colnames(z1))
    #
    # inforel parameter
    #
    if (missing(relnb)) { relnb <- 0 }
    inforel <- matrix(c(relnb, length(varnb)), ncol = 2)
    colnames(inforel) <- c("relnb", "depth")
    #
    # valuenames  and varnames parameters
    #
    valuenames <- split(colnames(tt), rep(paste(rep("v",length(varnb)),c(1:length(varnb)),sep=""), infovar[,2]))
    if (!missing(varnames)) {
      names(valuenames) <- varnames
    }
    #
    # Result
    #
   zr <-bca(tt = z1, m = v, cnames = colnz1, infovar = infovar, varnames = varnames, valuenames = valuenames, inforel = inforel)
   return(zr)
  }
}