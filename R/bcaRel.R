#' Representation of a mass function in a product space
#'
#' This function is used to represent a relation between two or more variables in their product space \code{P}. The relation can be described by more than one subset of \code{P}. Each subset can also  include more than one element. Complete disjunctive coding is used to represent one element in the input matrix of the function.
#' 
#' @param tt The description matrix of the subsets establishing the relation. This matrix is obtained by putting the variables side by side, as in a truth table representation. For each variable, there are as many columns as possible values. Each row of the matrix is an element of a subset. Each element is described by a sequence of 0 (absence of value of a variable) or 1 (presence of value). This forms a complete disjunctive coding.
#' @param spec A two column matrix. First column: numbers assigned to the sub-assemblies. Second column: the mass values of the sub-assemblies. If the subset has more than one element, the number of the subset and its associated mass value are repeated to match the number of elements in the subset.
#' @param infovar  A two column matrix containing variable identification numbers and the number of elements of each variable. The identification numbers must be ordered in increasing number.
#' @param varnames The names of the variables.
#' @param infovarnames Deprecated. Old name for \code{varnames}.
#' @param valuenames A list of the names of the variables with the name of the elements of their frame of discernment.
#' @param infovaluenames Deprecated. Old name for \code{valuenames}. 
#' @param relnb A number given to the relation. Set at 0 if omitted.
#' @return An object of class \code{bcaspec} called a bca for "basic chance assignment". This is a list containing the following components:  \itemize{
#' \item con The measure of conflict.
#' \item tt The resulting table of subsets. Rownames of the matrix of subsets are generated from the column names of the elements of the product frame. See \code{\link{nameRows}} for details.
#' \item spec The resulting two-column matrix of specification numbers with associated mass values.
#' \item infovar The two-column matrix of variables number and size given in the input data.
#' \item valuenames A list of the names of the variables with the name of the elements of their frame of discernment.
#' \item inforel A two-column matrix containing the relation number and the depth (number of variables) of the relation.
#' } 
#' @author Claude Boivin
#' @export
#' @examples
#' # A logical implication rule
#' # A typical relation between two variables is the
#' # logical implication a -> b. Let us suppose
#' # that a stands for Rain: {yes, no} and b stands for
#' # Roadworks: {yes, no}. From experience,
#' # I am 75 % sure that there will be RoadWorks if there is no rain.
#' 
#'  # 1. The tt table of the logical implication
#'  ttrwf <- matrix(c(0,1,1,0,1,0,1,0,1,0,0,1,1,1,1,1),
#'  nrow = 4, byrow = TRUE, 
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
  # Functions calls: productSpace, bca
  #
  # 1. Catch old parameters names, if anay and replace by the new ones
  #
  calls <- names(sapply(match.call(), deparse))[-1]
  if(any("infovarnames" %in% calls) & missing(varnames)) {
    warning("Parameter name 'infovarnames' is deprecated. Use 'varnames' instead.")
    varnames <- infovarnames
  }
  if(any("infovaluenames" %in% calls) & missing(valuenames)) {
    warning("Parameter name 'infovaluenames' is deprecated. Use 'valuenames' instead.")
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
  if (missing(varnames)) { 
    stop("varnames argument missing.") 
  }
  # End checks
  #
  # 3. Transform mass vector
  # remove duplicates in each specification to test the sum of masses
  #
  # correction 2023-03-19 Changement du 15 dec 22 pas bon
  v<- spec[!duplicated(spec),2] ## remove duplicates 
  #      
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
   zr <-bca(tt = z1, ssnames = NULL, sfod = NULL, m = v, cnames = colnz1, infovar = infovar, varnames = varnames, valuenames = valuenames, inforel = inforel)
   return(zr)
  }
}