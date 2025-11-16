#' Multidimensional DS Model
#'
#' Function \code{jointDSM} is used to assign mass values to subsets defined in a finite multidimensional state space \eqn{P}, which is the product space of unidimensional spaces \eqn{\Theta_1}, \eqn{\Theta_2}, ..., \eqn{\Theta_n}. To achieve the definition of the joint DSM of a focal element, a relation between two or more unidimensional SSR is described by one or more subsets of \eqn{\Theta_1}, \eqn{\Theta_2}, ..., \eqn{\Theta_n}. Hence each subset can possibly include more than one element. Complete disjunctive coding is used to represent a subset in the input matrix of the function.
#' 
#' @aliases bcaRel
#' @param tt The description matrix of the subsets establishing the relation between \eqn{\Theta_1}, \eqn{\Theta_2}, ..., \eqn{\Theta_n}. This matrix is made of one or more subsets and the whole SSR in the last row. The columns of the matrix are defined by putting the SSRs side by side, ordered by their *idvar* from left to right. The first columns  are the labels of the first SSR, followed by the labels of the second SSR till the end. For each SSR, there are as many columns as there is possible values. A subset is then defined in the way of a truth table representation, where we keep the "TRUE" part of the table. Hence each row of the table is an element of a subset, described by a sequence of 0 (absence of value) or 1 (presence of value). This is called a complete disjunctive coding. CAUTION: SSRs put side by side must be ordered by their *idvar* from left to right.
#' 
#' @param spec A two column matrix. First column: numbers assigned to the subsets. Second column: the mass values of the subsets. If a subset has more than one element (row), the number of the subset and its associated mass value are repeated to match the number of elements (rows) of the subset.
#' @param infovar  A two column matrix containing SSR identification numbers and the number of elements of each SSR. The identification numbers must be ordered in increasing number.
#' @param varnames The names of the SSR.
#' @param valuenames A list of the names of the SSR with the name of the elements of their SSR.
#' @param relnb A number given to the jointDSM. Set at 0 if omitted.
#' @return An object of class \code{DSMspec} called a DSM or "basic mass assignment". This is a list containing the following components:  \itemize{
#' \item con The measure of conflict.
#' \item tt The resulting table of subsets in its joint statte Space representation. Rownames of the matrix of subsets are generated from the column names of the elements of the product space. See \code{\link{nameRows}} for details.
#' \item spec The resulting two-column matrix of specification numbers with associated mass values.
#' \item infovar The two-column matrix of SSR number and size given in the input data.
#' \item valuenames A list of the names of the SSR with the name of their elements.
#' \item inforel A two-column matrix containing the relation number and the number of SSRs of the relation.
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
#' # 0. Define State space representations
#' ssWorks <- SSR(varnames = "W", idvar = 4, size = 2, cnames = c("Wy", "Wn") )
#' ssRain <- SSR(varnames = "R", idvar = 5, size = 2, cnames = c("Ry", "Rn") )
#' 
#'  # 1. The tt table of the logical implication
#'  ttrwf <- matrix(c(0,1,1,0,1,0,1,0,1,0,0,1,1,1,1,1),
#'  nrow = 4, byrow = TRUE, 
#'  dimnames = list(NULL, c("Wy", "Wn", "Ry", "Rn")) )
#'  
#'  # 2. The mass distribution
#'  specrw <-  matrix(c(1,1,1,2,0.75,0.75,0.75,0.25), ncol = 2, 
#'  dimnames = list(NULL, c("specnb", "mass")))
#'   
#'  # 3. Variables Identification Numbers and sizes (put VIN in ascending order)
#'  inforw - matrix(c(ssWorks$infovar, ssRain$infovar), ncol =2, byrow = TRUE,  dimnames = list(NULL, c("varnb", "size")) )
#'  
#' rel <- bcaRel(tt = ttrwf, spec = specrw, infovar = inforw,
#'  varnames = c("RdWorks", "Rain"), relnb = 6)
#'
  jointDSM <- function(tt, spec, infovar, varnames, valuenames, relnb = NULL) {
  #
  # Local variables: v, z1, colnz1, 
  # Functions calls: productSpace, DSM
  #
    .Deprecated("jointDSM", msg = "jointDSM is the new function name for the bcaRel function.", old = "bcaRel")
  # 1. Catch old parameters names, if anay and replace by the new ones
  #
  # 2. checks
  if ((is.matrix(spec) == FALSE) ) {
    stop("spec parameter must be a 2 columns matrix.")
  }
  if ((is.matrix(tt) ==FALSE) ) {
    stop("tt parameter must be a (0,1) or logical matrix.")
  }
  if (sum(tt[nrow(tt),]) != ncol(tt) ) {
    stop("The last row of parameter tt must be a row of ones.")
  }
  if ((is.matrix(infovar) ==FALSE) ) {
    stop("infovar parameter must be a 2 column numerical matrix with variables numbers in fist column and with sum of 2nd column = ncol(tt).")
  }
  # verify that varaibles numbers are in ascending order
  if ( sum(order(infovar[,1]) == 1:nrow(infovar) ) != nrow(infovar) ) {
    stop("Error in tt matrix: Variables put side by side must be so that variable identification numbers of infovar parameter are in ascending order.") 
  }
  #
  if( (nrow(tt) != nrow(spec)) | (sum(infovar[,2]) != ncol(tt)) ){ 
    stop("Error in input arguments: check your input data.") 
  }
  if (is.null(colnames(tt))) { 
    stop("Column names of tt matrix are missing.") 
  }
  if (missing(varnames)) { 
    stop("varnames argument missing.") 
  }
  if (!is.null(varnames)) {
    if (is.numeric(varnames)) {
      stop("Names of variables must start with a letter.")
    }
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
    zr <- DSM(tt, (spec)[,2], cnames = colnames(tt), idvar = varnb)
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
    rownames(infovar) <- varnames
    #
    # Result
    #
   zr <-DSM(tt = z1, m = v, cnames = colnz1, infovar = infovar, varnames = varnames, valuenames = valuenames, inforel = inforel)
   return(zr)
  }
  }
  #' @rdname jointDSM
  #' @export
  bcaRel <- jointDSM
  