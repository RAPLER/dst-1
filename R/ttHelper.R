#' helper to the construction of the *tt* matrix of a *bca*
#' 
#' The tt matrix is a (0,1) or boolean matrix. As the frame of discernment increases, it becomes more and more difficult to define the subsets by sequences of (0,1) - (true, false). Function *ttHelper* tries to facilitate this task. 
#' 
#' @param nvar number of variables. Default = 1.
#' @param x A list... each element , the positions of 1 (TRUE) columns from 1 (left) to number of elements (right)
#' @param card For the single variable case. The number of elements of the frame of discernment. Maximum 1023.
#' @param cnames For the single variable case. A character vector containing the names of the elements of the frame of discernment \eqn{\Theta}. The length must be equal to the number of elements of \eqn{\Theta}. The names are first searched in the \code{valuenames} parameter. If NULL, column names of the matrix \code{tt}.
#' @param binary Logical parameter. TRUE if multiple binary variables. Default = FALSE.
#' @param formula A valid propositional formula. For the multiple binary variables only.
#' @return A tt matrix.
#' @author Claude Boivin
#' @export
#' @examples 
#' # Single variable case
#' ym2 <- list(c(1,5,6), c(2,3,5,6), c(4), c(3,5), c(1,2,5,4), c(1:6) )
#' clabels <- c("a", "b", "c", "d", "e", "f")
#' ym2_enc <- ttHelper(ym2, card = 6, cnames = clabels)
#' rownames(ym2_enc) <- nameRows(ym2_enc)
#' ym2_enc
#' # multiple binary variables examples
#' union <- ttHelper(nvar = 2, binary = TRUE, formula = c("Peter | Mary"))
#' rownames(union) <- nameRows(union)
#' union
#' # Implication example
#' ifRainNoRoadworks <- ttHelper(nvar = 2, binary = TRUE, formula = c("!Rain | RdW") )
#' rownames(ifRainNoRoadworks) <- nameRows(ifRainNoRoadworks)
#' ifRainNoRoadworks
#' 
ttHelper<-function(x, card, cnames, nvar = 1, binary = FALSE, formula = NULL) { 
  #
  # Local variables: x_enc
  # Functions calls: encode truth_table
  #
  ## 1. Checks
  # 1.1. x must be a list
  # 1.2.card must be integer > 0
  # 1.2.1 card must be < 1024.
  # 1.3 Length of all list members must be <= card
  # 1.4. All list values must be integers between 1 and card
  # 1.5. if cnames not NULL, length of columns labels must equel card
  # 2. Calculations
  if (binary == FALSE) {
    x_enc <- lapply(x, FUN = function(x) {encode(rep(2,card), sum(2^(card-x) ) )})
    x_tt <- matrix(unlist(x_enc),ncol = card, byrow = TRUE)
    colnames(x_tt) <- cnames
  } 
  # 
  # Relation between binary variables
  if (binary == TRUE) {
  cat("Defining the relation ", formula,"\n")
 
    tt0 <- truth_table(formula)
    tt1 <- tt0[(tt0[, (1+nvar)] == TRUE),1:nvar ]
    tt2 <- cbind(tt1, !tt1)
    order <- matrix(1:(2*nvar), ncol = nvar, byrow = TRUE)
    tt <- tt2[,as.vector(order)]
    tt <- rbind(tt, rep(1,(2*nvar)))
    x_tt <- as.matrix(tt)
    if (is.null(cnames)) {
      colnames(x_tt) <- rep(c("true", "false"),nvar )
    }
    else {
      colnames(x_tt) <- cnames
    }
  rownames(x_tt) <- nameRows(x_tt)  
  }
  return(x_tt)
}