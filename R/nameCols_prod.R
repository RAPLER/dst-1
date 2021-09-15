#' Naming the columns of the tt matrix of a product space
#' 
#' This utility function makes use of the valuenames  and size parameters  of a set of variables to assign values names to the columns of the tt matrix of their product space.
#'  
#' @param valuenames A list of the names of the variables with the name of the elements of their frame of discernment.
#' @param size A vector of the size of the variables.
#' @return A character vector of length equal to the product of the sizes of the variables
#' @author Claude Boivin, Stat.ASSQ
# @examples 
#' infoval <- list(A = c("a1", "a2"), B = c("b1", "b2", "b3"))
#' sizes <- c(2,3)
#'  print(nameCols_prod(valuenames = infoval, size = sizes) )
#' @export
nameCols_prod <- function(valuenames, size) {
  #
  # 0. Local variables: nbvar, idnames, i, ci
  #
  # 1. Some checks
  #
  if (is.list(valuenames) == FALSE) {
    stop("first parameter not a list.")
  }
  if (is.numeric(size) == FALSE) {
    stop("Parameter size must be numeric.")
  }
  #
  # 2. Processing
  #
  nbvar <- length(size)
  var_to_keep <- 1:nbvar
  idnames <- names(valuenames) # retrieve names of variables
  znamesCols <- matrix(valuenames[[var_to_keep[1]]], ncol = 1)
  if (length(idnames) > 1) {
    for (i in 2:length(idnames)) {
      ci <-matrix(valuenames[[var_to_keep[i]]], ncol = 1)
      znamesCols <- dotprod(znamesCols, t(ci), "paste", "paste") # for dotprod of names
      znamesCols <-t(matrix(t(znamesCols), ncol = prod(dim(znamesCols))))
    } 
  }
  #
  return(znamesCols)
 }