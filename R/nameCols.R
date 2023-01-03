#' Naming the columns of the \code{tt} matrix
#' 
#' This utility function makes use of the valuenames  and size parameters  of a set of variables to assign values names to the columns of a \code{tt} matrix.
#'  
#' @param valuenames A list of the names of the variables with the name of the elements of their frame of discernment.
#' @param size A vector of the size of the variables.
#' @return A character vector of length equal to the sum of the sizes of the variables.
#' @author Claude Boivin
#' @examples 
#' infoval <- list(A = c("a1", "a2"), B = c("b1", "b2", "b3"))
#' sizes <- c(2,3)
#'  print(nameCols(valuenames = infoval, size = sizes) )
#' @export
nameCols <- function(valuenames, size) {
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
  idnames <- names(valuenames)
  znamesCols <- matrix(valuenames[[1]], ncol = 1)
  if (length(idnames) > 1) {
    for (i in 2:nbvar) {
      ci <-matrix(valuenames[[i]], ncol = 1)
      znamesCols <- c(znamesCols, ci)
    }
  }
  #
  return(znamesCols)
 }