#' Naming the rows of a matrix of focal elements
#' 
#'  This function uses the column names  of a matrix \code{f} of focal elements to construct the names of the rows.
#'  
#' @param f A boolean matrix or a matrix constructed in a boolean style (0,1). The names of the columns are the elements of a frame of discernment \eqn{\Theta}.
#' @return The result is a character vector of the names of the focal elements of the matrix \code{f}. The length of the result is \code{nrows(f)}. The set of all elements is called "frame", to avoid to long a string. The empty set is named "u00f8". The "+" sign is used instead of the "|" to represent the logical "or" operation.
#' @author Claude Boivin, Stat.ASSQ
#' @examples 
#' f <- matrix(c(0,0,0,1,0,0,0,0,1,1,0,1,1,1,1),ncol=3, byrow = TRUE)
#' colnames(f) <- c("A","B","C")
#' rownames(f) <-nameRows(f)
#' f
#' f2 <- matrix(c(0,0,0,1,0,0,0,0,1,1,0,1),ncol=3, byrow = TRUE)
#' colnames(f2) <- c("A2","B2","C2")
#' rownames(f2) <-nameRows(f2) 
#' f2
#' @export
#' 
nameRows<-function(f) {
  if (is.matrix(f) == FALSE) {
    stop("Input is not a matrix.")
  }
  if ( sum((f >1) > 0) > 0) {
    stop("Input is not a logical or (0,1) matrix.")
  }
  f <- f > 0
  # find non zero positions (identification of hypothesis)
  names<-colnames(f)
  if (is.null(names) == TRUE) {
    stop("No column names supplied.")
  }
  pos<-f*col(f) 
  n_colnames <- apply(pos >0, 1,"sum")
  fun2 <- function(pos, names) {paste(names[pos],collapse=" + ")}
  idFocal <- apply(pos, 1, FUN = fun2, names=names)
  if (sum((idFocal=="")*1:length(idFocal)) > 0) {
    idFocal[(idFocal=="")*1:length(idFocal)] <- "\u00f8" # "ø" code
  }
  if (sum(nnames_idFocal == ncol(pos)) > 0) {
    idFocal[(n_colnames== ncol(pos))*(1:length(idFocal))] <- "frame"
  }
  return(idFocal)
}
  