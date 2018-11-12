#' Using the column names of a matrix to construct names for the rows
#' 
#' This function determines the name of a row from all the columns that have a 1 for that row.
#'  
#' @param f A (0,1)-matrix or a boolean matrix.
#' @return The result is a character vector of the labels proposed for the rows of the matrix \code{f}. The length of the result is \code{nrow(f)}. 
#' @details The row containing all one's is called "frame", to avoid too long a character string. The empty set is named "u00f8". The "+" sign is used to represent the logical "or" operator. The space " " is used to represent the logical "and" operator. Note that in the case of a matrix representing a product space definition on many variables, row labels can be pretty long.
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
    idFocal[(idFocal=="")*1:length(idFocal)] <- "\u00f8" # UTF-8 code for empty set
  }
  if (sum(n_colnames == ncol(pos)) > 0) {
    idFocal[(n_colnames== ncol(pos))*(1:length(idFocal))] <- "frame"
  }
  return(idFocal)
}
  