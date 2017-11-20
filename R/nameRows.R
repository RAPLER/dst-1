#' Naming the rows of a matrix of focal elements
#' 
#'  This function uses the column names  of a matrix \code{f} of focal elements to construct the names of the rows.
#'  
#' @param f A matrix constructed in a boolean style (0,1) or a boolean matrix. The number of columns of the matrix must match the number of elements of the frame of discernment \eqn{\Theta}. See \code{\link{bca}} for details.
#' @return The result is a character vector of the names of the focal elements of the matrix \code{f}. The length of the result is \code{nrows(f)}. The last element of the vector is called "frame", to avoid to long a string. The empty set is named "u00f8". The "+" sign is used instead of the "|" to represent the logical "or" operation.
#' @author Claude Boivin, Stat.ASSQ
#' @examples 
#' f1 <- matrix(c(0,0,0,1,0,0,0,0,1,1,0,1,1,1,1),ncol=3, byrow = TRUE)
#' colnames(f1) <- c("A","B","C")
#' rownames(f1) <-nameRows(f1)
#' f1
#' @export
#' 
nameRows<-function(f) {
  # find non zero positions (identification of hypothesis)
  pos<-f*col(f) 
  names<-colnames(f)
  fun2 <- function(pos, names) {paste(names[pos],collapse=" + ")}
  idFocal <- apply(pos, 1, FUN = fun2, names=names)
  idFocal[length(idFocal)] <- "frame"
  if (sum((idFocal=="")*1:length(idFocal)) > 0) {
 # idFocal[(idFocal=="")*1:length(idFocal)] <- "ø"
    idFocal[(idFocal=="")*1:length(idFocal)] <- "\u00f8"
  }
  return(idFocal)
}
  