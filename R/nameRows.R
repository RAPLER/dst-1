#' Combining the column names of a matrix to construct names for the rows
#' 
#' This function determines the name of a row from all the columns of the \code{tt} that show 1 for that row.
#'  
#' @param tt A (0,1)-matrix or a boolean matrix.
#' @param f Deprecated. Old name for \code{tt} matrix.
#' @return A character vector of labels obtained for the rows of the \code{tt} matrix. The length of the result is \code{nrow(tt)}. 
#' @details The row containing only 1s is called "frame", to avoid too long a label. The empty set is identified by its code "u00f8". The "+" sign is used to represent the logical "or" operator. The space " " is used to represent the logical "and" operator. Note that in the case of a product space defined on many variables, row labels can become very long.
#' @author Claude Boivin
#' @examples 
#' tt <- matrix(c(0,0,0,1,0,0,0,0,1,1,0,1,1,1,1),ncol = 3, byrow = TRUE)
#' colnames(tt) <- c("A","B","C")
#' rownames(tt) <-nameRows(tt)
#' tt
#' @export
#' 
nameRows<-function(tt, f) {
  #
  # Local variables: pos, n_colnames, fun2
  # Functions calls: None
  #
  # 1. Catch old parameters names, if anay and replace by the new ones
  #
  # catch old parameter f and replace by tt if used instead of tt
  calls <- names(sapply(match.call(), deparse))[-1]
  if(any("f" %in% calls) & missing(tt)) {
    tt <- f
  }
  #
  # 2. Parameter checks
  #
  # Input is matrix?
  if ((is.matrix(tt) == FALSE) & (isS4(tt) == FALSE) ) {
    stop("Input is not a matrix.")
  }
  if ( sum((tt >1) > 0) > 0) {
    stop("Input is not a logical or (0,1) matrix.")
  }
  # Columns names present?
  tt <- tt > 0
  # find non zero positions (identification of hypothesis)
  names<-colnames(tt)
  if (is.null(names) == TRUE) {
    warning("No column names supplied. Column names are generated.")
    names <- colnames(tt, do.NULL = FALSE, prefix = "col")
  }
  #
  # 3. Calculations
  #
  pos<-tt*col(tt) 
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
  