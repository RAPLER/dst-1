#' Extension of the frame of discernment of a variable
#' 
#' This function works on a basic chance assignment (bca) \code{x} defined on a single variable. Iy=t allows the addition of new values to the frame of discernment. 
#' 
#' @param x An object of bca class, i.e. a basic chance assignment defined on one variable
#' @param use_ssnames Default= FALSE. Put TRUE if use of subset names is wanted.
#' @param lab A character vector containing the names of the elements to add to the frame of discernment.
#' @return zxtnd The bca with its frame extended
#' @author Claude Boivin
#' @export
#' @examples 
#' s1_e1 <- bca(tt = matrix(c(1,0,1,1),nrow = 2, byrow = TRUE), 
#' m = c(0.6,0.4), cnames = c("S1","S2"), varnames = "v1", idvar = 1) 
#' s13_names <- extFrame(s1_e1, lab = "S3", use_ssnames =TRUE)
#' s13 <- extFrame(s1_e1, lab = "S3")
extFrame <- function(x, use_ssnames = FALSE, lab = NULL) {
  # 1. Parameter checks
  #
  if ( inherits(x, "bcaspec") == FALSE) {
    stop("Input x not of class bcaspec.")
  }
  if( (is.character(lab) == FALSE) ) { 
    stop("parameter lab must be a character string") 
  }
  zcomp=lapply(X=1:length(colnames(x$tt)), FUN = function(X) {colnames(x$tt)[X] == lab})
  zcompSum =unlist(lapply(X=1:length(zcomp), FUN = function(X) {sum(zcomp[[X]]) } ) )
  if(sum(zcompSum) > 0 ) {
    stop("One on more new labels already in the frame. Labels cannot be reppeated.") 
  }
  #
  # 2. Computation
  zxtnd <- x
  zx <- matrix(rep(c(rep(0,-1+nrow(x$tt) ),1),length(lab)), ncol = length(lab) )
  names(zx) <- eval(lab)
  zxtnd$tt <- cbind(x$tt, zx)      # Adding a value "d" to the frame and
  colnames(zxtnd$tt) <- c(colnames(x$tt), lab)
  # moving the ignorance from the Fod of x to the Fod of zxtnd 
  # update the size of the frame
  zxtnd$infovar[,"size"] <- length(lab)+x$infovar[,"size"]
  # update the valuenames
  zxtnd$valuenames[[1]] <- colnames(zxtnd$tt)
  #
  # Update ssnames
  if ( (use_ssnames == TRUE) | is.null(zxtnd$ssnames) == FALSE ){
    n <- length(x$ssnames)
    zxtnd$ssnames[[n]] <- colnames(zxtnd$tt)
  }
  return(zxtnd)
}