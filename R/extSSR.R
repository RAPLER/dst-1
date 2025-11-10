#' Extension of a State Space Representation (SSR)
#' 
#' This function works on a marginal DSM \code{x} defined on a single variable. It allows the addition of new values to the SSR. 
#' @aliases extFrame
#' 
#' @param x An object of DSM class, i.e. a basic chance assignment defined on one SSR.
#' @param use_ssnames Default= FALSE. Put TRUE if use of subset names is wanted.
#' @param lab A character vector containing the names of the elements to add to the SSR.
#' @return zxtnd The DSM within its SSR extended.
#' @author Claude Boivin
#' @export
#' @examples 
#' s1_e1 <- DSM(tt = matrix(c(1,0,1,1),nrow = 2, byrow = TRUE), 
#' m = c(0.6,0.4), cnames = c("S1","S2"), varnames = "v1", idvar = 1) 
#' s13_names <- extSSR(s1_e1, lab = "S3", use_ssnames =TRUE)
#' s13 <- extSSR(s1_e1, lab = "S3")
#' s13$valuenames
  extSSR <- function(x, use_ssnames = FALSE, lab = NULL) {
  # 1. Parameter checks
  #
  if ( inherits(x, "DSMspec") == FALSE) {
    stop("Input x not of class DSMspec.")
  }
  if( (is.character(lab) == FALSE) ) { 
    stop("parameter lab must be a character string") 
  }
  zcomp=lapply(X=1:length(colnames(x$tt)), FUN = function(X) {colnames(x$tt)[X] == lab})
  zcompSum =unlist(lapply(X=1:length(zcomp), FUN = function(X) {sum(zcomp[[X]]) } ) )
  if(sum(zcompSum) > 0 ) {
    stop("One on more new labels already in the SSM. Labels cannot be reppeated.") 
  }
  #
  # 2. Computation
  zxtnd <- x
  zx <- matrix(rep(c(rep(0,-1+nrow(x$tt) ),1),length(lab)), ncol = length(lab) )
  names(zx) <- eval(lab)
  zxtnd$tt <- cbind(x$tt, zx)      # Adding a value "d" to the SSM and
  colnames(zxtnd$tt) <- c(colnames(x$tt), lab)
  # moving the ignorance from the Fod of x to the Fod of zxtnd 
  # update the size of the SSM
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
#' @rdname extSSR
#' @export
extFrame <- extSSR