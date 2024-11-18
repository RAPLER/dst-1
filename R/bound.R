#' Find inf, sup of a lattice
#' 
#' @param x  Binary matrix encoding the lattice from which the supremum or infimum is found
#' @param method Return supremum or infimum
#' @return b Binary vector of sup or inf
#' @author Peiyuan Zhu
#' @export
#' @examples 
#' M <- matrix(c(0,1,1,1,1,1),nrow=2)
#' x <- t(as.matrix(c(0,1,0)))
#' bound(M,"inf")
bound<-function(x, method=NULL) {
  if(!is.matrix(x)) {
    stop("x must be a matrix")
  }
  if(method=="inf") {
    b <- apply(x, 2, min)
  } else if (method=="sup") {
    b <- apply(x, 2, max)
  } else {
    stop("method must be one of inf, sup")
  }
  return(b)
}