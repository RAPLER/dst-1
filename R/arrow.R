#' Find upset, downset of a set among a lattice
#' 
#' @param x A binary vector encoding an element of a poset
#' @param M Binary matrix encoding the lattice from which the supset or downset is found
#' @param method Return supremum or infimum
#' @return b Binary vector of sup or inf
#' @author Peiyuan Zhu
#' @export
#' @examples 
arrow<-function(x,M,method=NULL){
  if(method=="up") {
    s <- M[apply(t(t(M)>=x), 1, all),]
  } else if (method=="down") {
    s <- M[apply(t(t(M)<=x), 1, all),]
  } else {
    stop("method must be one of up, down")
  }
  return(s)
}