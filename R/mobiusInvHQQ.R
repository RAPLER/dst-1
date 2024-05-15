#' Mobius inversion of commonality function
#' @details
#' @param
#' @return
#' @author
#' @import 
#' @importClassesFrom 
#' @export
#' @examples 
#' 
mobiusInvHQQ<-function(qq,h){
  m <- 0
  # go through all supersets as h union a combination of elements from the complement
  for (i in 0:(2**(length(h) - sum(h)) - 1)) {
    hh <- h
    dhh <- encode(rep(2, length(h) - sum(h)), i)
    hh[which(h == 0)] <- dhh
    m <- m + (-1) ** sum(dhh) * qq(hh)
  }
  return(m)
}