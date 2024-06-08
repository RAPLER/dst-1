#' Mobius inversion of commonality function
#' 
#' @param qq Commonality function
#' @param h Hypothesis to be evaluated
#' @return m Mass of the hypothesis
#' @author Peiyuan Zhu
#' @export
#' @examples 
#' x <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, byrow = TRUE),
#' m = c(0.2,0.5, 0.3), cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
#' qq <- commonality(x$tt,x$spec[,2])
#' mobiusInvHQQ(qq, matrix(c(0,1,0,1,1,0), nrow = 2, byrow = TRUE))
mobiusInvHQQ<-function(qq,h){
  m <- 0
  i <- 0
  # go through all supersets as h union a combination of elements from the complement
  while (i <= (2**(length(h) - sum(h)) - 1)) {
    hh <- h
    dhh <- encode(rep(2, length(h) - sum(h)), i)
    hh[which(h == 0)] <- dhh
    m <- m + (-1) ** sum(dhh) * qq(hh)
    i <- i + 1
  }
  return(m)
}