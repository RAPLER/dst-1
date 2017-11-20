#' Initialization of a frame of discernment
#' 
#' The purpose of this function is to initialize the basic chance assignment of all the singletons of a frame at a starting value of 0. Each singleton has a zero mass. The matrix of singletons is completed by the frame which receive a mass of one. One use of this function is to apply the plausibility transformation to a belief function and obtain its corresponding probability distribution (See \code{\link{plautrans}}). 
#' @param size The number of elements of the frame of discernment.
#' @param names a character vector of the names of the elements of the frame.
#' @return  A list in bca form, namely a matrix of n+1 rows for the n singletons and the whole set. The measure of conflict and node number are set at 0.
#' @author Claude Boivin, Stat.ASSQ
#' @examples
#' initsing(3,c("a","b","c"))
#' @export
#' 
initsing<-function(size, names) { 
  x1<-diag(1,size)
  x2<-matrix(rep(1,times=size),nrow=1)
  f<-rbind(x1,x2)
  v<-matrix(c(rep(0,times=size),1),ncol=1)
  y<-bca(f,v,cnames=names)
  return(y)      
}