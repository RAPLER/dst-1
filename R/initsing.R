#' Initialization of a frame of discernment
#' 
#' The purpose of this function is to initialize the basic chance assignment of all the singletons of a frame at a starting value of 0. Each singleton has a zero mass. The matrix of singletons is completed by the frame which receive a mass of one. One use of this function is to apply the plausibility transformation to a belief function and obtain its corresponding probability distribution (See \code{\link{plautrans}}). 
#' @param size The number of elements of the frame of discernment.
#' @param cnames A character vector of the names of the elements of the frame.
#' @param infovarnames (optional) A name for the variable can be supplied.
#' @return  An object of class "bcaspec".
#' @author Claude Boivin, Stat.ASSQ
#' @examples
#' initsing(3,cnames = c("a","b","c"))
#' initsing(3,cnames = c("a","b","c"), infovarnames = "choices")
#' @export
#' 
initsing<-function(size, cnames, infovarnames = NULL) { 
  x1<-diag(1,size)
  x2<-matrix(rep(1,times=size),nrow=1)
  f<-rbind(x1,x2)
  v<-matrix(c(rep(0,times=size),1),ncol=1)
  if (is.null(infovarnames)) {
    y<-bca(f,v,cnames=cnames)
  } else {
  y<-bca(f,v,cnames=cnames, infovarnames = infovarnames)
  }
  return(y)      
}