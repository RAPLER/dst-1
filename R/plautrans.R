#' Plausibility transformation applied on the distribution of singletons
#'
#' The plausibility transformation is used to obtain the probability distribution associated to a belief function. 
#' @param x A belief function in its bca form or the normalized result of the combination of two or more belief functions (see \code{\link{nzdsr}}.
#' @details First, we compute the belief and plausibility measures on all the singletons of the frame of discernment. The probability distribution of the singletons is derived from the plausibility measures of the singletons.
#' @return The matrix of singletons with the plausibility transformation added in last column.
#' @author Claude Boivin, Stat.ASSQ
#' @references Cobb, B. R. and Shenoy, P.P. (2006). On the plausibility transformation method for translating belief function models to probability models. Journal of Approximate Reasoning, 41(3), April 2006, 314--330.
#' @examples  
#' x <- bca(f=matrix(c(0,1,1,1,1,0,1,1,1),nrow=3, byrow = TRUE), m=c(0.2,0.5, 0.3), cnames =c("a", "b", "c"), n=1)
#' plautrans(x)
#' @export
#' 
plautrans <- function(x) {
# add all the singletons to the input bca
  nc <-ncol(x$combination) - 1
  x <- addTobca(x, diag(1, nc))
#  eliminate duplicates singletons if any  
  x <- nzdsr(x)
# compute measures of belief and plausibility  
  zx<-rbind(tabresul(x, singletonsOnly = TRUE)$mbp)
  nsing<- -4+ncol(zx)
  z1<-apply(rbind(zx[,1:nsing, drop = FALSE]),1,sum)
  zs<-rbind(zx[z1==1,])
  # test ordonner les singletons pour que les rownames correspondent
  zzs <- rbind(zs[,1:nsing, drop = FALSE])
  zord <- sapply(1:ncol(zzs),FUN = function(x) {decode(rep(2,ncol(zzs)), zzs[x,])})
  zs <- zs[order(zord,  decreasing = TRUE),]
#  rownames(zs) <- colnames(zx)[1:(dim(zx)[2]-4)]
  # calculate distribution
  trplau<-zs[,nsing+3]/sum(zs[,nsing+3])
  y<-cbind(zs[,1:nsing],trplau)
  return(y)
}