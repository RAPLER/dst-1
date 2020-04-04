#' Plausibility transformation of the singletons of a frame
#'
#' Given a mass function defined on some subsets of a frame \eqn{\Theta}, the application of the plausibility transformation to the singletons of \eqn{\Theta} yields the probability distribution associated with the this mass function. 
#' @param x A bca mass function.
#' @details We compute the plausibility measure of all the singletons of the frame of discernment. The probability distribution of the singletons is obtained from their plausibility measures.
#' @return The matrix of singletons with their plausibility transformation added in the last column.
#' @author Claude Boivin, Stat.ASSQ
#' @references Cobb, B. R. and Shenoy, P.P. (2006). On the plausibility transformation method for translating belief function models to probability models. Journal of Approximate Reasoning, 41(3), April 2006, 314--330.
#' @examples  
#' x <- bca(f=matrix(c(0,1,1,1,1,0,1,1,1),nrow=3, 
#' byrow = TRUE), m=c(0.2,0.5, 0.3), 
#' cnames =c("a", "b", "c"), 
#' infovarnames = "x", varnb = 1)
#' plautrans(x)
#' @export
#' 
plautrans <- function(x) {
  # checking input data 
  # 1. Input must be of class bcaspec
  if ( inherits(x, "bcaspec") == FALSE) {
    stop("Input argument not of class bcaspec.")
  }
  # 2. check if m_empty present and if not 0
  if (sum((apply(x$tt, 1, sum)) == 0) > 0) {
    row_m_empty <- match(1:nrow(x$tt), rownames(x$tt) == "\u00f8")
    row_m_empty <- row_m_empty[1]
    if (!is.na(row_m_empty)) {
      if (x$spec[row_m_empty,2] > 0) {
        stop("Invalid data: Empty set among the focal elements. Normalization necessary. See nzdsr function.")
      }
    }
  }
# Calculations  
# add all the singletons to the input bca
  nc <-ncol(x$tt)
  x <- addTobca(x, diag(1, nc))
#  eliminate duplicates singletons if any  
  x <- nzdsr(x)
# compute measures of belief and plausibility  
  zx<-rbind(tabresul(x, singletonsOnly = TRUE)$mbp)
  nsing <- -4+ncol(zx)
  z1<-apply(rbind(zx[,1:nsing, drop = FALSE]),1,sum)
  zs<-rbind(zx[z1==1,])
  # put singletons in same order as rownames
  zzs <- rbind(zs[,1:nsing, drop = FALSE])
  zord <- sapply(1:ncol(zzs),FUN = function(x) {decode(rep(2,ncol(zzs)), zzs[x,])})
  zs <- zs[order(zord,  decreasing = TRUE),]
  # calculate probability distribution of singletons
  trplau<-zs[,nsing+3]/sum(zs[,nsing+3])
  y<-cbind(zs[,1:nsing],trplau)
  return(y)
}