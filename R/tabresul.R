#' Prepare a table of results
#'
#' This utility function gather the bca representation of a belief function and the measures of belief and plausibility in one table.
#' @aliases tabresul
#' @param x A belief function in its bca form, generally the normalized result of the combination of two or more belief functions (see \code{\link{nzdsr}}.
#' @param removeZeroes = TRUE removes focal elements with 0 mass.
#' @param singletonsOnly = TRUE reduces the table of results to elementary events.
#' @return A list of three elements: \itemize{
#'   \item $mbp: The table of focal elements with the addition of the their associated mass, degree of belief, plausibility and the plausibility ratio
#'   \item $con The measure of conflict between focal elements.
#'   \item $n The node number.
#'   }
#' @author Claude Boivin, Stat.ASSQ
#' @examples  
#' x <- bca(f=matrix(c(0,1,1,1,1,0,1,1,1),nrow=3, byrow = TRUE), m=c(0.2,0.5, 0.3), cnames =c("a", "b", "c"), n=1)
#' y <- bca(f=matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), m=c(0.6, 0.4),  cnames = c("a", "b", "c"), n=1)
#' xy <- dsrwon(x,y)
#' tabresul(nzdsr(xy)) 
#' print("Remove focal elements with 0 mass")
#' tabresul(nzdsr(xy), removeZeroes = TRUE)
#' tabresul(nzdsr(xy), singletonsOnly = TRUE)
#' print("Add the two missing singletons")
#' xy1 <- addTobca(nzdsr(xy), matrix(c(0,1,0,0,0,1), nrow=2, byrow = TRUE))
#' tabresul(xy1, singletonsOnly = TRUE)
#' @export
#' 
tabresul<-function(x, singletonsOnly = FALSE, removeZeroes = FALSE) {  
# Compute Bel and Pl functions 
  BP<-belplau(x)
# prepare final result
  macc<-t(rbind(x$combination[,1]))
  W2<-rbind(x$combination[,-1])
# remove elements with mass=0, but the frame
  INUL<-c(macc[-length(macc)]>0,TRUE)
  if (removeZeroes == TRUE) {
    macc1<-t(rbind(macc[INUL]))
    W2a <- rbind(W2[INUL,])
    BP <- rbind(BP[INUL,])
  } else {
    macc1<-macc
    W2a<-W2
  }
  colnames(macc1)<-"mass"
  mbp<-cbind(W2a,macc1,BP)
# Prepare a table of results reduced to the singletons
  if (singletonsOnly == TRUE) {
    r <- mbp
    z2<-r[,c(1:(ncol(r)-4))]
    if (!is.null(dim(z2))) {
      r1<-r[apply(z2,1,sum)==1, , drop = FALSE]
    } else {
      z2<-rbind(r[,c(1:(ncol(x$combination)-1))])
      r1<-r[apply(z2,1,sum)==1,]
      if (is.null(dim(r1))) {
        r1<-t(as.matrix(r1))
      }
    }
    mbp <- r1
  }
  resul<-list(mbp=mbp, Conflict=x$con, n=x$n)
  return(resul)
}
 