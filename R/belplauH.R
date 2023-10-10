#' Calculate belief, disbelief, unkown, plausibility, plausibility ratio
#' @param MACC: vector of masses e.g. x$spec[,2]
#' @param W2: description matrix e.g. x$tt
#' @param h: hypotheses to be tested, same format as MACC
#' @author Peiyuan Zhu
#' @export
#' @examples 
#' x <- bca(matrix(c(1,1,0,1,1,1), nrow = 2, byrow = TRUE), c(0.8, 0.2), c(1,2,3))
#' belplauTestH(x,matrix(c(1,1,0,1,1,1), nrow=2, byrow = TRUE))
belplauH <-function(MACC, W2, h) {
  bel <- rep(0,nrow(h))
  disbel <- rep(0,nrow(h))
  for (i in 1:nrow(W2)) {
    # print(i)
    for (j in 1:nrow(h)) {
      # if bpa$tt[i,] is contained in h[j,]
      if (all(h[j,]-W2[i,] >= 0)) {
        bel[j] <- bel[j] + MACC[i]
      }
      # if complement of bpa$tt[i,] is contained in h[j,]
      if (all(+(!h[j,])-W2[i,] >= 0)) {
        disbel[j] <- disbel[j] + MACC[i]
      }
    }
    # if (i == 100) break
  }
  plau <- 1 - disbel
  rplau <- plau / (1 - bel)
  unkown <- plau - bel
  return(cbind(bel,disbel,unkown,plau,rplau))
}
