#' Compute belief, disbelief, unknown, plausibility, plausibility ratio based on commonality function
#' @details
#' @param
#' @return
#' @author
#' @import 
#' @importClassesFrom 
#' @export
#' @examples 
#' 
belplauHQQ<-function(qq, h){
  plau <- rep(0, nrow(h))
  one_minus_bel <- rep(0, nrow(h))
  for (i in 0:(2**length(h) - 1)) {
    x <- encode(rep(2, length(h)), i)
    for (j in 1:nrow(h)) {
      # if x is contained in h[j,]
      if (all(h[j,] - x >= 0) && sum(x) > 0) {
        plau[j] <- plau[j] + (-1) ** (sum(x) + 1) * qq(x)
      }
      # if x is contained in the complement of h[j,]
      if (all(+(!x) - h[j,] >= 0) && sum(x) > 0) {
        one_minus_bel[j] <- one_minus_bel[j] + (-1) ** (sum(x) + 1) * qq(x)
      }
    }
  }
  bel <- 1 - one_minus_bel
  disbel <- 1 - plau
  plau <- 1 - disbel
  rplau <- plau / one_minus_bel
  unc <- plau - bel
  z <- cbind(bel,disbel,unc,plau,rplau)
  rownames(z) <- rownames(h)
  return(z)
}