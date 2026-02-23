#' Calculate degree of unambiguous support (p), unambiguous contradiction (q), ambiguity (r), plausibility, and plausibility ratio
#' @aliases belplauH
#' @param MACC Vector of masses e.g. x$spec[,2]
#' @param W2 Description matrix e.g. x$tt
#' @param h Hypotheses to be tested, same format as x$tt
#' @return A matrix of \code{M} rows by 5 columns is returned, where \code{M} is the number of hypothesis tested: \itemize{
#'  \item Column 1: the degree of unambiguous support \code{p};
#'  \item Column 2: the degree of unambiguous contradiction \code{q};
#'  \item Column 3: the degree of ambiguity \code{r};
#'  \item Column 4: the degree of Plausibility \code{plau};
#'  \item Column 5: the Plausibility ratio \code{rplau}.
#'    }
#' @author Peiyuan Zhu
#' @import progress
#' @export
#' @examples 
#' x <- DSM(tt = matrix(c(1,1,0,1,1,1), nrow = 2, byrow = TRUE), m = c(0.8, 0.2), cnames = c(1,2,3))
#' DSAh(MACC = x$spec[,2], W2 = x$tt, h = x$tt)
#' hyp <- matrix(c(0,1,0,0,1,1), nrow = 2, byrow = TRUE)
#' rownames(hyp) <- nameRows(hyp)
#' DSAh(MACC = x$spec[,2], W2 = x$tt, h = hyp)
DSAh <-function(MACC, W2, h) {
  .Deprecated("DSMprint", msg = "DSAh is the new function name for the belplauH function.", old = "belplauH")
  p <- rep(0,nrow(h))
  q <- rep(0,nrow(h))
  pb <- progress_bar$new(
    format = "  computing belplau [:bar] :percent eta: :eta",
    total = nrow(W2), clear = FALSE, width= 100)
  for (i in 1:nrow(W2)) {
    pb$tick()
    for (j in 1:nrow(h)) {
      # if bpa$tt[i,] is contained in h[j,]
      if (all(h[j,]-W2[i,] >= 0)) {
        p[j] <- p[j] + MACC[i]
      }
      # if bpa$tt[i,] is contained in the complement of h[j,]
      if (all(+(!h[j,])-W2[i,] >= 0)) {
        q[j] <- q[j] + MACC[i]
      }
    }
  }
  plau <- 1 - q
  rplau <- plau / (1 - p)
  r <- plau - p
  z <- cbind(p,q,r,plau,rplau)
  rownames(z) <- rownames(h)
  return(z)
}
#' @rdname DSAh
#' @export
belplauH <- DSAh