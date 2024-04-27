#' Calculate belief, disbelief, unknown, plausibility, plausibility ratio with logsumexp
#' @param MACC Vector of masses e.g. x$spec[,2]
#' @param W2 Description matrix e.g. x$tt
#' @param h Hypotheses to be tested, same format as x$tt
#' @return A matrix of \code{M} rows by 5 columns is returned, where \code{M} is the number of hypothesis tested: \itemize{
#'  \item Column 1: the degree of Belief \code{bel};
#'  \item Column 2: the degree of Disbellief (belief in favor of the contrary hypothesis) \code{disbel};
#'  \item Column 3: the degree of Epistemic uncertainty \code{unc};
#'  \item Column 4: the degree of Plausibility \code{plau};
#'  \item Column 5: the Plausibility ratio \code{rplau}.
#'    }
#' @author Peiyuan Zhu
#' @export
#' @examples 
#' x <- bca(tt = matrix(c(1,1,0,1,1,1), nrow = 2, byrow = TRUE), m = c(0.8, 0.2), cnames = c(1,2,3))
#' belplauH(MACC = x$spec[,2], W2 = x$tt, h = x$tt)
#' hyp <- matrix(c(0,1,0, 0,1,1), nrow = 2, byrow = TRUE)
#' rownames(hyp) <- nameRows(hyp)
#' belplauH(MACC = x$spec[,2], W2 = x$tt, h = hyp)
belplauHLogsumexp <-function(MACC, W2, h) {
  log_bel <- rep(-Inf,nrow(h))
  log_disbel <- rep(-Inf,nrow(h))
  for (i in 1:nrow(W2)) {
    # print(i)
    for (j in 1:nrow(h)) {
      # if bpa$tt[i,] is contained in h[j,]
      if (all(h[j,]-W2[i,] >= 0)) {
        log_bel[j] <- logsum(log_bel[j], log(MACC[i]))
      }
      # if complement of bpa$tt[i,] is contained in h[j,]
      if (all(+(!h[j,])-W2[i,] >= 0)) {
        log_disbel[j] <- logsum(log_disbel[j], log(MACC[i]))
      }
    }
    # if (i == 100) break
  }
  bel <- exp(log_bel)
  disbel <- exp(log_disbel)
  plau <- exp(log1p(- disbel))
  rplau <- exp(log(plau) - log1p( - bel))
  unc <- plau - bel
  z <- cbind(bel,disbel,unc,plau,rplau)
  rownames(z) <- rownames(h)
  return(z)
}