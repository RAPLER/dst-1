#' Compute belief, disbelief, unknown, plausibility, plausibility ratio based on commonality function
#' 
#' @param qq Commonality function
#' @param h = NULL Hypothesis to be evaluated
#' @return z A matrix of \code{M} rows by 5 columns is returned, where \code{M} is the number of hypothesis tested: \itemize{
#'  \item Column 1: the degree of Belief \code{bel};
#'  \item Column 2: the degree of Disbellief (belief in favor of the contrary hypothesis) \code{disbel};
#'  \item Column 3: the degree of Epistemic uncertainty \code{unc};
#'  \item Column 4: the degree of Plausibility \code{plau};
#'  \item Column 5: the Plausibility ratio \code{rplau}.
#'    }
#' @author Peiyuan Zhu
#' @export
#' @examples 
#' x <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, byrow = TRUE),
#' m = c(0.2,0.5, 0.3), cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
#' qq <- commonality(x$tt,x$spec[,2])
#' belplauHQQ(qq,h=matrix(c(0,1,0), nrow=1, byrow=TRUE))
belplauHQQ<-function(qq, h=NULL){
  # check h
  if(is.null(h)) {
    stop("Must input h")
  }
  plau <- rep(0, nrow(h))
  one_minus_bel <- rep(0, nrow(h))
  i <- 0
  while (i <= (2**length(h) - 1)) {
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
    i <- i + 1
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