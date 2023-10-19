#' Construct m vector of a bca from marginal probabilities
#' 
#' @param marg_probs vector of marginal probabilities
#' @param a=1e-5 probability that the sample is reliable
#' @return vector of probability masses obtained from uniformly sampling the cut
#' @author Peiyuan Zhu
#' @export
#' @examples
#' p <- c(2,2,1.5,1.2,1,0,0)
#' mFromMarginal(p, FALSE)
mFromMarginal <- function(marg_probs, a=1e-5, min_prob=0, max_prob=2) {
  x <- a * diff(sort(unique(marg_probs))) / (max_prob - min_prob)
  return(c(x, 1 - sum(x))) 
}
