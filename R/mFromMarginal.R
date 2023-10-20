#' Construct m vector of a bca from marginal probabilities
#' 
#' @param marg_probs vector of marginal probabilities
#' @param a=1e-5 probability that the sample is reliable
#' @param min_prob=0 lower bound on marginal probabilities
#' @param max_prob=2 upper bound on marginal probabilities
#' @param simple=TRUE whether to use simple support function
#' @return vector of probability masses obtained from uniformly sampling the cut
#' @author Peiyuan Zhu
#' @export
#' @examples
#' p <- c(2,2,1.5,1.2,1,0,0)
#' mFromMarginal(p, FALSE)
mFromMarginal <- function(marg_probs, a=1e-5, min_prob=0, max_prob=2, simple=TRUE) {
  x <- a * ifelse(simple, 1, diff(sort(unique(marg_probs))) / (max_prob - min_prob))
  return(c(x, 1 - sum(x))) 
}
