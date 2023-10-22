#' Construct m vector of a bca from marginal probabilities
#' 
#' @param marg_probs vector of marginal probabilities
#' @param from_above=TRUE whether to cut marginal probabilities from above
#' @param a=1e-5 probability that the sample is reliable
#' @param simple=TRUE whether to use simple support function
#' @param min_prob=0 lower bound on marginal probabilities
#' @param max_prob=2 upper bound on marginal probabilities
#' @return vector of probability masses obtained from uniformly sampling the cut
#' @author Peiyuan Zhu
#' @export
#' @examples
#' x <- c(2,2,1.5,1.2,1,0,0)
#' mFromMarginal(x, FALSE)
mFromMarginal <- function(marg_probs, a=1e-5, simple=FALSE, min_prob=0, max_prob=2) {
  x <- a * (if(simple) 1 else diff(sort(unique(marg_probs))) / (max_prob - min_prob))
  return(c(x, 1 - sum(x))) 
}
