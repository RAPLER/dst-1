#' Construct tt matrix of a bca from marginal probabilities
#' 
#' @param marg_probs marginal probabilities
#' @param from_above=TRUE whether to cut marginal probabilities from above
#' @param simple=TRUE whether to use simple support function
#' @param min_prob=0 lower bound on marginal probabilities
#' @param max_prob=2 upper bound on marginal probabilities
#' @return matrix of possible subsets obtained from the cuts
#' @author Peiyuan Zhu
#' @export
#' @examples
#' x <- c(2,2,1.5,1.2,1,0,0)
#' ttmatrixFromMarginal(x, FALSE)
ttmatrixFromMarginal <- function(marg_probs, from_above=FALSE, simple=FALSE, min_prob=0, max_prob=2) {
  x <- if(from_above) {2 - marg_probs} else {marg_probs}
  return(do.call("rbind", lapply(if(simple) {c((max_prob - min_prob) / 2, min_prob)} else {sort(unique(x), decreasing = TRUE)}, function(c) as.integer(x >= c)))) 
}
