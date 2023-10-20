#' Construct tt matrix of a bca from marginal probabilities
#' 
#' @param marg_probs marginal probabilities
#' @param from_above=TRUE whether to cut marginal probabilities from above
#' @param min_prob=0 lower bound on marginal probabilities
#' @param max_prob=2 upper bound on marginal probabilities
#' @param simple=TRUE whether to use simple support function
#' @return matrix of possible subsets obtained from the cuts
#' @author Peiyuan Zhu
#' @export
#' @examples
#' p <- c(2,2,1.5,1.2,1,0,0)
#' ttmatrixFromMarginal(p, FALSE)
ttmatrixFromMarginal <- function(marg_probs, min_prob=0, max_prob=2, from_above=TRUE, simple=TRUE) {
  return(do.call("rbind", lapply(if(simple) {c((max_prob-min_prob)/2, if(from_above) {max_prob} else {min_prob})} else {sort(unique(marg_probs))}, function(c) as.integer(if(from_above) marg_probs <= c else marg_probs >= c)))) 
}
