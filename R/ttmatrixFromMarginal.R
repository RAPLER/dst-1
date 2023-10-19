#' Construct tt matrix of a bca from marginal probabilities
#' 
#' @param marg_probs marginal probabilities
#' @param from_above whether to cut marginal probabilities from above
#' @return matrix of possible subsets obtained from the cuts
#' @author Peiyuan Zhu
#' @export
#' @examples
#' p <- c(2,2,1.5,1.2,1,0,0)
#' ttmatrixFromMarginal(p, FALSE)
ttmatrixFromMarginal <- function(marg_probs, from_above=TRUE) {
  return(do.call("rbind", lapply(sort(unique(marg_probs)), function(c) as.integer(if(from_above) marg_probs <= c else marg_probs >= c)))) 
}
