#' Adding small probabilities
#' 
#' @param l1 log probabilities
#' @param l2 log probabilities
#' @return sum of probabilities exp(l1)+exp(l2)
#' @author Peiyuan Zhu
#' @export
#' @examples exp(logsum(log(1e-5),log(1e-5)))
#' @references Ben, (2022). Adding very small probabilities—How to compute? Cross Validated.
logsum <- function(l1, l2) { 
  if (l1 == -Inf & l2 == -Inf) {
    return(-Inf)
  } else {
    return(max(l1, l2) + log1p(exp(-abs(l1-l2))) )
  }
}
