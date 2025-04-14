#' Change depth to appropriate calculation
#' 
#' @details Depth equals the greatest index corresponding to 1 in the bit vector \code{x}.
#' @param  x A boolean or binary vector coerced to bit vector. 
#' @param  q A commonnality value associated to \code{x}.
#' @return A list of length 5 containing the following components:  \itemize{
#' \item x The binary table.
#' \item q The commonalities.
#' \item left The left side of the tree.
#' \item right The right side of the tree.
#' \item depth The depth of the tree..
#' }
#' @author  Peiyuan Zhu
#' @export
#' @examples 
#' # TO DO
#' 
createNode<-function(x, q) {
  d <- if(any(x)) max(which(x==1)) - 1 else -1
  list(
    x = x,
    q = q,
    left = NULL,
    right = NULL,
    depth = d
  )
}