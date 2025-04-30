#' Insert bit vector and value into a node
#' 
#' @details # TO DO
#' @param  x a boolean or binary vector coerced to bit vector. 
#' @param  q a commonnality value associated to \code{x}.
#' @param node An existing tree structure
#' @return The tree augmented structure.
#' @author  Peiyuan Zhu
#' @export
#' @examples 
#' # TO DO
insertNode <- function(x, q, node, index) {
  if (is.null(node)) {
    return(createNode(x, q, index))
  }
  
  if (!all(x[0:node$depth] == node$x[0:node$depth])) {
    depth_disj <- which(as.logical(xor(x, node$x)))[1]
    x_disj <- x
    x_disj[depth_disj] <- TRUE
    x_disj[(depth_disj+1):length(x_disj)] <- FALSE
    
    is_same <- all(x==x_disj)
    node_disj <- createNode(x_disj, if (is_same) q else NULL, if (is_same) index else NULL) # create disjunction node with the same q
    
    if ((node_disj$depth < node$depth) && (node_disj$depth < (length(x_disj) - 1))) {
      if ((x[node_disj$depth + 1] == TRUE)) {
        node_disj$left <- node
      } else {
        node_disj$right <- node
      }
      if(!is_same) node_disj <- insertNode(x, q, node_disj, index)
      return(node_disj)
    }
  }
  
  if (all(x==node$x)) {
    disj <- createNode(x,q,index)
    node$q <- disj$q # update q if there's already a disjunction node
    node$index <- disj$index
    node$depth <- disj$depth
    return(node)
  }
  
  if (x[node$depth+1] == node$x[node$depth+1]) {
    node$right <- insertNode(x, q, node$right, index)
  } else {
    node$left <- insertNode(x, q, node$left, index)
  }
  return(node)
}