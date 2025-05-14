#' Insert bit vector and value into a node
#' 
#' @details # TO DO
#' @param  x a boolean or binary vector coerced to bit vector. 
#' @param  q a commonnality value associated to \code{x}.
#' @param node2 An existing tree structure
#' @return The tree augmented structure.
#' @author  Peiyuan Zhu
#' @export
#' @examples 
#' # TO DO
insertNode <- function(node1, node2) {
  if (is.null(node2)) {
    return(node1)
  }
  
  if (!all(node1$x[0:node2$depth] == node2$x[0:node2$depth])) {
    depth_disj <- which(as.logical(xor(node1$x, node2$x)))[1]
    x_disj <- node1$x
    x_disj[depth_disj] <- TRUE
    x_disj[(depth_disj+1):length(x_disj)] <- FALSE
    
    is_same <- all(node1$x==x_disj)
    node_disj <- createNode(x_disj, if (is_same) node1$q else NULL, if (is_same) node1$index else NULL) # create disjunction node with the same q
    
    if ((node_disj$depth < node2$depth) && (node_disj$depth < (length(x_disj) - 1))) {
      if ((node1$x[node_disj$depth + 1] == TRUE)) {
        node_disj$left <- node2
        node_disj$right <- node1
      } else {
        node_disj$left <- node1
        node_disj$right <- node2
      }
      return(node_disj)
    }
  }
  
  if (all(node1$x==node2$x)) {
    if (!(node1$index %in% node2$index)) {
      node2$index <- c(node2$index, node1$index)
    }
    return(node2)
  }
  
  if (node1$x[node2$depth+1] == node2$x[node2$depth+1]) {
    node2$right <- insertNode(node1, node2$right)
  } else {
    node2$left <- insertNode(node1, node2$left)
  }
  return(node2)
}
