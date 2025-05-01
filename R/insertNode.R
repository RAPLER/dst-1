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
  
  if (node1$depth <= node2$depth && node1$depth < (length(node1$x) - 1)) {
    
    if (all(node1$x==node2$x)) {
      node2$q <- node1$q
      node2$index <- node1$index
      
      return(node2)
    } else if (!all(node1$x[0:node2$depth] == node2$x[0:node2$depth])) {
      
      depth_disj <- which(as.logical(xor(node1$x, node2$x)))[1]
      x_disj <- node1$x
      x_disj[depth_disj] <- TRUE
      x_disj[(depth_disj+1):length(x_disj)] <- FALSE
      
      is_equal <- all(node1$x==x_disj)
      node_disj <- if(is_equal) node1 else createNode(x_disj, NULL, NULL) 
      
      if (node1$x[node_disj$depth + 1] == TRUE) {
        node_disj$left <- node2
        if(!is_equal) node_disj$right <- node1
      } else {
        if(!is_equal) node_disj$left <- node1
        node_disj$right <- node2
      }
      
      #print("insert disjunction node")
      
      return(node_disj)
      
    } else {
      
      node1$right <- insertNode(node2, node1$right)
        
      #print("insert node itself")
      
      return(node1)
    }
  }
  
  if (node1$x[node2$depth+1] == TRUE) {
    
    print("R")
    node2$right <- insertNode(node1, node2$right)
    
  } else {
    
    print("L")
    node2$left <- insertNode(node1, node2$left)
    
  }
  return(node2)
}
