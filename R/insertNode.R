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
insertNode <- function(x, q, node){
  # Functions calls: createNode
  # Recursive
  if (is.null(node)) {
    return(createNode(x,q))
  }
  
  if (!all(x[0:node$depth]==node$x[0:node$depth])) {
    # Insert an disjunction node when needed
    
    # - If the new bit vector to be inserted does not equals the current bit vector up to the current depth
    # insert an disjunction node. Then insert all children of that node and the node to be inserted to the disjunction node.
    # - Otherwise there's no need to insert any disjunction node
    # - The disjunction node has XOR bit vector and NULL value
    
    depth_disj <- which(as.logical(xor(x,node$x)))[1]
    x_disj <- x
    x_disj[depth_disj] <- TRUE
    x_disj[(depth_disj+1):length(x_disj)] <- FALSE
    
    node_disj <- createNode(x_disj,NULL)

    if ((x[node_disj$depth+1]==TRUE) && 
        (node_disj$depth < node$depth) && 
        (node_disj$depth < (length(x_disj) - 1))) {
      # Decide left or right child
      
      # - If the new bit vector to be inserted doesn't equal the current bit vector up to the current depth plus one
      # and that the depth of the new bit vector has depth less than the current depth
      # then insert the new bit vector to the left of the current bit vector
      # - Otherwise insert the new bit vector to the right of the current bit vector
      
      #node_disj$left <- insertNode(node$x, node$q, node_disj$left)
      node_disj$left <- node
      node_disj$right <- insertNode(x, q, node_disj$right)
      
    } else {
      
      node_disj$left <- insertNode(x, q, node_disj$left)
      node_disj$right <- node
      #node_disj$right <- insertNode(node$x, node$q, node_disj$right)
      
    }
    
    return(node_disj)
    
  } else if (x[node$depth+1]==node$x[node$depth+1]) {
    # Decide left or right child
    
    # - If the new bit vector to be inserted doesn't equal the current bit vector up to the current depth plus one
    # then insert the new bit vector to the left of the current bit vector
    # - Otherwise insert the new bit vector to the right of the current bit vector
    
    node$right <- insertNode(x, q, node$right)
    
  } else {
    
    node$left <- insertNode(x, q, node$left)
    
  }
  
  return(node)
  
}