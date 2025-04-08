#' Insert bit vector and value into a node
#' 
insertNode <- function(x, q, node){
  
  if (is.null(node)) {
    return(createNode(x,q))
  }
  
  if (!all(x[0:node$depth]==node$x[0:node$depth])) {
    # insert an disjunction node when needed
    
    # - If the new bit vector to be inserted does not equals the current bit vector up to the current depth
    # insert an disjunction node. Then insert all children of that node and the node to be inserted to the disjunction node.
    # - Otherwise there's no need to insert any disjunction node
    # - The disjunction node has XOR bit vector and NULL value
    
    z <- xor(x,node$x)
    
    node_disj <- createNode(z,NULL)
    
    if (as.logical(x[node_disj$depth+1]==z[node_disj$depth+1])) {
      # decide left or right child
      
      # - If the new bit vector to be inserted doesn't equal the current bit vector up to the current depth plus one
      # then insert the new bit vector to the left of the current bit vector
      # - Otherwise insert the new bit vector to the right of the current bit vector
      
      node_disj$left <- insertNode(node$x, node$q, node_disj$left)
      node_disj$right <- insertNode(x, q, node_disj$right)
      
    } else {
      
      node_disj$left <- insertNode(x, q, node_disj$left)
      node_disj$right <- insertNode(node$x, node$q, node_disj$right)
      
    }
    
    return(node_disj)
    
  } else if (as.logical(x[node$depth+1]==node$x[node$depth+1])) {
    # decide left or right child
    
    # - If the new bit vector to be inserted doesn't equal the current bit vector up to the current depth plus one
    # then insert the new bit vector to the left of the current bit vector
    # - Otherwise insert the new bit vector to the right of the current bit vector
    
    node$right <- insertNode(x, q, node$right)
    
  } else {
    
    node$left <- insertNode(x, q, node$left)
    
  }
  
  return(node)
  
}