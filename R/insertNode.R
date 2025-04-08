insertNode<-function(x, q, node){
  
  if (is.null(node)) {
    return(createNode(x,q))
  }
  
  # TODO: insert an disjunction node when needed
  # - If the new bit vector to be inserted does not equals the current bit vector up to the current depth
  # insert an disjunction node. Then insert all children of that node and the node to be inserted to the disjunction node.
  # - Otherwise there's no need to insert any disjunction node
  # - The disjunction node has XOR bit vector and NA value
  
  if (TRUE) {
    
  }
  
  # TODO: change TRUE to the appropriate condition for comparison
  # - If the new bit vector to be inserted doesn't equal the current bit vector up to the current depth plus one
  # then insert the new bit vector to the left of the current bit vector
  # - Otherwise insert the new bit vector to the right of the current bit vector
  
  if (TRUE) {
    node$left <- insertNode(x, q, node$left)
  } else {
    node$right <- insertNode(x, q, node$right)
  }
  
  return(node)
  
}