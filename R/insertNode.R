insertNode<-function(x, q, node, depth = 0){
  
  if (is.null(node)) {
    return(createNode(x,q,depth))
  }
  
  # TODO: insert disjunction node when needed
  
  # TODO: change TRUE to the appropriate condition for comparison
  if (TRUE) {
    node$left <- insertNode(x, q, node$left, depth + 1)
  } else {
    node$right <- insertNode(x, q, node$right, depth + 1)
  }
  
  return(node)
  
}