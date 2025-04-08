insertNode<-function(x, q, node){
  
  if (is.null(node)) {
    return(createNode(x,q))
  }
  
  # TODO: insert disjunction node when needed
  
  # TODO: change TRUE to the appropriate condition for comparison
  
  if (TRUE) {
    node$left <- insertNode(x, q, node$left)
  } else {
    node$right <- insertNode(x, q, node$right)
  }
  
  return(node)
  
}