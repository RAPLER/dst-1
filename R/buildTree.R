#' Build a tree 
#' 
#' Example: Figure 12
buildTree<-function(tt, q){
  tree <- NULL
  
  n <- if (is.null(nrow(tt))) 1 else nrow(tt)
  
  for (i in 1:n) {
    
    row <- if (is.null(nrow(tt))) tt else tt[i, ]
    
    tree <- insertNode(as.bit(row),q[i],tree)
    
  }
  
  return(tree)
}