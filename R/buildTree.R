buildTree<-function(tt, q){
  tree <- NULL
  
  for (i in 1:nrow(tt)) {
    tree <- insertNode(tt[i,],q[i],tree)
  }
  
  return(tree)
}